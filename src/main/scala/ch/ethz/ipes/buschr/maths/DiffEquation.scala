package ch.ethz.ipes.buschr.maths

import JampackNew._

import scala.util.control.Breaks._

/** Five step first order differential equation solver.
  *
  * Solves a first order differential system, either as a system matrix or by using the factory method as a matrix of
  * coefficients of the equation solved to y^n^ = Ay^n-1^ + ... Dy, which becomes [A B C ... D].
  * The process is as follows:
  *
  * $ 1. Solve homogeneous first order problem given by y' = `_matrix` * y. This results in the object
  * `_eigenvalueDecomposition`, which contains the eigenvalues in D and the respective eigenvectors in X. Eigenvalues
  * and eigenvectors can be obtained using `eigenValues` and `eigenVectors` respectively. This is done automatically at
  * object creation.
  *
  * $ 2. Find generalized eigenvectors for base of y. This populates `_generalEigenVectors`, which is obtainable using
  * `generalEigenVectors`. This is done automatically at object creation.
  *
  * $ 3. (Optional) Add a non-homogeneous term to the right side, i.e. y' = Ay' + g. g can be either a constant, a
  * polynomial term or an exponential term. A mixture thereof is not supported. This is done by request using
  * `applyInhomogeneity`.
  *
  * $ 4. Add a data point to fix constants. This populates `_constants`, which is not directly obtainable. This step is
  * invoked using `applyDataPoint` or `applyStartingConditions`, which is called if the data point is at `t = 0`.
  *
  * $ 5. Evaluate the solution for either a single line in the solution vector or the whole vector. As Scala is
  * functional, this method can be used as a function handle elsewhere; however, if the matrix or data point are changed
  * the solution will change while the function handle stays valid.
  *
  * @param _matrix Matrix A in y'= Ax
  */
class DiffEquation(private var _matrix: Zmat) {
	require(_matrix.nrow == _matrix.ncol && _matrix.rank == _matrix.nrow, "Matrix must be full rank.")

	//step 0: initialisation. only have the matrix so far, so inhomogeneity is zero. also no data point yet to fix constants.
	private var _appliedDataPoint = false

	private var _typeOfInhomogeneity = DiffEquation.Inhomogeneity.Zero
	/** Matrix with particular solution, matching above type.
	  * - Zero is just zero or null
	  * - Constant is a vector of constants
	  * - Exponential is a matrix which will be multiplied by a vector of exp(x*t) in the solution
	  * - Polynomial is a maatrix that will be multiplied by a vector [1 t t^2^ ...]' in the solution
	  */
	private var _particularSolution: Zmat = new Zmat(_matrix.nrow, 1)
	private var _inhomogeneityExponents: Zmat = _
	private var _constants = Array.ofDim[Z](_matrix.nrow)

	// step 1: get eigenvalues and eigenvectors of matrix
	private var _eigenvalueDecomposition = new Eig(_matrix)

	// step 2: find generalized eigenvectors of matrix
	private var _generalEigenVectors = eigenVectors
	private var _seenEigs: Map[Z, Int] = Map()
	private var _usedSVD: Map[Z, JamaSVD] = Map()
	// iterate through all eigenvalues
	for (j <- _eigenvalueDecomposition.D.re.indices) {
		// eigenvalues are stored as real and complex part. combine for easier comparison.
		val eig = Z(_eigenvalueDecomposition.D.re(j), _eigenvalueDecomposition.D.im(j))
		// if the eigenvalue has appeared previously
		if (_seenEigs.contains(eig.round(5))) {
			// look for the appropriate eigenvector
			var foundBefore = false
			breakable {
				val v = _generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j, j).getZ.flatten
				for (k <- j - 1 to 0 by -1) {
					if (DiffEquation.sameEigenvectors(v,
						_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, k, k).getZ.flatten,
						5)) {
						foundBefore = true
						break
					}
				}
			}
			if (foundBefore) {
				// i've treated the eigenvalue with this vector before, so the most recent generalized eigenvector is in the column before
				var v = _generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j - 1, j - 1)
				// if i've treated a duplicate of this eigenvalue before, use already calculated SVD. otherwise create a new one
				val svd = if (_seenEigs(eig.round(5)) > 1) {
					_usedSVD(eig.round(5))
				}
				else {
					val t = new JamaSVD(_matrix - Eye.o(_matrix.nrow) * eig)
					_usedSVD += (eig.round(5) -> t)
					t
				}
				// create new generalized eigenvector using (A - eig*I)v2 = v1, resp. v2 = pinv(A - eig*I) * v1
				// pinv(X) = V * pinv(S) * U', and pinv(S) is the inverse of each non-zero element (non-square matrices not treated)
				v = svd.V * svd.S.pinv * svd.U.transpose.conj * v
				// add new generalized vector into matrix
				_generalEigenVectors.put(0, _generalEigenVectors.nrow - 1, j, j, v)
			}
			// else i haven't encountered this vector before, so it's a unique vector

			// increment count of eigenvector
			_seenEigs += (eig.round(5) -> (_seenEigs(eig.round(5)) + 1))
		}
		else {
			// new eigenvector, don't need to do anything
			_seenEigs += (eig.round(5) -> 1)
		}
	}

	// step 3: apply Inhomogeneity
	/** Adds non-homogeneous term to the equation.
	  *
	  * The additional term can be changed to zero, a constant, or an exponential function of t. Polynomial terms and
	  * mixed expressions are not supported.
	  *
	  * @param coefficients Complex array of coefficients for vector of either [1 t t^2^ ...]' for polynomials (in which
	  *                     case exponents is expected to be ''null'') or [exp(v1*t) exp(v2*t) ...]' for exponential
	  *                     inhomogeneities. The order of v1, v2 etc. is determined by the vector ''exponents''.
	  * @param exponents    Exponents for exponential inhomogeneity. Otherwise expected to be ''null''.
	  */
	def applyInhomogeneity(coefficients: Zmat, exponents: Zmat): Unit = {
		if (exponents == null) {
			if (coefficients == null) {
				// default case, but possible to solve y' = Ay + 0 after having solved y' = Ay + B
				_typeOfInhomogeneity = DiffEquation.Inhomogeneity.Zero
				_inhomogeneityExponents = null
				_particularSolution = null
			}
			else {
				require(coefficients.nrow == _matrix.nrow, "Inhomogeneity height must agree with matrix height.")
				if (coefficients.ncol == 1) {
					// guess for x' = Ax + U for constant U is x = const, with x' = 0. Therefore, solve 0 = Ax + U, which is
					// x_particular_ = -pinv(A) * U
					_typeOfInhomogeneity = DiffEquation.Inhomogeneity.Constant
					_inhomogeneityExponents = null
					_particularSolution = -_matrix.solve(coefficients)
				}
				else {
					_typeOfInhomogeneity = DiffEquation.Inhomogeneity.Polynomial
					_inhomogeneityExponents = null

					// x' = Ax + U for U = coeffs * [1 t t² t³ ...]' requires the guess x = X * [1 t t² t³ ...]', so
					// x' = X * diag([1 2 3 ...], -1) * [1 t t² t³ ...]' = A * X * [1 t t² t³ ...]' + coefficients * [1 t t² t³ ...]',
					// which allows for simplification to X * diag([1 2 3 ...], -1) = A * X + coefficients, which in turn
					// leads to the sylvester equation A * X - X * diag([1 2 3 ...], -1) = -coefficients
					val diffMatrix = new Zmat(coefficients.ncol, coefficients.ncol)
					for (i <- 1 until coefficients.ncol) {
						diffMatrix(i, i - 1) = -i
					}
					_particularSolution = DiffEquation.solveSylvesterEquation(_matrix, diffMatrix, -coefficients)
				}
			}
		}
		else {
			require(exponents.ncol == 1, "Exponents must be a single vector.")
			if (coefficients == null) {
				throw new Exception("Coefficients can only be null for exponents of null.")
			}
			else {
				require(coefficients.nrow == _matrix.nrow, "Inhomogeneity height must agree with matrix height.")
				require(coefficients.ncol == exponents.nrow, "Coefficients and exponents dimensions must agree.")

				_typeOfInhomogeneity = DiffEquation.Inhomogeneity.Exponential
				_inhomogeneityExponents = exponents.clone()

				// x' = Ax + U for U = coefficients * exp(exponents * t) requires the guess x = X * exp(exponents * t),
				// so x' = X * diag(exponents) * exp(exponents * t) = A * X * exp(exponents * t) + coefficients * exp(exponents * t),
				// which allows for simplification to X * diag(exponents) = A * X + coefficients, which in turn leads to
				// the sylvester equation A * X - X * diag(exponents) = -coefficients
				val diffMatrix = new Zmat(new Zdiagmat(exponents.getZ.map(-_.head.re), exponents.getZ.map(-_.head.im)))
				_particularSolution = DiffEquation.solveSylvesterEquation(_matrix, diffMatrix, -coefficients)
			}
		}
		// new inhomogeneity, so new coefficients need to be calculated.
		_appliedDataPoint = false
	}

	// step 4: apply data point to fix constants
	//TODO maybe implement a method that takes a t[] and allows different points in time for each differentiation?

	/** Apply data point with `t = 0`.
	  *
	  * @param vector Array of starting conditions for each line of `_matrix`
	  */
	def applyStartingConditions(vector: Array[Double]): Unit = {
		require(vector.length == _constants.length, s"Vector needs to be of size ${_constants.length} x 1.")

		// puts vector in a matrix to allow easier manipulation.
		val vectorMatrix = new Zmat(vector.map(Array(_)))
		_typeOfInhomogeneity match {
			case DiffEquation.Inhomogeneity.Zero =>
				_generalEigenVectors.solve(vectorMatrix)
					.get(0, vector.length - 1, 0, 0).getZ.map(_.head).copyToArray(_constants)
			case DiffEquation.Inhomogeneity.Constant =>
				// would need chains, but as t = 0 the chains are only the first element each.
				_generalEigenVectors.solve(vectorMatrix - _particularSolution)
					.get(0, vector.length - 1, 0, 0).getZ.map(_.head).copyToArray(_constants)
			case DiffEquation.Inhomogeneity.Exponential =>
				// would be _particularSolution * _inhomogeneityExponents(t), but since t = 0, that latter vector is = 1.
				// therefore, just add each row to form a column vector.
				_generalEigenVectors.solve(vectorMatrix - new Zmat(_particularSolution.getZ.map(i => Array(i.sum))))
					.get(0, vector.length - 1, 0, 0).getZ.map(_.head).copyToArray(_constants)
			case DiffEquation.Inhomogeneity.Polynomial =>
				// would be _particularSolution * [1 t t^2 ...], but since t = 0, the multiplication only returns the
				// first element of each row.
				_generalEigenVectors.solve(vectorMatrix - new Zmat(_particularSolution.getZ.map(i => Array(i.head))))
					.get(0, vector.length - 1, 0, 0).getZ.map(_.head).copyToArray(_constants)
		}
		// allow evaluation of solution
		_appliedDataPoint = true
	}

	def applyDataPoint(t: Double, vector: Array[Double]): Unit = {
		require(vector.length == _constants.length, s"Vector needs to be of size ${
			_constants.length
		} x 1.")

		if (t == 0) {
			// specialized case, as many elements either become 0 or 1.
			applyStartingConditions(vector)
			return
		}

		// calculate vectors scaled with their respective chain coefficients
		var scaledVectors = _generalEigenVectors.clone()
		// tracks the last vector in _generalEigenVectors that's equal to the eigenvectors, i.e. already unique. A new
		// unique eigenvector triggers a reset of chain length.
		var lastUnchangedEigenvector = 0
		for (j <- _generalEigenVectors.re.indices) {
			if (DiffEquation.sameEigenvectors(_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j, j).getZ.flatten,
				_eigenvalueDecomposition.X.get(0, _eigenvalueDecomposition.X.nrow - 1, j, j).getZ.flatten,
				5)) {
				lastUnchangedEigenvector = j
			}
			// collect results of each chain element
			var temp: Zmat = new Zmat(Array.fill[Z](_generalEigenVectors.nrow, 1)(0))
			// build chain for current vector
			for (k <- j to lastUnchangedEigenvector by -1) {
				temp += _generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, k, k) * math.pow(t, j - k) / (2 to j - k).product
			}
			// add scaled chain vector to matrix
			scaledVectors.put(0, scaledVectors.nrow - 1, j, j, temp)
		}

		// multiply exp(eig * t) in to the matrix using a diagonal matrix
		val temp = (_eigenvalueDecomposition.D.re, _eigenvalueDecomposition.D.im).zipped.map((i, j) => (new Z(i, j) * t).exp)
		scaledVectors *= new Zdiagmat(temp.map(_.re), temp.map(_.im))

		// put starting conditions in matrix form so it can be solved against
		var vectorMatrix = new Zmat(vector.length, 1)
		vectorMatrix.put(0, vector.length - 1, 0, 0, new Zmat(vector.map(Array(_))))

		// subtract inhomogeneity from vector to solve Ax + u = b using Ax = b - u
		_typeOfInhomogeneity match {
			case DiffEquation.Inhomogeneity.Zero => // do nothing
			case DiffEquation.Inhomogeneity.Constant =>
				vectorMatrix -= _particularSolution
			case DiffEquation.Inhomogeneity.Exponential =>
				vectorMatrix -= _particularSolution * new Zmat(_inhomogeneityExponents.getZ.map(_.map(i => (i * t).exp)))
			case DiffEquation.Inhomogeneity.Polynomial =>
				val vector = new Zmat(_particularSolution.ncol, 1)
				for (i <- 0 until vector.nrow) {
					vector(i, 0) = math.pow(t, i)
				}
				vectorMatrix -= _particularSolution * vector
		}
		scaledVectors.solve(vectorMatrix).get(0, vector.length - 1, 0, 0).getZ.map(_.head).copyToArray(_constants)

		_appliedDataPoint = true
	}

	// step 5: evaluate up solution to the problem.
	/** Evaulates a row of the solution vector at time `t`.
	  *
	  * @param t   Time point at which to evaluate the solution.
	  * @param row Row in the solution vector that's supposed to be evaluated. Default is 0, as n-order ODEs have the
	  *            solution in row 0.
	  * @return Result
	  */
	def solution(t: Double, row: Int = 0) = {
		require(row >= 0, "Can't have a negative row.")
		require(_appliedDataPoint, "No data point applied, solution not defined.")

		var result: Z = 0

		// add homogeneous solution
		var lastUnchangedEigenvector = 0
		for (j <- _generalEigenVectors.re.indices) {
			val eig = _eigenvalueDecomposition.D.get(j)
			if (DiffEquation.sameEigenvectors(_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j, j).getZ.flatten,
				_eigenvalueDecomposition.X.get(0, _eigenvalueDecomposition.X.nrow - 1, j, j).getZ.flatten,
				5)) {
				lastUnchangedEigenvector = j
			}
			var temp: Z = 0
			// build chain for current vector
			for (k <- j to lastUnchangedEigenvector by -1) {
				temp += _generalEigenVectors(row, k) * math.pow(t, j - k) / (2 to j - k).product
			}
			// multiply by constant
			result += temp * _constants(j) * (eig * t).exp
		}

		// add inhomogeneous solution
		_typeOfInhomogeneity match {
			case DiffEquation.Inhomogeneity.Zero => // do nothing
			case DiffEquation.Inhomogeneity.Constant => result += _particularSolution(row, 0)
			case DiffEquation.Inhomogeneity.Exponential =>
				result += (_particularSolution * new Zmat(_inhomogeneityExponents.getZ.map(_.map(i => (i * t).exp)))) (row, 0)
			case DiffEquation.Inhomogeneity.Polynomial =>
				val vector = new Zmat(_particularSolution.ncol, 1)
				for (i <- 0 until vector.nrow) {
					vector(i, 0) = math.pow(t, i)
				}
				result += (_particularSolution * vector) (row, 0)
		}

		result
	}

	/** Returns the whole solution vector at time `t`.
	  *
	  * @param t Point in time at which to evaluate system.
	  * @return Result
	  */
	def solutionVector(t: Double) = {
		require(_appliedDataPoint, "No data point applied, solution not defined.")

		var result = new Zmat(_generalEigenVectors.nrow, 1)

		// add homogeneous solution
		var lastUnchangedEigenvector = 0
		for (j <- _generalEigenVectors.re.indices) {
			val eig = Z(_eigenvalueDecomposition.D.re(j), _eigenvalueDecomposition.D.im(j))
			if (DiffEquation.sameEigenvectors(_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j, j).getZ.flatten,
				_eigenvalueDecomposition.X.get(0, _eigenvalueDecomposition.X.nrow - 1, j, j).getZ.flatten,
				5)) {
				lastUnchangedEigenvector = j
			}
			var temp = new Zmat(_generalEigenVectors.nrow, 1)
			// build chain for current vector
			for (k <- j to lastUnchangedEigenvector by -1) {
				temp += _generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, k, k) * math.pow(t, j - k) / (2 to j - k).product
			}
			// multiply by constant
			result += temp * _constants(j) * (eig * t).exp
		}

		// add inhomogeneous solution
		_typeOfInhomogeneity match {
			case DiffEquation.Inhomogeneity.Zero => // do nothing
			case DiffEquation.Inhomogeneity.Constant => result += _particularSolution.get(0, _generalEigenVectors.nrow - 1, 0, 0)
			case DiffEquation.Inhomogeneity.Exponential =>
				result += (_particularSolution * new Zmat(_inhomogeneityExponents.getZ.map(_.map(i => (i * t).exp))))
			case DiffEquation.Inhomogeneity.Polynomial =>
				val vector = new Zmat(_particularSolution.ncol, 1)
				for (i <- 0 until vector.nrow) {
					vector(i, 0) = math.pow(t, i)
				}
				result += _particularSolution * vector
		}

		result
	}

	/** Returns first derivative at point `t`.
	  *
	  * @param t   Point in time at which to evaluate solution row.
	  * @param row Row to evaulate. Default is zero because of n-order ODEs.
	  * @return Result
	  */
	def derivedSolution(t: Double, row: Int = 0) = {
		require(row >= 0, "Can't have a negative row.")
		require(_appliedDataPoint, "No data point applied, solution not defined.")

		var result: Z = 0

		// add homogeneous solution
		var lastUnchangedEigenvector = 0
		for (j <- _generalEigenVectors.re.indices) {
			val eig = _eigenvalueDecomposition.D.get(j)
			if (DiffEquation.sameEigenvectors(_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j, j).getZ.flatten,
				_eigenvalueDecomposition.X.get(0, _eigenvalueDecomposition.X.nrow - 1, j, j).getZ.flatten,
				5)) {
				lastUnchangedEigenvector = j
			}
			var temp: Z = 0
			// build chain for current vector
			for (k <- j to lastUnchangedEigenvector by -1) {
				temp += _generalEigenVectors(row, k) * math.pow(t, j - k) / (2 to j - k).product
			}
			temp *= eig
			for (k <- j - 1 to lastUnchangedEigenvector by -1) {
				temp += _generalEigenVectors(row, k) * math.pow(t, j - 1 - k) / (2 until j - k).product
			}
			// multiply by constant
			result += temp * _constants(j) * (eig * t).exp
		}

		// add inhomogeneous solution
		_typeOfInhomogeneity match {
			case DiffEquation.Inhomogeneity.Zero => // do nothing
			case DiffEquation.Inhomogeneity.Constant => // do nothing
			case DiffEquation.Inhomogeneity.Exponential =>
				result += (_particularSolution * new Zdiagmat(_inhomogeneityExponents.getZ.map(_.head.re), _inhomogeneityExponents.getZ.map(_.head.im))
					* new Zmat(_inhomogeneityExponents.getZ.map(_.map(i => (i * t).exp)))) (row, 0)
			case DiffEquation.Inhomogeneity.Polynomial =>
				val vector = new Zmat(_particularSolution.ncol, 1)
				val diffMatrix = new Zmat(_inhomogeneityExponents.ncol, _inhomogeneityExponents.ncol)
				for (i <- 1 until _inhomogeneityExponents.ncol) {
					diffMatrix(i, i - 1) = i
				}
				for (i <- 0 until vector.nrow) {
					vector(i, 0) = math.pow(t, i)
				}
				result += (_particularSolution * diffMatrix * vector) (row, 0)
		}

		result
	}

	/** Evaulates whole solution vector at time `t`.
	  *
	  * @param t Point in time at which to evaluate solution.
	  * @return Result
	  */
	def derivedSolutionVector(t: Double) = {
		require(_appliedDataPoint, "No data point applied, solution not defined.")

		var result = new Zmat(_generalEigenVectors.nrow, 1)

		// add homogeneous solution
		var lastUnchangedEigenvector = 0
		for (j <- _generalEigenVectors.re.indices) {
			val eig = Z(_eigenvalueDecomposition.D.re(j), _eigenvalueDecomposition.D.im(j))
			if (DiffEquation.sameEigenvectors(_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j, j).getZ.flatten,
				_eigenvalueDecomposition.X.get(0, _eigenvalueDecomposition.X.nrow - 1, j, j).getZ.flatten,
				5)) {
				lastUnchangedEigenvector = j
			}
			var temp = new Zmat(_generalEigenVectors.nrow, 1)
			// build chain for current vector
			for (k <- j to lastUnchangedEigenvector by -1) {
				temp += (_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, k, k) * math.pow(t, j - k) / (2 to j - k).product)
			}
			temp *= eig
			for (k <- j - 1 to lastUnchangedEigenvector by -1) {
				temp += (_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, k, k) * math.pow(k, j - 1 - k) / (2 until j - k).product)
			}
			// multiply by constant
			result += temp * _constants(j) * (eig * t).exp
		}

		// add inhomogeneous solution
		_typeOfInhomogeneity match {
			case DiffEquation.Inhomogeneity.Zero => // do nothing
			case DiffEquation.Inhomogeneity.Constant => // do nothing
			case DiffEquation.Inhomogeneity.Exponential =>
				result += (_particularSolution * new Zdiagmat(_inhomogeneityExponents.getZ.map(_.head.re), _inhomogeneityExponents.getZ.map(_.head.im))
					* new Zmat(_inhomogeneityExponents.getZ.map(_.map(i => (i * t).exp))))
			case DiffEquation.Inhomogeneity.Polynomial =>
				val vector = new Zmat(_particularSolution.ncol, 1)
				val diffMatrix = new Zmat(_inhomogeneityExponents.ncol, _inhomogeneityExponents.ncol)
				for (i <- 1 until _inhomogeneityExponents.ncol) {
					diffMatrix(i, i - 1) = i
				}
				for (i <- 0 until vector.nrow) {
					vector(i, 0) = math.pow(t, i)
				}
				result += _particularSolution * diffMatrix * vector
		}

		result
	}

	/** Copy of `_matrix`. Edits have no effect on this system.
	  *
	  * @return
	  */
	def matrix = _matrix.clone()

	/** Copy of the eigenvalues of `_matrix`. Edits have no effect on this system.
	  *
	  * @return
	  */
	def eigenValues = (_eigenvalueDecomposition.D.re, _eigenvalueDecomposition.D.im).zipped.map(new Z(_, _))

	/** Copy of the eigenvectors of `_matrix`. Edits have no effect on this system.
	  *
	  * @return
	  */
	def eigenVectors = _eigenvalueDecomposition.X.clone()

	/** Copy of the general eigenvectors of `_matrix`. Edits have no effect on this system.
	  *
	  * @return
	  */
	def generalEigenVectors = _generalEigenVectors.clone()

	/** Check if a data point was applied.
	  *
	  * @return
	  */
	def appliedDataPoint = _appliedDataPoint

	/** Returns a copy of this object without solving anything again.
	  *
	  * @return
	  */
	override def clone(): DiffEquation = {
		val copy = new DiffEquation(new Zmat(Array[Array[Z]](Array(1))))
		copy._matrix = _matrix.clone()
		copy._appliedDataPoint = _appliedDataPoint
		copy._typeOfInhomogeneity = _typeOfInhomogeneity
		copy._inhomogeneityExponents = _inhomogeneityExponents.clone()
		copy._particularSolution = _particularSolution.clone()
		copy._eigenvalueDecomposition = _eigenvalueDecomposition.clone()
		copy._generalEigenVectors = _generalEigenVectors.clone()
		copy._seenEigs = Map[Z, Int]() ++ _seenEigs
		copy._usedSVD = Map[Z, JamaSVD]() ++ _usedSVD
		copy._constants = _constants.clone()
		copy
	}

}

object DiffEquation {

	/**
	  * Differential equation in matrix form, including solver. A represents all coefficients of the differential equation,
	  * including zeros, starting at the highest order term (e.g. y'\'\' = y'\' + 3y' - 7y becomes {1, 3, -7}).
	  *
	  * @param coefficients The coefficients of the equation solved for the highest differential, not including inhomogeneities.
	  * @return Matrix representing the differential equation.
	  */
	def apply(coefficients: Array[Double]): DiffEquation = {
		val matrix = new Zmat(coefficients.length, coefficients.length)

		for (i <- 0 until coefficients.length - 1) {
			matrix(i, i + 1) = 1
		}
		matrix(coefficients.length - 1) = coefficients.reverse.map(new Z(_))
		new DiffEquation(matrix)
	}

	/** Checks if two (eigen)vectors are the same, i.e. checks if v1 = C * v2 holds for a given precision.
	  *
	  * @param v1        First vector
	  * @param v2        Second vector
	  * @param precision Decimal precision to use in compares
	  * @return
	  */
	private def sameEigenvectors(v1: Array[Z], v2: Array[Z], precision: Int): Boolean = {
		var v1abs = v1.map(i => i * i).sum.sqrt
		var v2abs = v2.map(i => i * i).sum.sqrt
		if (v1abs == implicitly[Z](0)) {
			v1abs = 1
		}
		if (v2abs == implicitly[Z](0)) {
			v2abs = 1
		}
		(v1.map(i => (i / v1abs).round(precision)) sameElements v2.map(i => (i / v2abs).round(precision))) ||
			(v1.map(i => (i / v1abs).round(precision)) sameElements v2.map(i => -(i / v2abs).round(precision)))
	}

	/** Solves Sylvester equation AX + XB = C by solving (I,,n,,xA + B'xI,,n,,)vec(X) = vec(C), where x is the Kronecker
	  * product, and vec(M) stacks the vectors of M.
	  *
	  * @param A A matrix in AX + XB = C
	  * @param B B matrix in AX + XB = C
	  * @param C C matrix in AX + XB = C
	  * @return X in AX + XB = C
	  */
	def solveSylvesterEquation(A: Zmat, B: Zmat, C: Zmat): Zmat = {
		require(A.nrow == A.ncol && B.nrow == B.ncol && C.nrow == A.nrow && C.ncol == B.ncol,
			"Matrix dimensions must agree.")

		println(s"A$A\nB$B\nC$C\n")

		// the new dimensions are m*n x m*n or m*n x 1.
		val dimension = A.nrow * B.nrow
		// create I_n x A
		val I_A = new Zmat(dimension, dimension)
		// create B' x I_n
		val I_B = new Zmat(dimension, dimension)
		// create vec(C)
		val vec_C = new Zmat(dimension, 1)
		// create X
		val X = new Zmat(A.nrow, B.nrow)

		// fill I_A. only need one index since I_n is diagonal, and factor is always 1
		for (i <- 0 until B.nrow) {
			I_A.put(i * A.nrow, (i + 1) * A.nrow - 1, i * A.nrow, (i + 1) * A.nrow - 1, A)
		}
		// fill I_B
		for (i <- 0 until B.nrow; j <- 0 until B.nrow) {
			I_B.put(i * A.nrow, (i + 1) * A.nrow - 1, j * A.nrow, (j + 1) * A.nrow - 1, Eye.o(A.nrow) * B(j, i))
		}
		// fill vec_C
		for (i <- 0 until C.ncol) {
			vec_C.put(i * C.nrow, (i + 1) * C.nrow - 1, 0, 0, C.get(0, C.nrow - 1, i, i))
		}

		// solve the system
		val vec_X = (I_A + I_B).solve(vec_C)

		// fill and return X
		for (i <- 0 until X.ncol) {
			X.put(0, X.nrow - 1, i, i, vec_X.get(i * A.nrow, (i + 1) * A.nrow - 1, 0, 0))
		}
		X
	}

	// this doesn't improve anything, since full matrices are utilized. maybe for triangular or larger matrices...
	/** Solves Sylvester equation AX + XB = C by solving (U^-1^AU)(U^-1^XV) + (U^-1^XV)(V^-1^BV) = U^-1^CV, where U and
	  * V are Schur transformation matrices for A,,s,, = U^-1^AU and B,,s,, = V^-1^BV, respectively. More efficient than
	  * direct solving.
	  *
	  * @param A A matrix in AX + XB = C
	  * @param B B matrix in AX + XB = C
	  * @param C C matrix in AX + XB = C
	  * @return X in AX + XB = C
	  */
	def solveSylvesterEquationEfficiently(A: Zmat, B: Zmat, C: Zmat): Zmat = {
		require(A.nrow == A.ncol && B.nrow == B.ncol && C.nrow == A.nrow && C.ncol == B.ncol,
			"Matrix dimensions must agree.")

		val Schur_A = new Schur(A)
		val Schur_B = new Schur(B)

		// solve UF = CV for F
		val F = Schur_A.U.inv * C * Schur_B.U
		// solve A_sY + YB_s = F for Y
		val Y = solveSylvesterEquation(Schur_A.T, Schur_B.T, F)
		// solve XV = UY for X
		Schur_A.U * Y * Schur_B.U.inv
	}

	object Inhomogeneity extends Enumeration {
		val Zero, Constant, Exponential, Polynomial = Value
		//val Mixed = Value
	}

}

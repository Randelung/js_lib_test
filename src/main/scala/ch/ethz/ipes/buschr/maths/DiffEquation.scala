package ch.ethz.ipes.buschr.maths

import JampackNew._

import scala.util.control.Breaks._

/** Five step first order differential equation solver.
  *
  * Solves a first order differential system, either as a system matrix or by using factory method as a matrix of
  * coefficients of the equation solved to y^n^ = Ay^n-1^ + ... Dy, which becomes [A B C ... D].
  * The process is as follows:
  *
  * $ 1. Solve homogeneous first order problem given by y' = _matrix * y. This results in the object
  * `_eigenvalueDecomposition`, which contains the eigenvalues in D and the respective eigenvectors in X. Eigenvalues
  * and eigenvectors can be obtained using `eigenValues` and `eigenVectors` respectively. This is done automatically at
  * object creation.
  *
  * $ 2. Find generalized eigenvectors for base of y. This populates `_generalEigenVectors`, which is obtainable using
  * `generalEigenVectors`. This is done automatically at object creation.
  *
  * $ 3. (Optional) Add a non-homogeneous term to the right side, i.e. y' = Ay' + g. g can be either a constant, a
  * polynomial term, an exponential term or a mixture thereof. This is done by request using provided methods. TODO link correct methods
  *
  * $ 4. Add a data point to fix constants. This populates `_constants`, which is not directly obtainable. This step is
  * invoked using `applyStartingConditions` or `applyDataPoint`.
  *
  * $ 5. Create a function handle using `solution` that allows direct function evaluation up to y^n-1^.
  *
  * @param _matrix Matrix A in y'= Ax
  */
class DiffEquation(private val _matrix: Zmat) {
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
	private var _inhomogeneityExponents: Zmat = null

	// step 1: get eigenvalues and eigenvectors of matrix
	private val _eigenvalueDecomposition = new Eig(_matrix)

	// step 2: find generalized eigenvectors of matrix
	private val _generalEigenVectors = eigenVectors
	private var _seenEigs: Map[Z, Int] = Map()
	private var _usedSVD: Map[Z, JamaSVD] = Map()
	for (j <- _eigenvalueDecomposition.D.re.indices) {
		val eig = Z(_eigenvalueDecomposition.D.re(j), _eigenvalueDecomposition.D.im(j))
		if (_seenEigs.contains(eig.round(5))) {
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
				_generalEigenVectors.put(0, _generalEigenVectors.nrow - 1, j, j, v)
			}
			// else i haven't encountered this vector before, so it's good

			// increment count of eigenvector
			_seenEigs += (eig.round(5) -> (_seenEigs(eig.round(5)) + 1))
		}
		else {
			_seenEigs += (eig.round(5) -> 1)
		}
	}

	// step 3: apply Inhomogeneity
	/** Adds non-homogeneous term to the equation.
	  *
	  * The additional term can be changed to zero, a constant, or an exponential function of t. Polynomial terms and
	  * mixed expressions are not supported.
	  *
	  * @param `type`       Type of non-homogeneity added
	  * @param coefficients Complex array of coefficients for vector of either [1 t t^2^ ...]' for polynomials (in which
	  *                     case exponents is expected to be ''null'') or [exp(v1*t) exp(v2*t) ...]' for exponential
	  *                     inhomogeneities. The order of v1, v2 etc. is determined by the vector ''exponents''.
	  * @param exponents    Exponents for exponential inhomogeneity. Otherwise expected to be ''null''.
	  */
	def applyInhomogeneity(`type`: DiffEquation.Inhomogeneity.Value, coefficients: Zmat, exponents: Zmat): Unit = {
		`type` match {
			case DiffEquation.Inhomogeneity.Zero =>
				require(coefficients == null, "Inhomogeneity of zero expects a null pointer as parameters.")
				require(exponents == null, "Inhomogeneity of zero expects a null pointer as exponents.")
				_typeOfInhomogeneity = DiffEquation.Inhomogeneity.Zero
				_particularSolution = null
				_inhomogeneityExponents = null

			case DiffEquation.Inhomogeneity.Constant =>
				require(coefficients.ncol == 1, "Constant inhomogeneity expects a single vector.")
				require(exponents == null, "Constant inhomogeneity expects a null pointer as exponents.")
				require(coefficients.nrow == _matrix.nrow, "Inhomogeneity height must agree with matrix height.")

				// guess for x' = Ax + U for constant U is x = const, with x' = 0. Therefore, solve 0 = Ax + U, which is
				// x_particular_ = -pinv(A) * U
				_typeOfInhomogeneity = DiffEquation.Inhomogeneity.Constant
				_particularSolution = -_matrix.solve(coefficients)
				_inhomogeneityExponents = null

			case DiffEquation.Inhomogeneity.Exponential =>
				require(coefficients.ncol == exponents.nrow, "Coefficients and exponents dimensions must agree.")
				require(exponents.ncol == 1, "Exponents must be a single vector.")
				require(coefficients.nrow == _matrix.nrow, "Inhomogeneity height must agree with matrix height.")
				_typeOfInhomogeneity = DiffEquation.Inhomogeneity.Exponential
				_inhomogeneityExponents = exponents.clone()

				// x' = Ax + U for U = coefficients * exp(exponents * t) requires the guess x = X * exp(exponents * t),
				// so x' = X * diag(exponents) * exp(exponents * t) = A * X * exp(exponents * t) + coefficients * exp(exponents * t),
				// which allows for simplification to X * diag(exponents) = A * X + coefficients, which in turn leads to
				// the sylvester equation A * X - X * diag(exponents) = -coefficients
				val diffMatrix = new Zmat(new Zdiagmat(exponents.getZ.map(-_.head.re), exponents.getZ.map(-_.head.im)))
				_particularSolution = DiffEquation.solveSylvesterEquation(_matrix, diffMatrix, -coefficients)

			case DiffEquation.Inhomogeneity.Polynomial =>
				require(exponents == null, "Polynomial inhomogeneity expects a null pointer as exponents.")
				require(coefficients.nrow == _matrix.nrow, "Inhomogeneity height must agree with matrix height.")
				_inhomogeneityExponents = null
				_typeOfInhomogeneity = DiffEquation.Inhomogeneity.Polynomial

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
		_appliedDataPoint = false
	}

	// step 4: apply data point to fix constants
	//TODO maybe implement a method that takes a t[] and allows different points in time for each differentiation?
	private val _constants = Array.ofDim[Z](_matrix.nrow)

	def applyStartingConditions(vector: Array[Double]): Unit = {
		require(vector.length == _constants.length, s"Vector needs to be of size ${
			_constants.length
		} x 1.")

		_appliedDataPoint = true
		val vectorMatrix = new Zmat(vector.length, 1)
		vectorMatrix.put(0, vector.length - 1, 0, 0, new Zmat(vector.map(Array(_))))
		_typeOfInhomogeneity match {
			case DiffEquation.Inhomogeneity.Zero =>
				_generalEigenVectors.solve(vectorMatrix)
					.get(0, vector.length - 1, 0, 0).getZ.map(_.head).copyToArray(_constants)
			case DiffEquation.Inhomogeneity.Constant =>
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
	}

	def applyDataPoint(t: Double, vector: Array[Double]): Unit = {
		require(vector.length == _constants.length, s"Vector needs to be of size ${
			_constants.length
		} x 1.")

		if (t == 0) {
			applyStartingConditions(vector)
			return
		}

		_appliedDataPoint = true

		// calculate vectors scaled with their respective chain coefficients
		var scaledVectors = _generalEigenVectors.clone()
		var lastUnchangedEigenvector = 0
		for (j <- _generalEigenVectors.re.indices) {
			if (DiffEquation.sameEigenvectors(_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j, j).getZ.flatten,
				_eigenvalueDecomposition.X.get(0, _eigenvalueDecomposition.X.nrow - 1, j, j).getZ.flatten,
				5)) {
				lastUnchangedEigenvector = j
			}
			var temp: Zmat = new Zmat(Array.fill[Z](_generalEigenVectors.nrow, 1)(0))
			// build chain for current vector
			for (k <- j to lastUnchangedEigenvector by -1) {
				temp += _generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, k, k) * math.pow(t, j - k) / (2 to j - k).product
			}
			scaledVectors.put(0, scaledVectors.nrow - 1, j, j, temp)
		}
		// multiply exp(eig * t) in to the matrix using a diagonal matrix
		val temp = (_eigenvalueDecomposition.D.re, _eigenvalueDecomposition.D.im).zipped.map((i, j) => (new Z(i, j) * t).exp)
		scaledVectors *= new Zdiagmat(temp.map(_.re), temp.map(_.im))

		// put vector in matrix so it can be solved against
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
	}

	// step 5: pick up solution to the problem.
	def solution(differentiation: Int = 0): (Double => Z) = {
		require(differentiation >= 0, "Can't have a negative differentiation.")
		require(_appliedDataPoint, "No data point applied, solution not defined.")

		(t: Double) => {
			var result: Z = 0

			// add homogeneous solution
			var lastUnchangedEigenvector = 0
			for (j <- _generalEigenVectors.re.indices) {
				val eig = Z(_eigenvalueDecomposition.D.re(j), _eigenvalueDecomposition.D.im(j))
				if (DiffEquation.sameEigenvectors(_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j, j).getZ.flatten,
					_eigenvalueDecomposition.X.get(0, _eigenvalueDecomposition.X.nrow - 1, j, j).getZ.flatten,
					5)) {
					lastUnchangedEigenvector = j
				}
				var temp: Z = 0
				// build chain for current vector
				for (k <- j to lastUnchangedEigenvector by -1) {
					temp += _generalEigenVectors(differentiation, k) * (eig * t).exp * math.pow(t, j - k) / (2 to j - k).product
				}
				// multiply by constant
				result += temp * _constants(j)
			}

			// add inhomogeneous solution
			_typeOfInhomogeneity match {
				case DiffEquation.Inhomogeneity.Zero => // do nothing
				case DiffEquation.Inhomogeneity.Constant => result += _particularSolution(differentiation, 0)
				case DiffEquation.Inhomogeneity.Exponential =>
					result += (_particularSolution * new Zmat(_inhomogeneityExponents.getZ.map(_.map(i => (i * t).exp)))) (differentiation, 0)
				case DiffEquation.Inhomogeneity.Polynomial =>
					val vector = new Zmat(_particularSolution.ncol, 1)
					for (i <- 0 until vector.nrow) {
						vector(i, 0) = math.pow(t, i)
					}
					result += (_particularSolution * vector) (differentiation, 0)
			}

			result
		}: Z
	}

	def matrix = _matrix.clone()

	def eigenValues = (_eigenvalueDecomposition.D.re, _eigenvalueDecomposition.D.im).zipped.map(new Z(_, _))

	def eigenVectors = _eigenvalueDecomposition.X.clone()

	def generalEigenVectors = _generalEigenVectors.clone()

	def appliedDataPoint = _appliedDataPoint

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

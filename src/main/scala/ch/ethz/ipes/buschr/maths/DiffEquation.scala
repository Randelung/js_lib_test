package ch.ethz.ipes.buschr.maths

import JampackNew._

import scala.util.control.Breaks._

/**
  * Differential equation in matrix form, including solver. A represents all coefficients of the differential equation,
  * including zeros, starting at the highest order term (e.g. y\'\'\' = y\'\' + 3y' - 7y becomes {1, 3, -7}).
  *
  * @param coefficients The coefficients of the equation solved for the highest differential, not including inhomogenities.
  * @return Matrix representing the differential equation.
  */
class DiffEquation(coefficients: Array[Double]) {

	private val _constants = Array.ofDim[Z](coefficients.length)
	private val _matrix = new Zmat(coefficients.length, coefficients.length)
	for (i <- 0 until coefficients.length - 1) {
		_matrix(i, i + 1) = 1
	}
	_matrix(coefficients.length - 1) = coefficients.reverse.map(new Z(_))
	private val _eigenvalueDecomposition = new Eig(_matrix)

	private val _generalEigenVectors = eigenVectors
	private var _seenEigs: Map[Z, Int] = Map()
	private var _usedSVD: Map[Z, Zsvd] = Map()
	for (j <- _eigenvalueDecomposition.D.re.indices) {
		val eig = Z(_eigenvalueDecomposition.D.re(j), _eigenvalueDecomposition.D.im(j))
		if (_seenEigs.contains(eig.round(5))) {
			var foundBefore = false
			breakable {
				val v = _generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j, j).getZ.flatten.map(_.round(5))
				for (k <- j - 1 to 0 by -1) {
					if (v sameElements
						_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, k, k).getZ.flatten.map(_.round(5))) {
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
					val t = new Zsvd(matrix - Eye.o(matrix.nrow) * eig)
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

	def matrix = _matrix.clone()

	def eigenValues = (_eigenvalueDecomposition.D.re, _eigenvalueDecomposition.D.im).zipped.map(new Z(_, _))

	def eigenVectors = _eigenvalueDecomposition.X.clone()

	def generalEigenVectors = _generalEigenVectors.clone()

	def applyStartingConditions(vector: Array[Double]) = {
		require(vector.length == coefficients.length, s"Vector needs to be of size ${coefficients.length} x 1.")
		val vectorMatrix = new Zmat(vector.length, 1)
		vectorMatrix.put(0, vector.length - 1, 0, 0, new Zmat(vector.map(Array(_))))
		_generalEigenVectors.solve(vectorMatrix).get(0, vector.length - 1, 0, 0).getZ.map(_.head).copyToArray(_constants)
	}

	//TODO maybe implement a method that takes a t[] and allows different points in time for each differentiation?
	def applyDataPoint(t: Double, vector: Array[Double]): Unit = {
		require(vector.length == coefficients.length, s"Vector needs to be of size ${coefficients.length} x 1.")
		/*if (t == 0) {
			applyStartingConditions(vector)
			return
		}*/

		// calculate vectors scaled with their respective chain coefficients
		var scaledVectors = _generalEigenVectors.clone()
		var lastUnchangedEigenvector = 0
		for (j <- _generalEigenVectors.re.indices) {
			val eig = Z(_eigenvalueDecomposition.D.re(j), _eigenvalueDecomposition.D.im(j))
			if (_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j, j).getZ.flatten sameElements
				_eigenvalueDecomposition.X.get(0, _eigenvalueDecomposition.X.nrow - 1, j, j).getZ.flatten[Z]) {
				lastUnchangedEigenvector = j
			}
			var temp: Zmat = new Zmat(Array.fill[Z](_generalEigenVectors.nrow, 1)(0))
			// build chain for current vector
			for (k <- j to lastUnchangedEigenvector by -1) {
				temp += _generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, k, k) * math.pow(t, j - k) / (2 to j - k).product
			}
			scaledVectors.put(0, scaledVectors.nrow - 1, j, j, temp)
		}
		val temp = (_eigenvalueDecomposition.D.re, _eigenvalueDecomposition.D.im).zipped.map((i, j) => (new Z(i, j) * t).exp)
		scaledVectors *= new Zdiagmat(temp.map(_.re), temp.map(_.im))

		// put vector in matrix so it can be solved against
		val vectorMatrix = new Zmat(vector.length, 1)
		vectorMatrix.put(0, vector.length - 1, 0, 0, new Zmat(vector.map(Array(_))))
		scaledVectors.solve(vectorMatrix).get(0, vector.length - 1, 0, 0).getZ.map(_.head).copyToArray(_constants)
	}

	def solution(differentiation: Int = 0): (Double => Z) = {
		require(differentiation >= 0, "Can't have a negative differentiation.")
		(t: Double) => {
			var result: Z = 0
			var lastUnchangedEigenvector = 0
			for (j <- _generalEigenVectors.re.indices) {
				val eig = Z(_eigenvalueDecomposition.D.re(j), _eigenvalueDecomposition.D.im(j))
				if (_generalEigenVectors.get(0, _generalEigenVectors.nrow - 1, j, j).getZ.flatten sameElements
					_eigenvalueDecomposition.X.get(0, _eigenvalueDecomposition.X.nrow - 1, j, j).getZ.flatten[Z]) {
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
			result
		}: Z
	}
}

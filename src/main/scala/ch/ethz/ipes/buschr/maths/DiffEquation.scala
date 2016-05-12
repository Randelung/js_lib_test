package ch.ethz.ipes.buschr.maths

import Jama.Matrix

/**
  * Differential equation in matrix form, including solver. A represents all coefficients of the differential equation,
  * including zeros, starting at the highest order term (e.g. y\'\'\' = y\'\' + 3y' - 7y becomes {1, 3, -7}).
  *
  * @param coeffitiens The coefficients of the equation solved for the highest differential, not including inhomogenities.
  * @return Matrix representing the differential equation.
  */
class DiffEquation(coeffitiens: Array[Double]) {

	private val _constants = Array.ofDim[Double](coeffitiens.length)
	private val _matrix = new Matrix(coeffitiens.length, coeffitiens.length)
	for (i <- 0 until coeffitiens.length - 1) {
		_matrix(i)(i + 1) = 1
	}
	coeffitiens.reverse.copyToArray(_matrix(coeffitiens.length - 1))
	private val _eigenvalueDecomposition = _matrix.eig()

	def matrix = _matrix.clone()

	def realEigenValues = _eigenvalueDecomposition.getRealEigenvalues.clone()

	def complexEigenValues = _eigenvalueDecomposition.getImagEigenvalues.clone()

	def eigenVectors = _eigenvalueDecomposition.getV.clone()

	def applyStartingConditions(vector: Array[Double]) = {
		require(vector.length == coeffitiens.length, s"Vector needs to be of size ${coeffitiens.length} x 1.")
		val vectorMatrix = new Matrix(vector.length, 1)
		vectorMatrix.setColumn(0, vector)
		_eigenvalueDecomposition.getV.solve(vectorMatrix).getColumn(0).copyToArray(_constants)
	}

	def solution: (Double => Complex) = {
		(t: Double) => {
			var result: Complex = 0
			for (i <- _eigenvalueDecomposition.getV.getArray.indices) {
				result += _eigenvalueDecomposition.getV(0, i) * _constants(i) * (Complex(_eigenvalueDecomposition.getRealEigenvalues(i), _eigenvalueDecomposition.getImagEigenvalues(i)) * t).exp
			}
			result
		}: Complex
	}
}

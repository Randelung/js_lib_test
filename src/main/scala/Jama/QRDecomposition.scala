package Jama

import Jama.util.Maths

@SerialVersionUID(1)
class QRDecomposition(A: Matrix) {

	private val QR: Array[Array[Double]] = A.getArrayCopy

	private val m: Int = A.getRowDimension

	private val n: Int = A.getColumnDimension

	private val Rdiag: Array[Double] = Array.ofDim[Double](n)

	for (k <- 0 until n) {
		var nrm = 0d
		for (i <- k until m) {
			nrm = Maths.hypot(nrm, QR(i)(k))
		}
		if (nrm != 0.0) {
			if (QR(k)(k) < 0) {
				nrm = -nrm
			}
			for (i <- k until m) {
				QR(i)(k) /= nrm
			}
			QR(k)(k) += 1.0
			for (j <- k + 1 until n) {
				var s = 0.0
				for (i <- k until m) {
					s += QR(i)(k) * QR(i)(j)
				}
				s = -s / QR(k)(k)
				for (i <- k until m) {
					QR(i)(j) += s * QR(i)(k)
				}
			}
		}
		Rdiag(k) = -nrm
	}

	def isFullRank: Boolean = {
		!Rdiag.contains(0)
	}

	def getH: Matrix = {
		val X = new Matrix(m, n)
		val H = X.getArray
		for (i <- 0 until m; j <- 0 until n) {
			H(i)(j) = if (i >= j) QR(i)(j) else 0.0
		}
		X
	}

	def getR: Matrix = {
		val X = new Matrix(n, n)
		val R = X.getArray
		for (i <- 0 until n; j <- 0 until n) {
			R(i)(j) = if (i < j) QR(i)(j) else if (i == j) Rdiag(i) else 0.0
		}
		X
	}

	def getQ: Matrix = {
		val X = new Matrix(m, n)
		val Q = X.getArray
		var k = n - 1
		while (k >= 0) {
			for (i <- 0 until m) {
				Q(i)(k) = 0.0
			}
			Q(k)(k) = 1.0
			for (j <- k until n if QR(k)(k) != 0) {
				var s = 0.0
				for (i <- k until m) {
					s += QR(i)(k) * Q(i)(j)
				}
				s = -s / QR(k)(k)
				for (i <- k until m) {
					Q(i)(j) += s * QR(i)(k)
				}
			}
			k -= 1
		}
		X
	}

	def solve(B: Matrix): Matrix = {
		if (B.getRowDimension != m) {
			throw new IllegalArgumentException("Matrix row dimensions must agree.")
		}
		if (!this.isFullRank) {
			throw new RuntimeException("Matrix is rank deficient.")
		}
		val nx = B.getColumnDimension
		val X = B.getArrayCopy
		for (k <- 0 until n; j <- 0 until nx) {
			var s = 0.0
			for (i <- k until m) {
				s += QR(i)(k) * X(i)(j)
			}
			s = -s / QR(k)(k)
			for (i <- k until m) {
				X(i)(j) += s * QR(i)(k)
			}
		}
		var k = n - 1
		while (k >= 0) {
			for (j <- 0 until nx) {
				X(k)(j) /= Rdiag(k)
			}
			for (i <- 0 until k; j <- 0 until nx) {
				X(i)(j) -= X(k)(j) * QR(i)(k)
			}
			k -= 1
		}
		new Matrix(X, n, nx).getMatrix(0, n - 1, 0, nx - 1)
	}
}

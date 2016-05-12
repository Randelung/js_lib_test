package Jama

//remove if not needed

@SerialVersionUID(1)
class CholeskyDecomposition(Arg: Matrix) extends java.io.Serializable {

	private val n: Int = Arg.getRowDimension

	private val L: Array[Array[Double]] = Array.ofDim[Double](n, n)

	private var isspd: Boolean = Arg.getColumnDimension == n

	val A = Arg.getArray

	for (j <- 0 until n) {
		val Lrowj = L(j)
		var d = 0d
		for (k <- 0 until j) {
			val Lrowk = L(k)
			var s = 0d
			for (i <- 0 until k) {
				s += Lrowk(i) * Lrowj(i)
			}
			s = (A(j)(k) - s) / L(k)(k)
			Lrowj(k) = s
			d = d + s * s
			isspd = isspd & (A(k)(j) == A(j)(k))
		}
		d = A(j)(j) - d
		isspd = isspd & (d > 0d)
		L(j)(j) = math.sqrt(math.max(d, 0d))
		for (k <- j + 1 until n) {
			L(j)(k) = 0d
		}
	}

	def isSPD: Boolean = isspd

	def getL: Matrix = new Matrix(L, n, n)

	def solve(B: Matrix): Matrix = {
		if (B.getRowDimension != n) {
			throw new IllegalArgumentException("Matrix row dimensions must agree.")
		}
		if (!isspd) {
			throw new RuntimeException("Matrix is not symmetric positive definite.")
		}
		val X = B.getArrayCopy
		val nx = B.getColumnDimension
		for (k <- 0 until n; j <- 0 until nx) {
			for (i <- 0 until k) {
				X(k)(j) -= X(i)(j) * L(k)(i)
			}
			X(k)(j) /= L(k)(k)
		}
		for (k <- n - 1 to 0 by -1; j <- 0 until nx) {
			for (i <- k + 1 until n) {
				X(k)(j) -= X(i)(j) * L(i)(k)
			}
			X(k)(j) /= L(k)(k)
		}
		new Matrix(X, n, nx)
	}
}

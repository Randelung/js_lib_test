package JamaNew

//remove if not needed

@SerialVersionUID(1)
class LUDecomposition(A: Matrix) extends java.io.Serializable {

	private val LU: Array[Array[Double]] = A.getArrayCopy

	private val m: Int = A.getRowDimension

	private val n: Int = A.getColumnDimension

	private var pivsign: Int = 1

	private val piv: Array[Int] = Array.ofDim[Int](n)

	for (i <- 0 until m) {
		piv(i) = i
	}

	var LUrowi: Array[Double] = null

	val LUcolj = Array.ofDim[Double](m)

	for (j <- 0 until n) {
		for (i <- 0 until m) {
			LUcolj(i) = LU(i)(j)
		}
		for (i <- 0 until m) {
			LUrowi = LU(i)
			val kmax = Math.min(i, j)
			var s = 0.0
			for (k <- 0 until kmax) {
				s += LUrowi(k) * LUcolj(k)
			}
			LUcolj(i) -= s
			LUrowi(j) = LUcolj(i)
		}
		var p = j
		for (i <- j + 1 until m) {
			if (Math.abs(LUcolj(i)) > Math.abs(LUcolj(p))) {
				p = i
			}
		}
		if (p != j) {
			for (k <- 0 until n) {
				val t = LU(p)(k)
				LU(p)(k) = LU(j)(k)
				LU(j)(k) = t
			}
			val k = piv(p)
			piv(p) = piv(j)
			piv(j) = k
			pivsign = -pivsign
		}
		if (j < m & LU(j)(j) != 0.0) {
			for (i <- j + 1 until m) {
				LU(i)(j) /= LU(j)(j)
			}
		}
	}

	def isNonsingular: Boolean = {
		(0 until n).forall(i => LU(i)(i) != 0)
	}

	def getL: Matrix = {
		val X = new Matrix(m, n)
		val L = X.getArray
		for (i <- 0 until m; j <- 0 until n) {
			L(i)(j) = if (i > j) LU(i)(j) else if (i == j) 1.0 else 0.0
		}
		X
	}

	def getU: Matrix = {
		val X = new Matrix(n, n)
		val U = X.getArray
		for (i <- 0 until n; j <- 0 until n) {
			U(i)(j) = if (i <= j) LU(i)(j) else 0.0
		}
		X
	}

	def getPivot: Array[Int] = {
		val p = Array.ofDim[Int](m)
		piv.copyToArray(p)
		p
	}

	def getDoublePivot: Array[Double] = {
		val vals = Array.ofDim[Double](m)
		for (i <- 0 until m) {
			vals(i) = piv(i).toDouble
		}
		vals
	}

	def det(): Double = {
		if (m != n) {
			throw new IllegalArgumentException("Matrix must be square.")
		}
		var d = pivsign.toDouble
		for (j <- 0 until n) {
			d *= LU(j)(j)
		}
		d
	}

	def solve(B: Matrix): Matrix = {
		if (B.getRowDimension != m) {
			throw new IllegalArgumentException("Matrix row dimensions must agree.")
		}
		if (!this.isNonsingular) {
			throw new RuntimeException("Matrix is singular.")
		}
		val nx = B.getColumnDimension
		val Xmat = B.getMatrix(piv, 0, nx - 1)
		val X = Xmat.getArray
		for (k <- 0 until n; i <- k + 1 until n; j <- 0 until nx) {
			X(i)(j) -= X(k)(j) * LU(i)(k)
		}
		for (k <- n - 1 to 0 by -1) {
			for (j <- 0 until nx) {
				X(k)(j) /= LU(k)(k)
			}
			for (i <- 0 until k; j <- 0 until nx) {
				X(i)(j) -= X(k)(j) * LU(i)(k)
			}
		}
		Xmat
	}
}

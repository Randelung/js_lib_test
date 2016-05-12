package Jama

import Jama.util.Maths

import scala.util.Random

object Matrix {

	def constructWithCopy(A: Array[Array[Double]]): Matrix = {
		val m = A.length
		val n = A(0).length
		val X = new Matrix(m, n)
		val C = X.getArray
		for (i <- 0 until m) {
			if (A(i).length != n) {
				throw new IllegalArgumentException("All rows must have the same length.")
			}
			for (j <- 0 until n) {
				C(i)(j) = A(i)(j)
			}
		}
		X
	}

	def random(m: Int, n: Int): Matrix = {
		val A = new Matrix(m, n)
		val X = A.getArray
		val random = new Random()
		for (i <- 0 until m; j <- 0 until n) {
			X(i)(j) = random.nextInt(100)
		}
		A
	}

	def identity(m: Int, n: Int): Matrix = {
		val A = new Matrix(m, n)
		val X = A.getArray
		for (i <- 0 until m; j <- 0 until n) {
			X(i)(j) = if (i == j) 1.0 else 0.0
		}
		A
	}
}

@SerialVersionUID(1)
class Matrix(private var m: Int, private var n: Int) extends Cloneable with java.io.Serializable {

	private var A: Array[Array[Double]] = Array.ofDim[Double](m, n)

	def this(m: Int, n: Int, s: Double) {
		this(m, n)
		this.m = m
		this.n = n
		A = Array.ofDim[Double](m, n)
		for (i <- 0 until m; j <- 0 until n) {
			A(i)(j) = s
		}
	}

	def this(A: Array[Array[Double]]) {
		this(A.length, A.head.length)
		for (i <- 0 until m if A(i).length != n) {
			throw new IllegalArgumentException("All rows must have the same length.")
		}
		this.A = A
	}

	def this(A: Array[Array[Double]], m: Int, n: Int) {
		this(m, n)
		this.A = A
	}

	def this(vals: Array[Double], m: Int) {
		this(m, vals.length)
		n = if (m != 0) vals.length / m else 0
		if (m * n != vals.length) {
			throw new IllegalArgumentException("Array length must be a multiple of m.")
		}
		A = Array.ofDim[Double](m, n)
		for (i <- 0 until m; j <- 0 until n) {
			A(i)(j) = vals(i + j * m)
		}
	}

	def apply(i: Int, j: Int) = A(i)(j)

	def apply(i: Int) = A(i)

	def update(i: Int, j: Int, value: Double) = {
		require(i < A.length && j < A.head.length, s"Invalid indices: $i >= ${A.length} or $j >= ${A.head.length}.")
		A(i)(j) = value
	}

	def copy(): Matrix = {
		val X = new Matrix(m, n)
		val C = X.getArray
		for (i <- 0 until m; j <- 0 until n) {
			C(i)(j) = A(i)(j)
		}
		X
	}

	override def clone(): Matrix = this.copy()

	def getArray: Array[Array[Double]] = A

	def getArrayCopy: Array[Array[Double]] = {
		val C = Array.ofDim[Double](m, n)
		for (i <- 0 until m; j <- 0 until n) {
			C(i)(j) = A(i)(j)
		}
		C
	}

	def getColumnPackedCopy: Array[Double] = {
		val vals = Array.ofDim[Double](m * n)
		for (i <- 0 until m; j <- 0 until n) {
			vals(i + j * m) = A(i)(j)
		}
		vals
	}

	def getRowPackedCopy: Array[Double] = {
		val vals = Array.ofDim[Double](m * n)
		for (i <- 0 until m; j <- 0 until n) {
			vals(i * n + j) = A(i)(j)
		}
		vals
	}

	def getRowDimension: Int = m

	def getColumnDimension: Int = n

	def get(i: Int, j: Int): Double = A(i)(j)

	def getMatrix(i0: Int,
				  i1: Int,
				  j0: Int,
				  j1: Int): Matrix = {
		val X = new Matrix(i1 - i0 + 1, j1 - j0 + 1)
		val B = X.getArray
		try {
			for (i <- i0 to i1; j <- j0 to j1) {
				B(i - i0)(j - j0) = A(i)(j)
			}
		}
		catch {
			case e: ArrayIndexOutOfBoundsException =>
				throw new ArrayIndexOutOfBoundsException("Submatrix indices")
		}
		X
	}

	def getMatrix(r: Array[Int], c: Array[Int]): Matrix = {
		val X = new Matrix(r.length, c.length)
		val B = X.getArray
		try {
			for (i <- r.indices; j <- c.indices) {
				B(i)(j) = A(r(i))(c(j))
			}
		}
		catch {
			case e: ArrayIndexOutOfBoundsException =>
				throw new ArrayIndexOutOfBoundsException("Submatrix indices")
		}
		X
	}

	def getMatrix(i0: Int, i1: Int, c: Array[Int]): Matrix = {
		val X = new Matrix(i1 - i0 + 1, c.length)
		val B = X.getArray
		try {
			for (i <- i0 to i1; j <- c.indices) {
				B(i - i0)(j) = A(i)(c(j))
			}
		}
		catch {
			case e: ArrayIndexOutOfBoundsException =>
				throw new ArrayIndexOutOfBoundsException("Submatrix indices")
		}
		X
	}

	def getMatrix(r: Array[Int], j0: Int, j1: Int): Matrix = {
		val X = new Matrix(r.length, j1 - j0 + 1)
		val B = X.getArray
		try {
			for (i <- r.indices; j <- j0 to j1) {
				B(i)(j - j0) = A(r(i))(j)
			}
		}
		catch {
			case e: ArrayIndexOutOfBoundsException =>
				throw new ArrayIndexOutOfBoundsException("Submatrix indices")
		}
		X
	}

	def setColumn(j: Int, vector: Array[Double]): Unit = {
		require(vector.length == A.length, "Vector length must be equal to matrix height.")
		for (i <- vector.indices) {
			A(i)(j) = vector(i)
		}
	}

	def getColumn(j: Int): Array[Double] = {
		val array = Array.ofDim[Double](A.length)
		for (i <- A.indices) {
			array(i) = A(i)(j)
		}
		array
	}

	def setMatrix(i0: Int,
				  i1: Int,
				  j0: Int,
				  j1: Int,
				  X: Matrix) {
		try {
			for (i <- i0 to i1; j <- j0 to j1) {
				A(i)(j) = X.get(i - i0, j - j0)
			}
		}
		catch {
			case e: ArrayIndexOutOfBoundsException =>
				throw new ArrayIndexOutOfBoundsException("Submatrix indices")
		}
	}

	def setMatrix(r: Array[Int], c: Array[Int], X: Matrix) {
		try {
			for (i <- r.indices; j <- c.indices) {
				A(r(i))(c(j)) = X.get(i, j)
			}
		}
		catch {
			case e: ArrayIndexOutOfBoundsException =>
				throw new ArrayIndexOutOfBoundsException("Submatrix indices")
		}
	}

	def setMatrix(r: Array[Int],
				  j0: Int,
				  j1: Int,
				  X: Matrix) {
		try {
			for (i <- r.indices; j <- j0 to j1) {
				A(r(i))(j) = X.get(i, j - j0)
			}
		}
		catch {
			case e: ArrayIndexOutOfBoundsException =>
				throw new ArrayIndexOutOfBoundsException("Submatrix indices")
		}
	}

	def setMatrix(i0: Int,
				  i1: Int,
				  c: Array[Int],
				  X: Matrix) {
		try {
			for (i <- i0 to i1; j <- c.indices) {
				A(i)(c(j)) = X.get(i - i0, j)
			}
		}
		catch {
			case e: ArrayIndexOutOfBoundsException =>
				throw new ArrayIndexOutOfBoundsException("Submatrix indices")
		}
	}

	def transpose(): Matrix = {
		val X = new Matrix(n, m)
		val C = X.getArray
		for (i <- 0 until m; j <- 0 until n) {
			C(j)(i) = A(i)(j)
		}
		X
	}

	def norm1(): Double = {
		var f = 0d
		for (j <- 0 until n) {
			var s = 0d
			for (i <- 0 until m) {
				s += Math.abs(A(i)(j))
			}
			f = Math.max(f, s)
		}
		f
	}

	def norm2(): Double = {
		new SVDecomposition(this).norm2()
	}

	def normInf(): Double = {
		var f = 0d
		for (i <- 0 until m) {
			var s = 0d
			for (j <- 0 until n) {
				s += Math.abs(A(i)(j))
			}
			f = Math.max(f, s)
		}
		f
	}

	def normF(): Double = {
		var f = 0d
		for (i <- 0 until m; j <- 0 until n) {
			f = Maths.hypot(f, A(i)(j))
		}
		f
	}

	def unary_-(): Matrix = {
		val X = new Matrix(m, n)
		val C = X.getArray
		for (i <- 0 until m; j <- 0 until n) {
			C(i)(j) = -A(i)(j)
		}
		X
	}

	def +(B: Matrix): Matrix = {
		checkMatrixDimensions(B)
		val X = new Matrix(m, n)
		val C = X.getArray
		for (i <- 0 until m; j <- 0 until n) {
			C(i)(j) = A(i)(j) + B.A(i)(j)
		}
		X
	}

	def +=(B: Matrix): Matrix = {
		checkMatrixDimensions(B)
		for (i <- 0 until m; j <- 0 until n) {
			A(i)(j) = A(i)(j) + B.A(i)(j)
		}
		this
	}

	def -(B: Matrix): Matrix = {
		checkMatrixDimensions(B)
		val X = new Matrix(m, n)
		val C = X.getArray
		for (i <- 0 until m; j <- 0 until n) {
			C(i)(j) = A(i)(j) - B.A(i)(j)
		}
		X
	}

	def -=(B: Matrix): Matrix = {
		checkMatrixDimensions(B)
		for (i <- 0 until m; j <- 0 until n) {
			A(i)(j) = A(i)(j) - B.A(i)(j)
		}
		this
	}

	def elementTimes(B: Matrix): Matrix = {
		checkMatrixDimensions(B)
		val X = new Matrix(m, n)
		val C = X.getArray
		for (i <- 0 until m; j <- 0 until n) {
			C(i)(j) = A(i)(j) * B.A(i)(j)
		}
		X
	}

	def elementTimesEquals(B: Matrix): Matrix = {
		checkMatrixDimensions(B)
		for (i <- 0 until m; j <- 0 until n) {
			A(i)(j) = A(i)(j) * B.A(i)(j)
		}
		this
	}

	def elementRightDivide(B: Matrix): Matrix = {
		checkMatrixDimensions(B)
		val X = new Matrix(m, n)
		val C = X.getArray
		for (i <- 0 until m; j <- 0 until n) {
			C(i)(j) = A(i)(j) / B.A(i)(j)
		}
		X
	}

	def elementRightDivideEquals(B: Matrix): Matrix = {
		checkMatrixDimensions(B)
		for (i <- 0 until m; j <- 0 until n) {
			A(i)(j) = A(i)(j) / B.A(i)(j)
		}
		this
	}

	def elementLeftDivide(B: Matrix): Matrix = {
		checkMatrixDimensions(B)
		val X = new Matrix(m, n)
		val C = X.getArray
		for (i <- 0 until m; j <- 0 until n) {
			C(i)(j) = B.A(i)(j) / A(i)(j)
		}
		X
	}

	def elementLeftDivideEquals(B: Matrix): Matrix = {
		checkMatrixDimensions(B)
		for (i <- 0 until m; j <- 0 until n) {
			A(i)(j) = B.A(i)(j) / A(i)(j)
		}
		this
	}

	def *(s: Double): Matrix = {
		val X = new Matrix(m, n)
		val C = X.getArray
		for (i <- 0 until m; j <- 0 until n) {
			C(i)(j) = s * A(i)(j)
		}
		X
	}

	def *=(s: Double): Matrix = {
		for (i <- 0 until m; j <- 0 until n) {
			A(i)(j) = s * A(i)(j)
		}
		this
	}

	def *(B: Matrix): Matrix = {
		if (B.m != n) {
			throw new IllegalArgumentException("Matrix inner dimensions must agree.")
		}
		val X = new Matrix(m, B.n)
		val C = X.getArray
		val Bcolj = Array.ofDim[Double](n)
		for (j <- 0 until B.n) {
			for (k <- 0 until n) {
				Bcolj(k) = B.A(k)(j)
			}
			for (i <- 0 until m) {
				val Arowi = A(i)
				var s = 0d
				for (k <- 0 until n) {
					s += Arowi(k) * Bcolj(k)
				}
				C(i)(j) = s
			}
		}
		X
	}

	def lu(): LUDecomposition = new LUDecomposition(this)

	def qr(): QRDecomposition = new QRDecomposition(this)

	def chol(): CholeskyDecomposition = new CholeskyDecomposition(this)

	def svd(): SVDecomposition = new SVDecomposition(this)

	def eig(): EigenvalueDecomposition = new EigenvalueDecomposition(this)

	def solve(B: Matrix): Matrix = {
		if (m == n) new LUDecomposition(this).solve(B) else new QRDecomposition(this).solve(B)
	}

	def solveTranspose(B: Matrix): Matrix = transpose().solve(B.transpose())

	def inverse(): Matrix = solve(Matrix.identity(m, m))

	def det(): Double = new LUDecomposition(this).det()

	def rank(): Int = {
		new SVDecomposition(this).rank()
	}

	def cond(): Double = {
		new SVDecomposition(this).cond()
	}

	def trace(): Double = {
		var t = 0d
		for (i <- 0 until Math.min(m, n)) {
			t += A(i)(i)
		}
		t
	}

	private def checkMatrixDimensions(B: Matrix) {
		if (B.m != m || B.n != n) {
			throw new IllegalArgumentException("Matrix dimensions must agree.")
		}
	}

	override def toString: String = {
		A.deep.mkString("\n")
	}

	override def equals(obj: scala.Any): Boolean = {
		obj match {
			case matrix: Matrix =>
				for (i <- A.indices; j <- A.head.indices) {
					if (matrix.A(i)(j) != A(i)(j)) {
						return false
					}
				}
				true
			case _ => false
		}
	}
}

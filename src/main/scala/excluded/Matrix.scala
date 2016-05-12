package excluded

import ch.ethz.ipes.buschr.schematics.facades.Implicits

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Matrix private {

	private var table: mutable.Seq[mutable.Seq[Double]] = _

	def height = table.size

	def width = table.head.size

	def apply(i: Int, j: Int) = table(i)(j)

	def apply(i: Int) = table(i)

	def update(i: Int, j: Int, value: Double) = {
		require(i < table.size && j < table.head.size, "Index out of bounds.")
		table(i)(j) = value
	}

	def update(i: Int, value: Seq[Double]) = {
		require(table.head.size == value.size, "Row dimensions must match.")
		require(i < table.size, "Index out of bounds.")
		for (j <- value.indices) {
			table(i)(j) = value(j)
		}
	}

	def getColumn(j: Int): Seq[Double] = {
		require(j < table.head.size, "Index out of bounds.")
		val buffer = ListBuffer[Double]()
		table.foreach(i => buffer.append(i(j)))
		buffer
	}

	def setColumn(j: Int, vector: Seq[Double]): Unit = {
		require(j < table.head.size, "Index out of bounds.")
		require(vector.size == table.size, "Column dimnesions must match.")
		for (i <- vector.indices) {
			table(i)(j) = vector(i)
		}
	}

	def +(that: Matrix): Matrix = {
		require(that.table.size == table.size && that.table.head.size == table.head.size, "Matrix dimensions must agree.")
		val matrix = clone()
		for (i <- table.indices; j <- table.head.indices) {
			matrix(i, j) += that(i, j)
		}
		matrix
	}

	def -(that: Matrix): Matrix = {
		import Implicits.IntAsMatrix
		this + (-1).toSquareMatrix(that.height) * that
	}

	def *(factor: Matrix): Matrix = {
		val matrix = Matrix.zeros(table.size, factor.table.head.size)
		for (i <- matrix.table.indices; j <- factor.table.head.indices) {
			matrix(i, j) = {
				val v1 = this (i)
				val v2 = factor.getColumn(j)
				var sum = 0d
				for (k <- v1.indices) {
					sum += v1(k) * v2(k)
				}
				sum
			}
		}
		matrix
	}

	def **(factor: Matrix): Double = {
		require(width == 1 && factor.width == 1, "Matrices must be vectors, i.e. width = 0")
		var sum = 0d
		for (elem <- factor.table.indices) {
			sum += this (elem, 0) * factor(elem, 0)
		}
		sum
	}

	def solve(vector: Matrix): Matrix = {
		require(vector.table.size == table.size, "Matrix height must agreewith vector length.")
		val solution = vector.clone()
		val matrix = clone()
		// create diagonal matrix
		for (pivot <- matrix.table.indices) {
			require(matrix.table(pivot)(pivot) != 0, "Pivot is zero, which mean matrix is not full rank.")
			// make diagonal entry 1
			val divisor = matrix.table(pivot)(pivot)
			for (pivot_row <- pivot until matrix.table.head.size) {
				matrix.table(pivot)(pivot_row) = matrix.table(pivot)(pivot_row) / divisor
			}
			solution.table(pivot)(0) = solution.table(pivot).head / divisor
			// make lower rows 0 at pivot point
			for (pivot_column <- pivot + 1 until matrix.table.size) {
				// factor for subtraction
				val factor = matrix.table(pivot_column)(pivot)
				// subtract pivot row from current row
				for (current_row <- pivot until matrix.table.head.size) {
					matrix.table(pivot_column)(current_row) = matrix.table(pivot_column)(current_row) - factor * matrix.table(pivot)(current_row)
				}
				solution(pivot_column)(0) = solution(pivot_column).head - factor * solution(pivot).head
			}
		}
		// make it identity matrix
		for (pivot_row <- matrix.table.size - 1 until 0 by -1) {
			// make all entries in column 0
			for (current_row <- pivot_row - 1 to 0 by -1) {
				val factor = matrix.table(current_row)(pivot_row)
				matrix.table(current_row)(pivot_row) = 0
				solution(current_row)(0) = solution(current_row).head - factor * solution(pivot_row).head
			}
		}
		solution
	}

	def gauss: Matrix = {
		val matrix = clone()
		// create diagonal matrix
		for (pivot <- matrix.table.indices) {
			require(matrix.table(pivot)(pivot) != 0, "Pivot is zero, which mean matrix is not full rank.")
			// make diagonal entry 1
			val divisor = matrix.table(pivot)(pivot)
			for (pivot_row <- pivot until matrix.table.head.size) {
				matrix.table(pivot)(pivot_row) = matrix.table(pivot)(pivot_row) / divisor
			}
			// make lower rows 0 at pivot point
			for (pivot_column <- pivot + 1 until matrix.table.size) {
				// factor for subtraction
				val factor = matrix.table(pivot_column)(pivot)
				// subtract pivot row from current row
				for (current_row <- pivot until matrix.table.head.size) {
					matrix.table(pivot_column)(current_row) =
						matrix.table(pivot_column)(current_row) - factor * matrix.table(pivot)(current_row)
				}
			}
		}
		matrix
	}

	def determinant: Double = {
		val matrix = gauss
		var product = 1d
		for (i <- matrix.table.indices) {
			product = product * matrix.table(i)(i)
		}
		product
	}

	def vectorNorm: Double = {
		require(table.size == 1 || table.head.size == 1, "Matrix must be vector.")
		var sum = 0d
		if (table.size == 1) {
			table.head.foreach(i => sum += i * i)
		}
		else {
			table.foreach(i => sum += i.head * i.head)
		}
		math.sqrt(sum)
	}

	def transpose: Matrix = {
		val matrix = Matrix(table.head.size, table.size)
		for (i <- table.head.indices; j <- table.indices) {
			matrix(i)(j) = table(j)(i)
		}
		matrix
	}

	def QRDecomposition: (Matrix, Matrix) = {
		import ch.ethz.ipes.buschr.schematics.facades.Implicits.DoubleAsMatrix

		val matrix = clone()
		val Q = Matrix.identity(matrix.table.size)
		for (k <- matrix.table.head.indices) {
			val t = matrix.getColumn(k).splitAt(k)._2
			val z = Matrix(matrix.table.head.size - k, 1)
			z.setColumn(0, t)
			var v = Matrix(z.height, z.width)
			v(0, 0) = -math.signum(z(0, 0)) * z.vectorNorm - z(0, 0)
			for (i <- 1 until v.height) {
				v(i, 0) = -z(i, 0)
			}
			v = (1 / math.sqrt((v.transpose * v) (0, 0))).toSquareMatrix(v.height) * v

			for (j <- matrix.table.head.indices) {
				var t = Matrix(matrix.table.size - k, 1)
				val (l, r) = matrix.getColumn(j).splitAt(k)
				t.setColumn(0, r)
				t = t - v * (2d.toSquareMatrix(1) * (v.transpose * t))
				matrix.setColumn(j, l ++ t.getColumn(0))
			}

			for (j <- Q.table.indices) {
				var t = Matrix(Q.table.size - k, 1)
				val (l, r) = Q.getColumn(j).splitAt(k)
				t.setColumn(0, r)
				t = t - v * (2d.toSquareMatrix(1) * (v.transpose * t))
				Q.setColumn(j, l ++ t.getColumn(0))
			}
		}
		val R = Matrix(Q.table.size)
		for (i <- R.table.indices) {
			R(i) = matrix(i)
		}
		(Q.transpose, R)
	}

	override def toString: String = table.mkString("\n")

	override def clone(): Matrix = {
		val matrix = Matrix(table.size, table.head.size)
		for (i <- table.indices; j <- table.head.indices) {
			matrix(i)(j) = table(i)(j)
		}
		matrix
	}
}

object Matrix {

	def apply(size: Int) = {
		require(size > 0, "Can't initialize Matrix with size <= 0.")
		val m = new Matrix()
		m.table = mutable.Seq.fill(size, size)(0)
		m
	}

	def apply(height: Int, width: Int) = {
		require(height > 0, "Can't initialize Matrix with height <= 0.")
		require(width > 0, "Can't initialize matrix with width <= 0.")
		val m = new Matrix()
		m.table = mutable.Seq.fill(height, width)(0)
		m
	}

	def apply(height: Int, width: Int, initial: Double) = {
		require(height > 0, "Can't initialize Matrix with height <= 0.")
		require(width > 0, "Can't initialize matrix with width <= 0.")
		val m = new Matrix()
		m.table = mutable.Seq.fill(height, width)(initial)
		m
	}

	def identity(size: Int) = {
		require(size > 0, "Can't initialize Matrix with size <= 0.")
		val m = new Matrix()
		m.table = mutable.Seq.fill(size, size)(0)
		for (i <- 0 until size) {
			m.table(i)(i) = 1
		}
		m
	}

	def identity(height: Int, width: Int) = {
		require(height > 0, "Can't initialize Matrix with height <= 0.")
		require(width > 0, "Can't initialize matrix with width <= 0.")
		val m = new Matrix()
		m.table = mutable.Seq.fill(height, width)(0)
		for (i <- 0 until (if (height > width) width else height)) {
			m.table(i)(i) = 1
		}
		m
	}

	def zeros(size: Int) = {
		apply(size, size, 0)
	}

	def zeros(height: Int, width: Int) = {
		apply(height, width, 0)
	}

	def ones(size: Int) = {
		apply(size, size, 1)
	}

	def ones(height: Int, width: Int) = {
		apply(height, width, 1)
	}
}

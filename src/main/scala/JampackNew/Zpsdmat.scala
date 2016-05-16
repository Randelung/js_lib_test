package JampackNew

class Zpsdmat(re: Array[Array[Double]], im: Array[Array[Double]]) extends Zmat(re, im) {

	def this(A: Array[Array[Z]]) {
		this(A.map(_.map(_.re)), A.map(_.map(_.im)))
	}

	def this(A: Array[Array[Double]]) {
		this(A, Array.fill[Double](A.length, A.head.length)(0))
	}

	def this(A: Zmat) {
		this(A.re, A.im)
	}

	def this(nrow: Int, ncol: Int) {
		this(Array.fill[Double](nrow, ncol)(0), Array.fill[Double](nrow, ncol)(0))
	}
}

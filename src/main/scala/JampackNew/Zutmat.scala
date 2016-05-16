package JampackNew

class Zutmat(re: Array[Array[Double]], im: Array[Array[Double]]) extends Zmat(re, im) {

	def this(A: Array[Array[Z]]) {
		this(A.map(_.map(_.re)), A.map(_.map(_.im)))
	}

	def this(A: Array[Array[Double]]) {
		this(A, null)
	}

	def this(A: Zmat) {
		this(A.re, A.im)
	}

	def this(nrow: Int, ncol: Int) {
		this(Array.ofDim[Double](nrow, ncol), Array.ofDim[Double](nrow, ncol))
	}

	override def toString: String = Print.o(this)
}

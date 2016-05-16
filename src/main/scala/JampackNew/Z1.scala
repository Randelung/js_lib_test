package JampackNew

class Z1(var n: Int) {

	if (n <= 0) {
		throw new JampackException("Nonpositive dimension.")
	}

	var re = Array.ofDim[Double](n)

	var im = Array.ofDim[Double](n)

	def get(i: Int): Z = new Z(re(i), im(i))

	def put(i: Int, z: Z) {
		re(i) = z.re
		im(i) = z.im
	}

	def put(i: Int, real: Double, imag: Double) {
		re(i) = real
		im(i) = imag
	}

	def Times(i: Int, z: Z) {
		var t: Double = 0.0
		t = re(i) * z.re - im(i) * z.im
		im(i) = re(i) * z.im + im(i) * z.re
		re(i) = t
	}

	override def toString: String = Print.o(this)
}

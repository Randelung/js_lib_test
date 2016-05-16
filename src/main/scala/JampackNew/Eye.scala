package JampackNew

object Eye {
	def o(n: Int): Zmat = o(n, n)

	def o(m: Int, n: Int): Zmat = {
		val I = new Zmat(m, n)
		for (i <- 0 until math.min(m, n)) {
			I.re(i)(i) = 1
			I.im(i)(i) = 0
		}
		I
	}
}
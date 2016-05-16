package JampackNew

object H {
	def o(A: Zmat): Zmat = {
		val Ah: Zmat = new Zmat(A.nc, A.nr)
		for (i <- 0 until A.nr; j <- 0 until A.nc) {
			Ah.re(j)(i) = A.re(i)(j)
			Ah.im(j)(i) = -A.im(i)(j)
		}
		Ah
	}

	def o(D: Zdiagmat): Zdiagmat = {
		val Dh: Zdiagmat = new Zdiagmat(D)
		for (i <- 0 until Dh.n) {
			Dh.im(i) = -Dh.im(i)
		}
		Dh
	}

	def trans(A: Zmat): Zmat = {
		val Ah: Zmat = new Zmat(A.nc, A.nr)
		for (i <- 0 until A.nr; j <- 0 until A.nc) {
			Ah.re(j)(i) = A.re(i)(j)
			Ah.im(j)(i) = A.im(i)(j)
		}
		Ah
	}
}
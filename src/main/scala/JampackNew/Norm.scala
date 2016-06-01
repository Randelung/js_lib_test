package JampackNew

object Norm {

	def fro(A: Zmat,
			ii1: Int,
			ii2: Int,
			jj1: Int,
			jj2: Int): Double = {
		var i1: Int = 0
		var i2: Int = 0
		var j1: Int = 0
		var j2: Int = 0
		var fac: Double = 0.0
		var nrm: Double = 0.0
		var scale: Double = 0.0
		i1 = ii1 - A.basex
		i2 = ii2 - A.basex
		j1 = jj1 - A.basex
		j2 = jj2 - A.basex
		scale = 0.0
		for (i <- i1 to i2; j <- j1 to j2) {
			scale = Math.max(scale, Math.abs(A.re(i)(j)) + Math.abs(A.im(i)(j)))
		}
		if (scale == 0) {
			return 0.0
		}
		if (scale < 1) {
			scale = scale * 1.0e20
		}
		scale = 1 / scale
		nrm = 0
		for (i <- i1 to i2; j <- j1 to j2) {
			fac = scale * A.re(i)(j)
			nrm = nrm + fac * fac
			fac = scale * A.im(i)(j)
			nrm = nrm + fac * fac
		}
		Math.sqrt(nrm) / scale
	}

	def fro(A: Zmat): Double = {
		A.loadProperties()
		Norm.fro(A, A.baseIndex, A.rx, A.baseIndex, A.cx)
	}

	def fro(u: Z1): Double = {
		var fac: Double = 0.0
		var nrm: Double = 0.0
		var scale: Double = 0.0
		val n = u.n
		scale = 0.0
		for (i <- 0 until n) {
			scale = Math.max(scale, Math.abs(u.re(i)) + Math.abs(u.im(i)))
		}
		if (scale == 0) {
			return 0.0
		}
		if (scale < 1) {
			scale = scale * 1.0e20
		}
		scale = 1 / scale
		nrm = 0
		for (i <- 0 until n) {
			fac = scale * u.re(i)
			nrm = nrm + fac * fac
			fac = scale * u.im(i)
			nrm = nrm + fac * fac
		}
		Math.sqrt(nrm) / scale
	}

	def fro(D: Zdiagmat): Double = {
		var fac: Double = 0.0
		var nrm: Double = 0.0
		var scale: Double = 0.0
		val n = D.order
		scale = 0.0
		for (i <- 0 until n) {
			scale = Math.max(scale, Math.abs(D.re(i)) + Math.abs(D.im(i)))
		}
		if (scale == 0) {
			return 0.0
		}
		if (scale < 1) {
			scale = scale * 1.0e20
		}
		scale = 1 / scale
		nrm = 0
		for (i <- 0 until n) {
			fac = scale * D.re(i)
			nrm = nrm + fac * fac
			fac = scale * D.im(i)
			nrm = nrm + fac * fac
		}
		Math.sqrt(nrm) / scale
	}
}

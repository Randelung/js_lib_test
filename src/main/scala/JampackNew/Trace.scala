package JampackNew

object Trace {

	def o(A: Zmat): Z = {
		if (A.nc != A.nr) {
			throw new RuntimeException("Nonsquare matrix")
		}
		var t: Z = 0
		for (i <- 0 until A.nr) {
			t += A.getZ(i)(i)
		}
		t
	}

	def o(D: Zdiagmat): Z = {
		var t: Z = 0
		for (i <- 0 until D.order) {
			t += D.get(i)
		}
		t
	}
}

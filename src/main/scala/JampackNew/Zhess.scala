package JampackNew

class Zhess(A: Zmat) {

	if (A.nr != A.nc) {
		throw new JampackException("Matrix not square")
	}

	var H: Zmat = new Zmat(A)

	var U: Zmat = Eye.o(H.nr)

	val work = new Z1(H.nr)

	for (k <- H.bx until H.cx - 1) {
		val u = House.genc(H, k + 1, H.rx, k)
		House.ua(u, H, k + 1, H.rx, k + 1, H.cx, work)
		House.au(H, u, H.bx, H.rx, k + 1, H.cx, work)
		House.au(U, u, U.bx, U.rx, k + 1, U.cx, work)
	}
}

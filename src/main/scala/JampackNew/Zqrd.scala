package JampackNew

class Zqrd(A: Zmat) {

	A.getProperties()

	var Q: Zmat = Eye.o(A.nr)

	var hqr = if (A.HQR == null) new Zhqrd(A) else A.HQR

	var R: Zutmat = hqr.R

	if (A.nr > A.nc) {
		R = new Zutmat(Merge.o21(R, new Zmat(A.nr - A.nc, A.nc)))
	}

	for (k <- hqr.ntran - 1 to 0 by -1) {
		House.ua(hqr.U(k), Q, k + A.bx, A.rx, k + A.bx, A.rx)
	}
}

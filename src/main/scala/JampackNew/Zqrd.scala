package JampackNew

class Zqrd(A: Zmat) {

	A.loadProperties()

	var Q: Zmat = Eye.o(A.nr)

	var hqr = if (A.HQR == null) new Zhqrd(A) else A.HQR

	var R: Zutmat = hqr.R

	if (A.nr > A.nc) {
		R = new Zutmat(Merge.o21(R, new Zmat(A.nr - A.nc, A.nc)))
	}

	for (k <- hqr.ntran - 1 to 0 by -1) {
		House.ua(hqr.U(k), Q, k + A.baseIndex, A.rx, k + A.baseIndex, A.rx)
	}
}

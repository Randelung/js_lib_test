package JampackNew

class Zhqrd(A: Zmat) {

	var nrow: Int = A.nr

	var ncol: Int = A.nc

	var ntran: Int = Math.min(A.nr, A.nc)

	var U: Array[Z1] = Array.ofDim[Z1](ntran)

	var R: Zutmat = new Zutmat(A)

	A.loadProperties()

	for (k <- A.baseIndex until A.baseIndex + ntran) {
		U(k - A.baseIndex) = House.genc(R, k, A.rx, k)
		House.ua(U(k - A.baseIndex), R, k, A.rx, k + 1, A.cx)
	}

	if (nrow > ncol) {
		R = new Zutmat(R.get(R.baseIndex, R.cx, R.baseIndex, R.cx))
	}

	def qb(B: Zmat): Zmat = {
		if (B.ncol != ncol) {
			throw new JampackException("Inconsistent dimensions.")
		}
		val C = new Zmat(B)
		var k = ntran - 1
		while (k >= 0) {
			House.ua(U(k), C, C.baseIndex + k, C.rx, C.baseIndex, C.cx)
			k -= 1
		}
		C
	}

	def qhb(B: Zmat): Zmat = {
		if (B.ncol != ncol) {
			throw new JampackException("Inconsistent dimensions.")
		}
		val C = new Zmat(B)
		for (k <- 0 until ntran) {
			House.ua(U(k), C, C.baseIndex + k, C.rx, C.baseIndex, C.cx)
		}
		C
	}

	def bq(B: Zmat): Zmat = {
		if (B.nrow != ncol) {
			throw new JampackException("Inconsistent dimensions.")
		}
		H.o(qhb(H.o(B)))
	}

	def bqh(A: Zmat, B: Zmat): Zmat = {
		if (B.nrow != ncol) {
			throw new JampackException("Inconsistent dimensions.")
		}
		H.o(qb(H.o(B)))
	}
}

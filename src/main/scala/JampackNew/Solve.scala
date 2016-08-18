package JampackNew

object Solve {

	def aib(L: Zltmat, B: Zmat): Zmat = {
		L.loadProperties()
		B.loadProperties()
		if (L.nr != L.nc) throw new JampackException("Rectangular matrix.")
		if (L.nr != B.nr) throw new JampackException("Inconsistent dimensions.")
		val X = new Zmat(B)
		for (i <- 0 until L.nr; j <- 0 until B.nc) {
			for (k <- 0 until i) {
				X.re(i)(j) = X.re(i)(j) - L.re(i)(k) * X.re(k)(j) + L.im(i)(k) * X.im(k)(j)
				X.im(i)(j) = X.im(i)(j) - L.im(i)(k) * X.re(k)(j) - L.re(i)(k) * X.im(k)(j)
			}
			if (L.re(i)(i) == 0.0 && L.im(i)(i) == 0.0) throw new JampackException("Zero diagonal in solving triangular system")
			X.put0(i, j, X.get0(i, j) / L.get0(i, i))
		}
		X
	}

	def ahib(L: Zltmat, B: Zmat): Zmat = {
		var x: Z = 0
		L.loadProperties()
		B.loadProperties()
		if (L.nr != L.nc) throw new JampackException("Rectangular matrix.")
		if (L.nr != B.nr) throw new JampackException("Inconsistent dimensions.")
		val X = new Zmat(B)
		for (i <- L.nr - 1 to 0 by -1) {
			if (L.re(i)(i) == 0.0 && L.im(i)(i) == 0.0) throw new JampackException("Zero diagonal in solving triangular system")
			for (j <- 0 until B.nc) {
				x = X.get0(i, j) / L.get0(i, i).conj
				X.put0(i, j, x)
				for (k <- 0 until i) {
					X.re(k)(j) = X.re(k)(j) - X.re(i)(j) * L.re(i)(k) - X.im(i)(j) * L.im(i)(k)
					X.im(k)(j) = X.im(k)(j) + X.re(i)(j) * L.im(i)(k) - X.im(i)(j) * L.re(i)(k)
				}
			}
		}
		X
	}

	def bai(B: Zmat, L: Zltmat): Zmat = {
		L.loadProperties()
		B.loadProperties()
		if (L.nr != L.nc) throw new JampackException("Rectangular matrix.")
		if (L.nr != B.nc) throw new JampackException("Inconsistent dimensions.")
		H.o(ahib(L, H.o(B)))
	}

	def bahi(B: Zmat, L: Zltmat): Zmat = {
		L.loadProperties()
		B.loadProperties()
		if (L.nr != L.nc) throw new JampackException("Rectangular matrix.")
		if (L.nc != B.nc) throw new JampackException("Inconsistent dimensions.")
		H.o(aib(L, H.o(B)))
	}

	def aib(U: Zutmat, B: Zmat): Zmat = {
		var x: Z = 0
		U.loadProperties()
		B.loadProperties()
		if (U.nr != U.nc) throw new JampackException("Rectangular matrix.")
		if (U.nr != B.nr) throw new JampackException("Inconsistent dimensions.")
		val X = new Zmat(B)
		for (i <- U.nr - 1 to 0 by -1; j <- 0 until B.nc) {
			for (k <- i + 1 until U.nc) {
				X.re(i)(j) = X.re(i)(j) - U.re(i)(k) * X.re(k)(j) + U.im(i)(k) * X.im(k)(j)
				X.im(i)(j) = X.im(i)(j) - U.im(i)(k) * X.re(k)(j) - U.re(i)(k) * X.im(k)(j)
			}
			if (U.re(i)(i) == 0.0 && U.im(i)(i) == 0.0) throw new JampackException(s"Zero diagonal in solving triangular system")
			x = X.get0(i, j) / U.get0(i, i)
			X.put0(i, j, x)
		}
		X
	}

	def ahib(U: Zutmat, B: Zmat): Zmat = {
		var x: Z = 0
		U.loadProperties()
		B.loadProperties()
		if (U.nr != U.nc) throw new JampackException("Rectangular matrix.")
		if (U.nr != B.nr) throw new JampackException("Inconsistent dimensions.")
		val X = new Zmat(B)
		for (i <- 0 until U.nr) {
			if (U.re(i)(i) == 0.0 && U.im(i)(i) == 0) throw new JampackException("Zero diagonal in solving lower triangular system")
			for (j <- 0 until B.nc) {
				x = X.get0(i, j) / U.get0(i, i).conj
				X.put0(i, j, x)
				for (k <- i + 1 until U.nr) {
					X.re(k)(j) = X.re(k)(j) - X.re(i)(j) * U.re(i)(k) - X.im(i)(j) * U.im(i)(k)
					X.im(k)(j) = X.im(k)(j) - X.im(i)(j) * U.re(i)(k) + X.re(i)(j) * U.im(i)(k)
				}
			}
		}
		X
	}

	def bai(B: Zmat, U: Zutmat): Zmat = {
		U.loadProperties()
		B.loadProperties()
		if (U.nr != U.nc) throw new JampackException("Rectangular matrix.")
		if (U.nr != B.nc) throw new JampackException("Inconsistent dimensions.")
		H.o(ahib(U, H.o(B)))
	}

	def bahi(B: Zmat, U: Zutmat): Zmat = {
		U.loadProperties()
		B.loadProperties()
		if (U.nr != U.nc) throw new JampackException("Rectangular matrix.")
		if (U.nc != B.nc) throw new JampackException("Inconsistent dimensions.")
		H.o(aib(U, H.o(B)))
	}

	def aib(A: Zmat, B: Zmat): Zmat = {
		var LU: Zludpp = null
		A.loadProperties()
		B.loadProperties()
		if (A.nr != A.nc) throw new JampackException("Rectangular matrix.")
		if (A.nr != B.nr) throw new JampackException("Inconsistent dimensions.")
		if (Parameters.History) {
			A.clean()
			if (A.LU == null) A.LU = new Zludpp(A)
			LU = A.LU
		}
		else LU = new Zludpp(A)
		val X = new Zmat(B)
		Pivot.row(X, LU.pvt)
		aib(LU.U, aib(LU.L, X))
	}

	def ahib(A: Zmat, B: Zmat): Zmat = {
		var LU: Zludpp = null
		A.loadProperties()
		B.loadProperties()
		if (A.nr != A.nc) throw new JampackException("Rectangular matrix.")
		if (A.nr != B.nr) throw new JampackException("Inconsistent dimensions.")
		if (Parameters.History) {
			A.clean()
			if (A.LU == null) A.LU = new Zludpp(A)
			LU = A.LU
		}
		else LU = new Zludpp(A)
		Pivot.rowi(ahib(LU.L, ahib(LU.U, B)), LU.pvt)
	}

	def bai(B: Zmat, A: Zmat): Zmat = {
		var LU: Zludpp = null
		A.loadProperties()
		B.loadProperties()
		if (A.nr != A.nc) throw new JampackException("Rectangular matrix.")
		if (A.nr != B.nc) throw new JampackException("Inconsistent dimensions.")
		if (Parameters.History) {
			A.clean()
			if (A.LU == null) A.LU = new Zludpp(A)
			LU = A.LU
		}
		else LU = new Zludpp(A)
		H.o(ahib(A, H.o(B)))
	}

	def bahi(B: Zmat, A: Zmat): Zmat = {
		var LU: Zludpp = null
		A.loadProperties()
		B.loadProperties()
		if (A.nr != A.nc) throw new JampackException("Rectangular matrix.")
		if (A.nr != B.nc) throw new JampackException("Inconsistent dimensions.")
		if (Parameters.History) {
			A.clean()
			if (A.LU == null) A.LU = new Zludpp(A)
			LU = A.LU
		}
		else LU = new Zludpp(A)
		H.o(aib(A, H.o(B)))
	}

	def aib(A: Zpsdmat, B: Zmat): Zmat = {
		var CHOL: Zchol = null
		A.loadProperties()
		B.loadProperties()
		if (A.nr != A.nc) throw new JampackException("Rectangular matrix.")
		if (A.nr != B.nr) throw new JampackException("Inconsistent dimensions.")
		if (Parameters.History) {
			A.clean()
			if (A.CHOL == null) A.CHOL = new Zchol(A)
			CHOL = A.CHOL
		}
		else CHOL = new Zchol(A)
		Solve.aib(CHOL.R, Solve.ahib(CHOL.R, B))
	}

	def bai(B: Zmat, A: Zpsdmat): Zmat = {
		var CHOL: Zchol = null
		A.loadProperties()
		B.loadProperties()
		if (A.nr != A.nc) throw new JampackException("Rectangular matrix.")
		if (A.nr != B.nc) throw new JampackException("Inconsistent dimensions.")
		if (Parameters.History) {
			A.clean()
			if (A.CHOL == null) A.CHOL = new Zchol(A)
			CHOL = A.CHOL
		}
		else CHOL = new Zchol(A)
		Solve.bahi(Solve.bai(B, CHOL.R), CHOL.R)
	}
}

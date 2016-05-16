package JampackNew

object Times {

	def o(z: Z, A: Zmat): Zmat = {
		val B = new Zmat(A.nrow, A.ncol)
		for (i <- 0 until A.nrow; j <- 0 until A.ncol) {
			B.re(i)(j) = z.re * A.re(i)(j) - z.im * A.im(i)(j)
			B.im(i)(j) = z.im * A.re(i)(j) + z.re * A.im(i)(j)
		}
		B
	}

	def o(A: Zmat, B: Zmat): Zmat = {
		if (A.ncol != B.nrow) throw new JampackException("Unconformity in product")
		val C = new Zmat(A.nrow, B.ncol)
		for (i <- 0 until A.nrow; k <- 0 until A.ncol; j <- 0 until B.ncol) {
			C.re(i)(j) = C.re(i)(j) + A.re(i)(k) * B.re(k)(j) - A.im(i)(k) * B.im(k)(j)
			C.im(i)(j) = C.im(i)(j) + A.im(i)(k) * B.re(k)(j) + A.re(i)(k) * B.im(k)(j)
		}
		C
	}

	def aha(A: Zmat): Zpsdmat = {
		val C = new Zpsdmat(A.ncol, A.ncol)
		for (k <- 0 until A.nrow; i <- 0 until A.ncol) {
			C.re(i)(i) = C.re(i)(i) + A.re(k)(i) * A.re(k)(i) + A.im(k)(i) * A.im(k)(i)
			C.im(i)(i) = 0
			for (j <- i + 1 until A.ncol) {
				C.re(i)(j) = C.re(i)(j) + A.re(k)(i) * A.re(k)(j) + A.im(k)(i) * A.im(k)(j)
				C.im(i)(j) = C.im(i)(j) + A.re(k)(i) * A.im(k)(j) - A.im(k)(i) * A.re(k)(j)
			}
		}
		for (i <- 0 until A.ncol; j <- i + 1 until A.ncol) {
			C.re(j)(i) = C.re(i)(j)
			C.im(j)(i) = -C.im(i)(j)
		}
		C
	}

	def aah(A: Zmat): Zpsdmat = {
		val C = new Zpsdmat(A.nrow, A.nrow)
		for (i <- 0 until A.nrow) {
			for (k <- 0 until A.ncol) {
				C.re(i)(i) = C.re(i)(i) + A.re(i)(k) * A.re(i)(k) + A.im(i)(k) * A.im(i)(k)
			}
			C.im(i)(i) = 0
			for (j <- i + 1 until A.nrow) {
				for (k <- 0 until A.ncol) {
					C.re(i)(j) = C.re(i)(j) + A.re(i)(k) * A.re(j)(k) + A.im(i)(k) * A.im(j)(k)
					C.im(i)(j) = C.im(i)(j) - A.re(i)(k) * A.im(j)(k) + A.im(i)(k) * A.re(j)(k)
				}
				C.re(j)(i) = C.re(i)(j)
				C.im(j)(i) = -C.im(i)(j)
			}
		}
		C
	}

	def o(z: Z, D: Zdiagmat): Zdiagmat = {
		val B = new Zdiagmat(D)
		for (i <- 0 until D.order) {
			B.re(i) = z.re * D.re(i) - z.im * D.im(i)
			B.im(i) = z.im * D.re(i) + z.re * D.im(i)
		}
		B
	}

	def o(D1: Zdiagmat, D2: Zdiagmat): Zdiagmat = {
		if (D1.order != D2.order) {
			throw new JampackException("Unconformity in product")
		}
		val D3 = new Zdiagmat(D1.order)
		for (i <- 0 until D3.order) {
			D3.re(i) = D1.re(i) * D2.re(i) - D1.im(i) * D2.im(i)
			D3.im(i) = D1.re(i) * D2.im(i) + D1.im(i) * D2.re(i)
		}
		D3
	}

	def o(D: Zdiagmat, A: Zmat): Zmat = {
		if (D.order != A.nrow) {
			throw new JampackException("Unconformity in product.")
		}
		val B = new Zmat(A.nrow, A.ncol)
		for (i <- 0 until A.nrow; j <- 0 until A.nc) {
			B.re(i)(j) = D.re(i) * A.re(i)(j) - D.im(i) * A.im(i)(j)
			B.im(i)(j) = D.re(i) * A.im(i)(j) + D.im(i) * A.re(i)(j)
		}
		B
	}

	def o(A: Zmat, D: Zdiagmat): Zmat = {
		if (D.order != A.ncol) {
			throw new JampackException("Unconformity in product.")
		}
		val B = new Zmat(A.nrow, A.ncol)
		for (i <- 0 until A.nrow; j <- 0 until A.ncol) {
			B.re(i)(j) = D.re(j) * A.re(i)(j) - D.im(j) * A.im(i)(j)
			B.im(i)(j) = D.re(j) * A.im(i)(j) + D.im(j) * A.re(i)(j)
		}
		B
	}
}

package JampackNew

object Minus {

	def o(A: Zmat, B: Zmat): Zmat = {
		if (A.nrow != B.nrow || A.ncol != B.ncol) throw new JampackException("Matrices not conformable for subtraction")
		val C = new Zmat(A.nrow, A.ncol)
		for (i <- 0 until A.nrow; j <- 0 until A.ncol) {
			C.re(i)(j) = A.re(i)(j) - B.re(i)(j)
			C.im(i)(j) = A.im(i)(j) - B.im(i)(j)
		}
		C
	}

	def o(A: Zmat): Zmat = {
		val B = new Zmat(A.nrow, A.ncol)
		for (i <- 0 until A.nrow; j <- 0 until A.ncol) {
			B.re(i)(j) = -A.re(i)(j)
			B.im(i)(j) = -A.im(i)(j)
		}
		B
	}

	def o(A: Zmat, D: Zdiagmat): Zmat = {
		if (D.order != A.nrow || D.order != A.ncol) {
			throw new JampackException("Matrices not conformable for subtraction")
		}
		val C = new Zmat(A)
		for (i <- 0 until D.order) {
			C.re(i)(i) = C.re(i)(i) - D.re(i)
			C.im(i)(i) = C.im(i)(i) - D.im(i)
		}
		C
	}

	def o(D: Zdiagmat, A: Zmat): Zmat = {
		if (D.order != A.nrow || D.order != A.ncol) {
			throw new JampackException("Matrices not conformable for subtraction")
		}
		val C = new Zmat(A)
		for (i <- 0 until D.order) {
			C.re(i)(i) = D.re(i) - C.re(i)(i)
			C.im(i)(i) = D.im(i) - C.im(i)(i)
		}
		C
	}

	def o(D1: Zdiagmat, D2: Zdiagmat): Zdiagmat = {
		if (D1.order != D2.order) {
			throw new JampackException("Matrices not conformable for subtraction")
		}
		val C = new Zdiagmat(D1)
		for (i <- 0 until D1.order) {
			C.re(i) = C.re(i) - D2.re(i)
			C.im(i) = C.im(i) - D2.im(i)
		}
		C
	}

	def o(D: Zdiagmat): Zdiagmat = {
		val B = new Zdiagmat(D)
		for (i <- 0 until B.order) {
			B.re(i) = -B.re(i)
			B.im(i) = -B.im(i)
		}
		B
	}
}

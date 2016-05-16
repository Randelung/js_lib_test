package JampackNew

object Inv {

	def o(L: Zltmat): Zltmat = {
		if (L.nrow != L.ncol) throw new JampackException("Cannot compute the inverse of a rectangular matrix.")
		new Zltmat(Solve.aib(L, Eye.o(L.nrow)))
	}

	def o(U: Zutmat): Zutmat = {
		if (U.nrow != U.ncol) throw new JampackException("Cannot compute the inverse of a rectangular matrix.")
		new Zutmat(Solve.aib(U, Eye.o(U.nrow)))
	}

	def o(A: Zmat): Zmat = {
		if (A.nrow != A.ncol) throw new JampackException("Cannot compute the inverse of a rectangular matrix.")
		Solve.aib(A, Eye.o(A.nrow))
	}

	def o(A: Zpsdmat): Zpsdmat = {
		if (A.nrow != A.ncol) throw new JampackException("Cannot compute the inverse of a rectangular matrix.")
		val B = new Zpsdmat(Solve.aib(A, Eye.o(A.nrow)))
		for (i <- 0 until B.ncol) {
			for (j <- i + 1 until B.ncol) {
				B.re(j)(i) = B.re(i)(j)
				B.im(j)(i) = -B.im(i)(j)
			}
			B.im(i)(i) = 0.0
		}
		B
	}

	def o(D: Zdiagmat): Zdiagmat = {
		val Di = new Zdiagmat(D.n)
		for (i <- 0 until D.order) {
			var d = new Z(D.re(i), D.im(i))
			if (d.re == 0 && d.im == 0) {
				throw new JampackException("Singuar matrix.")
			}
			d = Z.ONE / d
			Di.re(i) = d.re
			Di.im(i) = d.im
		}
		Di
	}
}

package JampackNew

object Merge {

	def o(B: Array[Array[Zmat]]): Zmat = {
		var bi: Int = 0
		var bi0nr: Int = 0
		var bj: Int = 0
		var b0jnc: Int = 0
		var bnc: Int = 0
		var bnr: Int = 0
		var i: Int = 0
		var il: Int = 0
		var j: Int = 0
		var jl: Int = 0
		var nc: Int = 0
		var nr: Int = 0
		var Bij: Zmat = null
		bnr = B.length
		bnc = B(0).length
		nc = 0
		for (bj <- 0 until bnc) {
			b0jnc = B(0)(bj).ncol
			bi = 1
			while (bi < bnr) {
				if (B(bi)(bj).ncol != b0jnc) {
					throw new JampackException("Blocks do not conform")
				}
				bi += 1
			}
			nc = nc + b0jnc
		}
		nr = 0
		for (bi <- 0 until bnr) {
			bi0nr = B(bi)(0).nrow
			for (bj <- 1 until bnc) {
				if (B(bi)(bj).nrow != bi0nr) {
					throw new JampackException("Blocks do not conform")
				}
			}
			nr = nr + bi0nr
		}
		val A = new Zmat(nr, nc)
		il = 0
		for (bi <- 0 until bnr) {
			jl = 0
			for (bj <- 0 until bnc) {
				Bij = B(bi)(bj)
				for (i <- il until il + Bij.nrow; j <- jl until jl + Bij.ncol) {
					A.re(i)(j) = Bij.re(i - il)(j - jl)
					A.im(i)(j) = Bij.im(i - il)(j - jl)
				}
				jl = jl + Bij.ncol
			}
			il = il + B(bi)(0).nrow
		}
		A
	}

	def o12(B00: Zmat, B01: Zmat): Zmat = {
		val B = Array.ofDim[Zmat](1, 2)
		B(0)(0) = B00
		B(0)(1) = B01
		Merge.o(B)
	}

	def o21(B00: Zmat, B10: Zmat): Zmat = {
		val B = Array.ofDim[Zmat](2, 1)
		B(0)(0) = B00
		B(1)(0) = B10
		Merge.o(B)
	}

	def o22(B00: Zmat,
			B01: Zmat,
			B10: Zmat,
			B11: Zmat): Zmat = {
		val B = Array.ofDim[Zmat](2, 2)
		B(0)(0) = B00
		B(0)(1) = B01
		B(1)(0) = B10
		B(1)(1) = B11
		Merge.o(B)
	}

	def o13(B00: Zmat, B01: Zmat, B02: Zmat): Zmat = {
		val B = Array.ofDim[Zmat](1, 3)
		B(0)(0) = B00
		B(0)(1) = B01
		B(0)(2) = B02
		Merge.o(B)
	}

	def o23(B00: Zmat,
			B01: Zmat,
			B02: Zmat,
			B10: Zmat,
			B11: Zmat,
			B12: Zmat): Zmat = {
		val B = Array.ofDim[Zmat](2, 3)
		B(0)(0) = B00
		B(0)(1) = B01
		B(0)(2) = B02
		B(1)(0) = B10
		B(1)(1) = B11
		B(1)(2) = B12
		Merge.o(B)
	}

	def o31(B00: Zmat, B10: Zmat, B20: Zmat): Zmat = {
		val B = Array.ofDim[Zmat](3, 1)
		B(0)(0) = B00
		B(1)(0) = B10
		B(2)(0) = B20
		Merge.o(B)
	}

	def o32(B00: Zmat,
			B01: Zmat,
			B10: Zmat,
			B11: Zmat,
			B20: Zmat,
			B21: Zmat): Zmat = {
		val B = Array.ofDim[Zmat](3, 2)
		B(0)(0) = B00
		B(0)(1) = B01
		B(1)(0) = B10
		B(1)(1) = B11
		B(2)(0) = B20
		B(2)(1) = B21
		Merge.o(B)
	}

	def o33(B00: Zmat,
			B01: Zmat,
			B02: Zmat,
			B10: Zmat,
			B11: Zmat,
			B12: Zmat,
			B20: Zmat,
			B21: Zmat,
			B22: Zmat): Zmat = {
		val B = Array.ofDim[Zmat](3, 3)
		B(0)(0) = B00
		B(0)(1) = B01
		B(0)(2) = B02
		B(1)(0) = B10
		B(1)(1) = B11
		B(1)(2) = B12
		B(2)(0) = B20
		B(2)(1) = B21
		B(2)(2) = B22
		Merge.o(B)
	}
}

package JampackNew

object Pivot {

	def row(A: Zmat, pvt: Array[Int]): Zmat = {
		val np = pvt.length
		if (np > A.nrow) throw new JampackException("Inconsistent array dimensions")
		A.dirty = true
		for (k <- 0 until np; j <- 0 until A.ncol) {
			var t = A.re(k)(j)
			A.re(k)(j) = A.re(pvt(k))(j)
			A.re(pvt(k))(j) = t
			t = A.im(k)(j)
			A.im(k)(j) = A.im(pvt(k))(j)
			A.im(pvt(k))(j) = t
		}
		A
	}

	def rowi(A: Zmat, pvt: Array[Int]): Zmat = {
		val np = pvt.length
		if (np > A.nrow) throw new JampackException("Inconsistent array dimensions")
		A.dirty = true
		for (k <- np - 1 to 0 by -1; j <- 0 until A.nc) {
			var t = A.re(k)(j)
			A.re(k)(j) = A.re(pvt(k))(j)
			A.re(pvt(k))(j) = t
			t = A.im(k)(j)
			A.im(k)(j) = A.im(pvt(k))(j)
			A.im(pvt(k))(j) = t
		}
		A
	}
}

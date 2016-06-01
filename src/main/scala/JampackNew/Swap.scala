package JampackNew

object Swap {

	def rows(A: Zmat, _r1: Int, _r2: Int) {
		var r1 = _r1
		var r2 = _r2
		A.loadProperties()
		if (r1 < A.baseIndex || r1 > A.rx || r2 < A.baseIndex || r2 > A.rx) {
			throw new JampackException("Inconsistent row indices")
		}
		A.dirty = true
		r1 = r1 - A.baseIndex
		r2 = r2 - A.baseIndex
		for (j <- 0 until A.nr) {
			var t = A.re(r1)(j)
			A.re(r1)(j) = A.re(r2)(j)
			A.re(r2)(j) = t
			t = A.im(r1)(j)
			A.im(r1)(j) = A.im(r2)(j)
			A.im(r2)(j) = t
		}
	}

	def cols(A: Zmat, _c1: Int, _c2: Int) {
		var c1 = _c1
		var c2 = _c2
		A.loadProperties()
		if (c1 < A.baseIndex || c1 > A.cx || c2 < A.baseIndex || c2 > A.cx) {
			throw new JampackException("Inconsistent row indices")
		}
		A.dirty = true
		c1 = c1 - A.baseIndex
		c2 = c2 - A.baseIndex
		for (i <- 0 until A.nc) {
			var t = A.re(i)(c1)
			A.re(i)(c1) = A.re(i)(c2)
			A.re(i)(c2) = t
			t = A.im(i)(c1)
			A.im(i)(c1) = A.im(i)(c2)
			A.im(i)(c2) = t
		}
	}
}

package JampackNew

object House {

	def genc(A: Zmat,
			 _r1: Int,
			 _r2: Int,
			 _c: Int): Z1 = {
		var c = _c
		var r1 = _r1
		var r2 = _r2
		var ru = 0
		var norm = 0d
		var s = 0d
		var scale: Z = null
		var t: Z = 0
		c = c - A.basex
		r1 = r1 - A.basex
		r2 = r2 - A.basex
		ru = r2 - r1 + 1
		val u = new Z1(r2 - r1 + 1)
		for (i <- r1 to r2) {
			u.put(i - r1, A.re(i)(c), A.im(i)(c))
			A.re(i)(c) = 0
			A.im(i)(c) = 0
		}
		norm = Norm.fro(u)
		if (r1 == r2 || norm == 0) {
			A.re(r1)(c) = -u.re(0)
			A.im(r1)(c) = -u.im(0)
			u.put(0, math.sqrt(2), 0)
			return u
		}
		scale = new Z(1 / norm, 0)
		if (u.re(0) != 0 || u.im(0) != 0) {
			t = u.get(0)
			t = t.conj / t.abs
			scale *= t
		}
		t = -Z.ONE / scale
		A.put(r1 + A.basex, c + A.basex, t)
		for (i <- 0 until ru) {
			u.Times(i, scale)
		}
		u.re(0) += 1
		u.im(0) = 0
		s = math.sqrt(1 / u.re(0))
		for (i <- 0 until ru) {
			u.re(i) *= s
			u.im(i) *= s
		}
		u
	}

	def genr(A: Zmat,
			 _r: Int,
			 _c1: Int,
			 _c2: Int): Z1 = {
		var r = _r
		var c1 = _c1
		var c2 = _c2
		var cu = 0
		var norm = 0d
		var s = 0d
		var scale: Z = null
		var t: Z = 0
		var t1: Z = 0
		r = r - A.basex
		c1 = c1 - A.basex
		c2 = c2 - A.basex
		cu = c2 - c1 + 1
		val u = new Z1(cu)
		for (j <- c1 to c2) {
			u.put(j - c1, A.re(r)(j), A.im(r)(j))
			A.re(r)(j) = 0
			A.im(r)(j) = 0
		}
		norm = Norm.fro(u)
		if (c1 == c2 || norm == 0) {
			A.re(r)(c1) = -u.re(0)
			A.im(r)(c1) = -u.im(0)
			u.put(0, math.sqrt(2), 0)
			return u
		}
		scale = new Z(1 / norm, 0)
		if (u.re(0) != 0 || u.im(0) != 0) {
			t = u.get(0)
			t1 = t.conj
			t = t1 / t.abs
			scale *= t
		}
		t = -(Z.ONE / scale)
		A.put(r + A.basex, c1 + A.basex, t)
		for (j <- 0 until cu) {
			u.Times(j, scale)
		}
		u.re(0) = u.re(0) + 1
		u.im(0) = 0
		s = math.sqrt(1 / u.re(0))
		for (j <- 0 until cu) {
			u.re(j) *= s
			u.im(j) *= -s
		}
		u
	}

	def ua(u: Z1,
		   A: Zmat,
		   _r1: Int,
		   _r2: Int,
		   _c1: Int,
		   _c2: Int,
		   v: Z1): Zmat = {
		var r1 = _r1
		var r2 = _r2
		var c1 = _c1
		var c2 = _c2
		if (r2 < r1 || c2 < c1) {
			return A
		}
		if (r2 - r1 + 1 > u.n) {
			throw new JampackException("Householder vector too short.")
		}
		if (c2 - c1 + 1 > v.n) {
			throw new JampackException("Work vector too short.")
		}
		A.dirty = true
		r1 = r1 - A.basex
		r2 = r2 - A.basex
		c1 = c1 - A.basex
		c2 = c2 - A.basex
		for (j <- c1 to c2) {
			v.re(j - c1) = 0
			v.im(j - c1) = 0
		}
		for (i <- r1 to r2; j <- c1 to c2) {
			v.re(j - c1) = v.re(j - c1) + u.re(i - r1) * A.re(i)(j) + u.im(i - r1) * A.im(i)(j)
			v.im(j - c1) = v.im(j - c1) + u.re(i - r1) * A.im(i)(j) - u.im(i - r1) * A.re(i)(j)
		}
		for (i <- r1 to r2; j <- c1 to c2) {
			A.re(i)(j) = A.re(i)(j) - u.re(i - r1) * v.re(j - c1) + u.im(i - r1) * v.im(j - c1)
			A.im(i)(j) = A.im(i)(j) - u.re(i - r1) * v.im(j - c1) - u.im(i - r1) * v.re(j - c1)
		}
		A
	}

	def ua(u: Z1,
		   A: Zmat,
		   r1: Int,
		   r2: Int,
		   c1: Int,
		   c2: Int): Zmat = {
		if (c1 > c2) {
			return A
		}
		ua(u, A, r1, r2, c1, c2, new Z1(c2 - c1 + 1))
	}

	def au(A: Zmat,
		   u: Z1,
		   _r1: Int,
		   _r2: Int,
		   _c1: Int,
		   _c2: Int,
		   v: Z1): Zmat = {
		var r1 = _r1
		var r2 = _r2
		var c1 = _c1
		var c2 = _c2
		if (r2 < r1 || c2 < c1) {
			return A
		}
		if (c2 - c1 + 1 > u.n) {
			throw new JampackException("Householder vector too short.")
		}
		if (r2 - r1 + 1 > v.n) {
			throw new JampackException("Work vector too short.")
		}
		A.dirty = true
		r1 = r1 - A.basex
		r2 = r2 - A.basex
		c1 = c1 - A.basex
		c2 = c2 - A.basex
		for (i <- r1 to r2) {
			v.put(i - r1, 0, 0)
			for (j <- c1 to c2) {
				v.re(i - r1) = v.re(i - r1) + A.re(i)(j) * u.re(j - c1) - A.im(i)(j) * u.im(j - c1)
				v.im(i - r1) = v.im(i - r1) + A.re(i)(j) * u.im(j - c1) + A.im(i)(j) * u.re(j - c1)
			}
		}
		for (i <- r1 to r2; j <- c1 to c2) {
			A.re(i)(j) = A.re(i)(j) - v.re(i - r1) * u.re(j - c1) - v.im(i - r1) * u.im(j - c1)
			A.im(i)(j) = A.im(i)(j) + v.re(i - r1) * u.im(j - c1) - v.im(i - r1) * u.re(j - c1)
		}
		A
	}

	def au(A: Zmat,
		   u: Z1,
		   r1: Int,
		   r2: Int,
		   c1: Int,
		   c2: Int): Zmat = {
		if (r2 < r1) {
			return A
		}
		au(A, u, r1, r2, c1, c2, new Z1(r2 - r1 + 1))
	}
}

package JampackNew

object Rot {

	def genc(_xr: Double,
			 _xi: Double,
			 yr: Double,
			 yi: Double): Rot = {
		var xr = _xr
		var xi = _xi
		var s: Double = 0.0
		var absx: Double = 0.0
		var absxy: Double = 0.0
		val P = new Rot()
		if (xr == 0 && xi == 0) {
			P.c = 0
			P.sr = 1
			P.si = 0
			P.zr = yr
			P.zi = yi
			return P
		}
		s = math.abs(xr) + math.abs(xi)
		absx = s * math.sqrt((xr / s) * (xr / s) + (xi / s) * (xi / s))
		s = math.abs(s) + math.abs(yr) + math.abs(yi)
		absxy = s * math.sqrt((absx / s) * (absx / s) + (yr / s) * (yr / s) + (yi / s) * (yi / s))
		P.c = absx / absxy
		xr = xr / absx
		xi = xi / absx
		P.sr = (xr * yr + xi * yi) / absxy
		P.si = (xi * yr - xr * yi) / absxy
		P.zr = xr * absxy
		P.zi = xi * absxy
		P
	}

	def genc(_xr: Double,
			 _xi: Double,
			 yr: Double,
			 yi: Double,
			 P: Rot) {
		var xr = _xr
		var xi = _xi
		var s: Double = 0.0
		var absx: Double = 0.0
		var absxy: Double = 0.0
		if (xr == 0 && xi == 0) {
			P.c = 0
			P.sr = 1
			P.si = 0
			P.zr = yr
			P.zi = yi
			return
		}
		s = math.abs(xr) + math.abs(xi)
		absx = s * math.sqrt((xr / s) * (xr / s) + (xi / s) * (xi / s))
		s = math.abs(s) + math.abs(yr) + math.abs(yi)
		absxy = s * math.sqrt((absx / s) * (absx / s) + (yr / s) * (yr / s) + (yi / s) * (yi / s))
		P.c = absx / absxy
		xr = xr / absx
		xi = xi / absx
		P.sr = (xr * yr + xi * yi) / absxy
		P.si = (xi * yr - xr * yi) / absxy
		P.zr = xr * absxy
		P.zi = xi * absxy
	}

	def genc(x: Double, y: Double): Rot = {
		val P = new Rot()
		P.si = 0
		P.zi = 0
		if (x == 0 & y == 0) {
			P.c = 1
			P.sr = 0
			P.zr = 0
			return P
		}
		val s = math.abs(x) + math.abs(y)
		P.zr = s * math.sqrt((x / s) * (x / s) + (y / s) * (y / s))
		P.c = x / P.zr
		P.sr = y / P.zr
		P
	}

	def genc(x: Double, y: Double, P: Rot) {
		P.si = 0
		P.zi = 0
		if (x == 0 & y == 0) {
			P.c = 1
			P.sr = 0
			P.zr = 0
			return
		}
		val s = math.abs(x) + math.abs(y)
		P.zr = s * math.sqrt((x / s) * (x / s) + (y / s) * (y / s))
		P.c = x / P.zr
		P.sr = y / P.zr
	}

	def genc(A: Zmat,
			 ii1: Int,
			 ii2: Int,
			 jj: Int): Rot = {
		A.dirty = true
		val i1 = ii1 - A.basex
		val i2 = ii2 - A.basex
		val j = jj - A.basex
		val P = Rot.genc(A.re(i1)(j), A.im(i1)(j), A.re(i2)(j), A.im(i2)(j))
		A.re(i1)(j) = P.zr
		A.im(i1)(j) = P.zi
		A.re(i2)(j) = 0
		A.im(i2)(j) = 0
		P
	}

	def genc(A: Zmat,
			 ii1: Int,
			 ii2: Int,
			 jj: Int,
			 P: Rot) {
		A.dirty = true
		val i1 = ii1 - A.basex
		val i2 = ii2 - A.basex
		val j = jj - A.basex
		Rot.genc(A.re(i1)(j), A.im(i1)(j), A.re(i2)(j), A.im(i2)(j), P)
		A.re(i1)(j) = P.zr
		A.im(i1)(j) = P.zi
		A.re(i2)(j) = 0
		A.im(i2)(j) = 0
	}

	def genr(_xr: Double,
			 _xi: Double,
			 yr: Double,
			 yi: Double): Rot = {
		var xr = _xr
		var xi = _xi
		var s: Double = 0.0
		var absx: Double = 0.0
		var absxy: Double = 0.0
		val P = new Rot()
		if (xr == 0 && xi == 0) {
			P.c = 0
			P.sr = 1
			P.si = 0
			P.zr = yr
			P.zi = yi
			return P
		}
		s = math.abs(xr) + math.abs(xi)
		absx = s * math.sqrt((xr / s) * (xr / s) + (xi / s) * (xi / s))
		s = math.abs(s) + math.abs(yr) + math.abs(yi)
		absxy = s * math.sqrt((absx / s) * (absx / s) + (yr / s) * (yr / s) + (yi / s) * (yi / s))
		P.c = absx / absxy
		xr = xr / absx
		xi = xi / absx
		P.sr = -(xr * yr + xi * yi) / absxy
		P.si = (xi * yr - xr * yi) / absxy
		P.zr = xr * absxy
		P.zi = xi * absxy
		P
	}

	def genr(_xr: Double,
			 _xi: Double,
			 yr: Double,
			 yi: Double,
			 P: Rot) {
		var xr = _xr
		var xi = _xi
		var s: Double = 0.0
		var absx: Double = 0.0
		var absxy: Double = 0.0
		if (xr == 0 && xi == 0) {
			P.c = 0
			P.sr = 1
			P.si = 0
			P.zr = yr
			P.zi = yi
			return
		}
		s = math.abs(xr) + math.abs(xi)
		absx = s * math.sqrt((xr / s) * (xr / s) + (xi / s) * (xi / s))
		s = math.abs(s) + math.abs(yr) + math.abs(yi)
		absxy = s * math.sqrt((absx / s) * (absx / s) + (yr / s) * (yr / s) + (yi / s) * (yi / s))
		P.c = absx / absxy
		xr = xr / absx
		xi = xi / absx
		P.sr = -(xr * yr + xi * yi) / absxy
		P.si = (xi * yr - xr * yi) / absxy
		P.zr = xr * absxy
		P.zi = xi * absxy
	}

	def genr(A: Zmat,
			 ii: Int,
			 jj1: Int,
			 jj2: Int): Rot = {
		A.dirty = true
		val i = ii - A.basex
		val j1 = jj1 - A.basex
		val j2 = jj2 - A.basex
		val P = Rot.genr(A.re(i)(j1), A.im(i)(j1), A.re(i)(j2), A.im(i)(j2))
		A.re(i)(j1) = P.zr
		A.im(i)(j1) = P.zi
		A.re(i)(j2) = 0
		A.im(i)(j2) = 0
		P
	}

	def genr(A: Zmat,
			 ii: Int,
			 jj1: Int,
			 jj2: Int,
			 P: Rot) {
		A.dirty = true
		val i = ii - A.basex
		val j1 = jj1 - A.basex
		val j2 = jj2 - A.basex
		Rot.genr(A.re(i)(j1), A.im(i)(j1), A.re(i)(j2), A.im(i)(j2), P)
		A.re(i)(j1) = P.zr
		A.im(i)(j1) = P.zi
		A.re(i)(j2) = 0
		A.im(i)(j2) = 0
	}

	def genr(x: Double, y: Double): Rot = {
		val P = new Rot()
		P.si = 0
		P.zi = 0
		val s = math.abs(x) + math.abs(y)
		if (s == 0) {
			P.c = 1
			P.sr = 0
			P.zr = 0
			return P
		}
		P.zr = s * math.sqrt((x / s) * (x / s) + (y / s) * (y / s))
		P.c = x / P.zr
		P.sr = -y / P.zr
		P
	}

	def genr(x: Double, y: Double, P: Rot) {
		P.si = 0
		P.zi = 0
		val s = math.abs(x) + math.abs(y)
		if (s == 0) {
			P.c = 1
			P.sr = 0
			P.zr = 0
			return
		}
		P.zr = s * math.sqrt((x / s) * (x / s) + (y / s) * (y / s))
		P.c = x / P.zr
		P.sr = -y / P.zr
	}

	def pa(P: Rot,
		   A: Zmat,
		   ii1: Int,
		   ii2: Int,
		   jj1: Int,
		   jj2: Int) {
		var t1r: Double = 0.0
		var t1i: Double = 0.0
		var t2r: Double = 0.0
		var t2i: Double = 0.0
		A.dirty = true
		val i1 = ii1 - A.basex
		val i2 = ii2 - A.basex
		val j1 = jj1 - A.basex
		val j2 = jj2 - A.basex
		for (j <- j1 to j2) {
			t1r = P.c * A.re(i1)(j) + P.sr * A.re(i2)(j) - P.si * A.im(i2)(j)
			t1i = P.c * A.im(i1)(j) + P.sr * A.im(i2)(j) + P.si * A.re(i2)(j)
			t2r = P.c * A.re(i2)(j) - P.sr * A.re(i1)(j) - P.si * A.im(i1)(j)
			t2i = P.c * A.im(i2)(j) - P.sr * A.im(i1)(j) + P.si * A.re(i1)(j)
			A.re(i1)(j) = t1r
			A.im(i1)(j) = t1i
			A.re(i2)(j) = t2r
			A.im(i2)(j) = t2i
		}
	}

	def pha(P: Rot,
			A: Zmat,
			ii1: Int,
			ii2: Int,
			jj1: Int,
			jj2: Int) {
		var t1r: Double = 0.0
		var t1i: Double = 0.0
		var t2r: Double = 0.0
		var t2i: Double = 0.0
		A.dirty = true
		val i1 = ii1 - A.basex
		val i2 = ii2 - A.basex
		val j1 = jj1 - A.basex
		val j2 = jj2 - A.basex
		for (j <- j1 to j2) {
			t1r = P.c * A.re(i1)(j) - P.sr * A.re(i2)(j) + P.si * A.im(i2)(j)
			t1i = P.c * A.im(i1)(j) - P.sr * A.im(i2)(j) - P.si * A.re(i2)(j)
			t2r = P.c * A.re(i2)(j) + P.sr * A.re(i1)(j) + P.si * A.im(i1)(j)
			t2i = P.c * A.im(i2)(j) + P.sr * A.im(i1)(j) - P.si * A.re(i1)(j)
			A.re(i1)(j) = t1r
			A.im(i1)(j) = t1i
			A.re(i2)(j) = t2r
			A.im(i2)(j) = t2i
		}
	}

	def ap(A: Zmat,
		   P: Rot,
		   ii1: Int,
		   ii2: Int,
		   jj1: Int,
		   jj2: Int) {
		var t1r: Double = 0.0
		var t1i: Double = 0.0
		var t2r: Double = 0.0
		var t2i: Double = 0.0
		A.dirty = true
		val i1 = ii1 - A.basex
		val i2 = ii2 - A.basex
		val j1 = jj1 - A.basex
		val j2 = jj2 - A.basex
		for (i <- i1 to i2) {
			t1r = P.c * A.re(i)(j1) - P.sr * A.re(i)(j2) - P.si * A.im(i)(j2)
			t1i = P.c * A.im(i)(j1) - P.sr * A.im(i)(j2) + P.si * A.re(i)(j2)
			t2r = P.c * A.re(i)(j2) + P.sr * A.re(i)(j1) - P.si * A.im(i)(j1)
			t2i = P.c * A.im(i)(j2) + P.sr * A.im(i)(j1) + P.si * A.re(i)(j1)
			A.re(i)(j1) = t1r
			A.im(i)(j1) = t1i
			A.re(i)(j2) = t2r
			A.im(i)(j2) = t2i
		}
	}

	def aph(A: Zmat,
			P: Rot,
			ii1: Int,
			ii2: Int,
			jj1: Int,
			jj2: Int) {
		var t1r: Double = 0.0
		var t1i: Double = 0.0
		var t2r: Double = 0.0
		var t2i: Double = 0.0
		A.dirty = true
		val i1 = ii1 - A.basex
		val i2 = ii2 - A.basex
		val j1 = jj1 - A.basex
		val j2 = jj2 - A.basex
		for (i <- i1 to i2) {
			t1r = P.c * A.re(i)(j1) + P.sr * A.re(i)(j2) + P.si * A.im(i)(j2)
			t1i = P.c * A.im(i)(j1) + P.sr * A.im(i)(j2) - P.si * A.re(i)(j2)
			t2r = P.c * A.re(i)(j2) - P.sr * A.re(i)(j1) + P.si * A.im(i)(j1)
			t2i = P.c * A.im(i)(j2) - P.sr * A.im(i)(j1) - P.si * A.re(i)(j1)
			A.re(i)(j1) = t1r
			A.im(i)(j1) = t1i
			A.re(i)(j2) = t2r
			A.im(i)(j2) = t2i
		}
	}
}

class Rot {

	var c: Double = _

	var sr: Double = _

	var si: Double = _

	var zr: Double = _

	var zi: Double = _
}

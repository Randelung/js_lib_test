package JampackNew

import scala.util.control.Breaks

object Zsvd {

	var MAXITER: Int = 30
}

class Zsvd(XX: Zmat) {

	private val X = new Zmat(XX)

	var U: Zmat = Eye.o(X.nr)

	var V: Zmat = Eye.o(X.nc)

	private val mc = math.min(X.nr, X.nc)

	var S: Zdiagmat = new Zdiagmat(mc)

	private var il: Int = 0
	private var iu: Int = 0
	private var iter: Int = 0
	private var kk: Int = 0
	private var m: Int = 0

	private var as: Double = 0.0
	private var at: Double = 0.0
	private var au: Double = 0.0
	private var axkk: Double = 0.0
	private var axkk1: Double = 0.0
	private var dmax: Double = 0.0
	private var dmin: Double = 0.0
	private var ds: Double = 0.0
	private var ea: Double = 0.0
	private var es: Double = 0.0
	private var shift: Double = 0.0
	private var ss: Double = 0.0
	private var t: Double = 0.0

	private var xkk: Z = null
	private var xkk1: Z = null
	private var xk1k1: Z = null

	private val P = new Rot()

	private var scale: Z = 0

	private var zr: Z = 0

	private var h: Z1 = null

	private val temp = new Z1(math.max(X.nr, X.nc))

	private val d = Array.ofDim[Double](mc)

	private val e = Array.ofDim[Double](mc - 1)

	m = math.min(X.rx, X.cx)

	for (k <- X.baseIndex to m) {
		h = House.genc(X, k, X.rx, k)
		House.ua(h, X, k, X.rx, k + 1, X.cx, temp)
		House.au(U, h, U.baseIndex, U.rx, k, U.cx, temp)
		if (k != X.cx) {
			h = House.genr(X, k, k + 1, X.cx)
			House.au(X, h, k + 1, X.rx, k + 1, X.cx, temp)
			House.au(V, h, V.baseIndex, V.rx, k + 1, V.cx, temp)
		}
	}

	for (k <- X.baseIndex to m) {
		kk = k - X.baseIndex
		xkk = X.get(k, k)
		axkk = xkk.abs
		X.put(k, k, new Z(axkk))
		d(kk) = axkk
		scale = if (axkk != 0) xkk.conj / axkk else 1
		if (k < X.cx) {
			X(k, k + 1) *= scale
		}
		scale = scale.conj
		for (i <- U.baseIndex to U.rx) {
			U(i, k) *= scale
		}
		if (k < X.cx) {
			xkk1 = X.get(k, k + 1)
			axkk1 = xkk1.abs
			X.put(k, k + 1, new Z(axkk1))
			e(kk) = axkk1
			scale = if (axkk1 != 0) xkk1.conj / axkk1 else 1
			if (k < X.rx) {
				X(k + 1, k + 1) *= scale
			}
			for (i <- V.baseIndex to V.rx) {
				V(i, k + 1) *= scale
			}
		}
	}

	m = m - X.baseIndex

	if (X.nr < X.nc) {
		t = e(m)
		for (k <- m to 0 by -1) {
			Rot.genr(d(k), t, P)
			d(k) = P.zr
			if (k != 0) {
				t = P.sr * e(k - 1)
				e(k - 1) = P.c * e(k - 1)
			}
			Rot.ap(V, P, V.baseIndex, V.rx, k + V.baseIndex, X.rx + 1)
			Rot.ap(X, P, X.baseIndex, X.rx, k + X.baseIndex, X.rx + 1)
		}
	}

	iu = m

	iter = 0

	val outer = new Breaks
	val inner = new Breaks

	println(e.deep)
	println(d.deep)

	outer.breakable {
		while (true) {
			inner.breakable {
				while (iu > 0) {
					if (math.abs(e(iu - 1)) > 1.0e-16 * (math.abs(d(iu)) + math.abs(d(iu - 1)))) {
						inner.break
					}
					e(iu - 1) = 0
					iter = 0
					iu = iu - 1
				}
			}
			iter = iter + 1
			if (iter > Zsvd.MAXITER) {
				throw new JampackException("Maximum number of iterations exceeded.")
			}
			if (iu == 0) {
				outer.break
			}
			il = iu - 1
			inner.breakable {
				while (il > 0) {
					if (math.abs(e(il - 1)) <= 1.0e-16 * (math.abs(d(il)) + math.abs(d(il - 1)))) {
						inner.break
					}
					il = il - 1
				}
			}
			if (il != 0) {
				e(il - 1) = 0
			}
			dmax = math.max(math.abs(d(iu)), math.abs(d(iu - 1)))
			dmin = math.min(math.abs(d(iu)), math.abs(d(iu - 1)))
			ea = math.abs(e(iu - 1))
			if (dmin == 0) {
				shift = 0
			}
			else if (ea < dmax) {
				as = 1 + dmin / dmax
				at = (dmax - dmin) / dmax
				au = ea / dmax
				au = au * au
				shift = dmin * (2 / (math.sqrt(as * as + au) + math.sqrt(at * at + au)))
			}
			else {
				au = dmax / ea
				if (au == 0) {
					shift = (dmin * dmax) / ea
				}
				else {
					as = 1 + dmin / dmax
					at = (dmax - dmin) / dmax
					t = 1 / (math.sqrt(1 + (as * au) * (as * au)) + math.sqrt(1 + (at * au) * (at * au)))
					shift = (t * dmin) * au
				}
			}
			t = math.max(math.max(math.abs(d(il)), math.abs(e(il))), shift)
			ds = d(il) / t
			es = e(il) / t
			ss = shift / t
			Rot.genr((ds - ss) * (ds + ss), ds * es, P)
			for (i <- il until iu) {
				t = P.c * d(i) - P.sr * e(i)
				e(i) = P.sr * d(i) + P.c * e(i)
				d(i) = t
				t = -P.sr * d(i + 1)
				d(i + 1) = P.c * d(i + 1)
				Rot.ap(V, P, V.baseIndex, V.rx, V.baseIndex + i, V.baseIndex + i + 1)
				Rot.genc(d(i), t, P)
				d(i) = P.zr
				t = P.c * e(i) + P.sr * d(i + 1)
				d(i + 1) = P.c * d(i + 1) - P.sr * e(i)
				e(i) = t
				Rot.aph(U, P, U.baseIndex, U.rx, U.baseIndex + i, U.baseIndex + i + 1)
				if (i != iu - 1) {
					t = P.sr * e(i + 1)
					e(i + 1) = P.c * e(i + 1)
					Rot.genr(e(i), t, P)
					e(i) = P.zr
				}
			}
		}
	}

	for (k <- m to 0 by -1) {
		if (d(k) < 0) {
			d(k) = -d(k)
			for (i <- 0 until X.nc) {
				V.re(i)(k) = -V.re(i)(k)
				V.im(i)(k) = -V.im(i)(k)
			}
		}
		for (j <- k until m) {
			if (d(j) < d(j + 1)) {
				t = d(j)
				d(j) = d(j + 1)
				d(j + 1) = t
				for (i <- 0 until X.nr) {
					t = U.re(i)(j)
					U.re(i)(j) = U.re(i)(j + 1)
					U.re(i)(j + 1) = t
					t = U.im(i)(j)
					U.im(i)(j) = U.im(i)(j + 1)
					U.im(i)(j + 1) = t
				}
				for (i <- 0 until X.nc) {
					t = V.re(i)(j)
					V.re(i)(j) = V.re(i)(j + 1)
					V.re(i)(j + 1) = t
					t = V.im(i)(j)
					V.im(i)(j) = V.im(i)(j + 1)
					V.im(i)(j + 1) = t
				}
			}
		}
	}

	S.re = d
}

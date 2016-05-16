package JampackNew

import scala.util.control.Breaks

object Schur {

	var MAXITER: Int = 30
}

class Schur(A: Zmat) {

	val H = new Zhess(A)

	var T: Zutmat = new Zutmat(H.H)

	var U: Zmat = H.U

	var i: Int = 0
	var il: Int = 0
	var iter: Int = 0
	var iu: Int = 0
	var k: Int = 0

	var d: Double = 0.0
	var sd: Double = 0.0
	var sf: Double = 0.0

	var b: Z = 0
	var c: Z = 0
	var disc: Z = 0
	var kappa: Z = 0
	var p: Z = null
	var q: Z = null
	var r: Z = null
	var r1: Z = 0
	var r2: Z = 0
	var s: Z = null
	var z1: Z = 0
	var z2: Z = 0

	val P = new Rot()

	if (A.nr != A.nc) {
		throw new JampackException("Nonsquare matrix")
	}

	iu = T.rx

	iter = 0

	val outer = new Breaks
	val inner = new Breaks

	outer.breakable {
		while (true) {
			inner.breakable {
				while (iu > T.bx) {
					d = Z.abs1(T.get(iu, iu)) + Z.abs1(T.get(iu - 1, iu - 1))
					sd = Z.abs1(T.get(iu, iu - 1))
					if (sd >= 1.0e-16 * d) inner.break
					T.put(iu, iu - 1, Z.ZERO)
					iter = 0
					iu = iu - 1
				}
			}
			if (iu == T.bx) outer.break
			iter = iter + 1
			if (iter >= Schur.MAXITER) {
				throw new JampackException("Maximum number of iterations exceeded.")
			}
			il = iu - 1
			inner.breakable {
				while (il > T.bx) {
					d = Z.abs1(T.get(il, il)) + Z.abs1(T.get(il - 1, il - 1))
					sd = Z.abs1(T.get(il, il - 1))
					if (sd < 1.0e-16 * d) inner.break
					il = il - 1
				}
			}
			if (il != T.bx) {
				T.put(il, il - 1, Z.ZERO)
			}
			p = T.get(iu - 1, iu - 1)
			q = T.get(iu - 1, iu)
			r = T.get(iu, iu - 1)
			s = T.get(iu, iu)
			sf = Z.abs1(p) + Z.abs1(q) + Z.abs1(r) + Z.abs1(s)
			p /= sf
			q /= sf
			r /= sf
			s /= sf
			z1 = p * s
			z2 = r * q
			c = z1 - z2
			b = p + s
			z1 = b * b
			z2 = 4 * c
			disc = z1 - z2
			disc = disc.sqrt
			r1 = b + disc
			r2 = b - disc
			r1 = r1 / 2
			r2 = r2 / 2
			if (Z.abs1(r1) > Z.abs1(r2)) {
				r2 = c / r1
			}
			else {
				r1 = c / r2
			}
			z1 = r1 - s
			z2 = r2 - s
			if (Z.abs1(z1) < Z.abs1(z2)) {
				kappa = sf * r1
			}
			else {
				kappa = sf * r2
			}
			p = T.get(il, il) - kappa
			q = T.get(il + 1, il)
			Rot.genc(p.re, p.im, q.re, q.im, P)
			i = il
			while (i < iu) {
				Rot.pa(P, T, i, i + 1, i, T.cx)
				Rot.aph(T, P, T.bx, Math.min(i + 2, iu), i, i + 1)
				Rot.aph(U, P, U.bx, U.rx, i, i + 1)
				if (i != iu - 1) {
					Rot.genc(T, i + 1, i + 2, i, P)
				}
				i += 1
			}
		}
	}
}

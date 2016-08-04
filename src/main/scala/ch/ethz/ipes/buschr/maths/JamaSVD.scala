package ch.ethz.ipes.buschr.maths

import JampackNew.{Zdiagmat, Zmat}

import scala.util.control.Breaks._

/** JAMA SVD algorithm, adapted for scala. Only accepts real matrices. */
class JamaSVD(Arg: Zmat) {
	require(Arg.im.forall(_.forall(_ == 0)), "Matrix must be real.")

	private val A = Arg.clone().re

	private val m: Int = Arg.nrow

	private val n: Int = Arg.ncol

	private val nu = Math.min(m, n)

	private val _U: Array[Array[Double]] = Array.ofDim[Double](m, nu)

	private val _V: Array[Array[Double]] = Array.ofDim[Double](n, n)

	private val s: Array[Double] = Array.ofDim[Double](math.min(m + 1, n))

	private val e = Array.ofDim[Double](n)

	private val work = Array.ofDim[Double](m)

	private val wantu = true

	private val wantv = true

	private val nct = Math.min(m - 1, n)

	private val nrt = Math.max(0, Math.min(n - 2, m))

	for (k <- 0 until Math.max(nct, nrt)) {
		if (k < nct) {
			s(k) = 0
			for (i <- k until m) {
				s(k) = JamaSVD.hypot(s(k), A(i)(k))
			}
			if (s(k) != 0.0) {
				if (A(k)(k) < 0.0) {
					s(k) = -s(k)
				}
				for (i <- k until m) {
					A(i)(k) /= s(k)
				}
				A(k)(k) += 1.0
			}
			s(k) = -s(k)
		}
		for (j <- k + 1 until n) {
			if ((k < nct) & (s(k) != 0.0)) {
				var t = 0d
				for (i <- k until m) {
					t += A(i)(k) * A(i)(j)
				}
				t = -t / A(k)(k)
				for (i <- k until m) {
					A(i)(j) += t * A(i)(k)
				}
			}
			e(j) = A(k)(j)
		}
		if (wantu & (k < nct)) {
			for (i <- k until m) {
				_U(i)(k) = A(i)(k)
			}
		}
		if (k < nrt) {
			e(k) = 0
			for (i <- k + 1 until n) {
				e(k) = JamaSVD.hypot(e(k), e(i))
			}
			if (e(k) != 0.0) {
				if (e(k + 1) < 0.0) {
					e(k) = -e(k)
				}
				for (i <- k + 1 until n) {
					e(i) /= e(k)
				}
				e(k + 1) += 1.0
			}
			e(k) = -e(k)
			if ((k + 1 < m) & (e(k) != 0.0)) {
				for (i <- k + 1 until m) {
					work(i) = 0.0
				}
				for (j <- k + 1 until n; i <- k + 1 until m) {
					work(i) += e(j) * A(i)(j)
				}
				for (j <- k + 1 until n) {
					val t = -e(j) / e(k + 1)
					for (i <- k + 1 until m) {
						A(i)(j) += t * work(i)
					}
				}
			}
			if (wantv) {
				for (i <- k + 1 until n) {
					_V(i)(k) = e(i)
				}
			}
		}
	}

	var p = Math.min(n, m + 1)

	if (nct < n) {
		s(nct) = A(nct)(nct)
	}

	if (m < p) {
		s(p - 1) = 0.0
	}

	if (nrt + 1 < p) {
		e(nrt) = A(nrt)(p - 1)
	}

	e(p - 1) = 0.0

	if (wantu) {
		for (j <- nct until nu) {
			for (i <- 0 until m) {
				_U(i)(j) = 0.0
			}
			_U(j)(j) = 1.0
		}
		var k = nct - 1
		while (k >= 0) {
			if (s(k) != 0.0) {
				for (j <- k + 1 until nu) {
					var t = 0d
					for (i <- k until m) {
						t += _U(i)(k) * _U(i)(j)
					}
					t = -t / _U(k)(k)
					for (i <- k until m) {
						_U(i)(j) += t * _U(i)(k)
					}
				}
				for (i <- k until m) {
					_U(i)(k) = -_U(i)(k)
				}
				_U(k)(k) = 1.0 + _U(k)(k)
				for (i <- 0 until k - 1) {
					_U(i)(k) = 0.0
				}
			}
			else {
				for (i <- 0 until m) {
					_U(i)(k) = 0.0
				}
				_U(k)(k) = 1.0
			}
			k -= 1
		}
	}

	if (wantv) {
		var k = n - 1
		while (k >= 0) {
			if ((k < nrt) & (e(k) != 0.0)) {
				for (j <- k + 1 until nu) {
					var t = 0d
					for (i <- k + 1 until n) {
						t += _V(i)(k) * _V(i)(j)
					}
					t = -t / _V(k + 1)(k)
					for (i <- k + 1 until n) {
						_V(i)(j) += t * _V(i)(k)
					}
				}
			}
			for (i <- 0 until n) {
				_V(i)(k) = 0.0
			}
			_V(k)(k) = 1.0
			k -= 1
		}
	}

	val pp = p - 1

	var iter = 0

	val eps = Math.pow(2.0, -52.0)

	val tiny = Math.pow(2.0, -966.0)

	while (p > 0) {
		var k: Int = 0
		var kase: Int = 0
		k = p - 2
		breakable {
			while (k >= -1) {
				if (k == -1) {
					break
				}
				if (Math.abs(e(k)) <= tiny + eps * (Math.abs(s(k)) + Math.abs(s(k + 1)))) {
					e(k) = 0.0
					break
				}
				k -= 1
			}
		}
		if (k == p - 2) {
			kase = 4
		}
		else {
			var ks: Int = 0
			ks = p - 1
			breakable {
				while (ks >= k) {
					if (ks == k) {
						break
					}
					val t = (if (ks != p) Math.abs(e(ks)) else 0) + (if (ks != k + 1) Math.abs(e(ks - 1)) else 0)
					if (Math.abs(s(ks)) <= tiny + eps * t) {
						s(ks) = 0.0
						break
					}
					ks -= 1
				}
			}
			if (ks == k) {
				kase = 3
			}
			else if (ks == p - 1) {
				kase = 1
			}
			else {
				kase = 2
				k = ks
			}
		}
		k += 1
		kase match {
			case 1 =>
				var f = e(p - 2)
				e(p - 2) = 0.0
				var j = p - 2
				while (j >= k) {
					var t = JamaSVD.hypot(s(j), f)
					val cs = s(j) / t
					val sn = f / t
					s(j) = t
					if (j != k) {
						f = -sn * e(j - 1)
						e(j - 1) = cs * e(j - 1)
					}
					if (wantv) {
						for (i <- 0 until n) {
							t = cs * _V(i)(j) + sn * _V(i)(p - 1)
							_V(i)(p - 1) = -sn * _V(i)(j) + cs * _V(i)(p - 1)
							_V(i)(j) = t
						}
					}
					j -= 1
				}
			case 2 =>
				var f = e(k - 1)
				e(k - 1) = 0.0
				for (j <- k until p) {
					var t = JamaSVD.hypot(s(j), f)
					val cs = s(j) / t
					val sn = f / t
					s(j) = t
					f = -sn * e(j)
					e(j) = cs * e(j)
					if (wantu) {
						for (i <- 0 until m) {
							t = cs * _U(i)(j) + sn * _U(i)(k - 1)
							_U(i)(k - 1) = -sn * _U(i)(j) + cs * _U(i)(k - 1)
							_U(i)(j) = t
						}
					}
				}
			case 3 =>
				val scale = Math.max(Math.max(Math.max(Math.max(Math.abs(s(p - 1)), Math.abs(s(p - 2))), Math.abs(e(p - 2))),
					Math.abs(s(k))), Math.abs(e(k)))
				val sp = s(p - 1) / scale
				val spm1 = s(p - 2) / scale
				val epm1 = e(p - 2) / scale
				val sk = s(k) / scale
				val ek = e(k) / scale
				val b = ((spm1 + sp) * (spm1 - sp) + epm1 * epm1) / 2.0
				val c = (sp * epm1) * (sp * epm1)
				var shift = 0.0
				if ((b != 0.0) | (c != 0.0)) {
					shift = Math.sqrt(b * b + c)
					if (b < 0.0) {
						shift = -shift
					}
					shift = c / (b + shift)
				}
				var f = (sk + sp) * (sk - sp) + shift
				var g = sk * ek
				for (j <- k until p - 1) {
					var t = JamaSVD.hypot(f, g)
					var cs = f / t
					var sn = g / t
					if (j != k) {
						e(j - 1) = t
					}
					f = cs * s(j) + sn * e(j)
					e(j) = cs * e(j) - sn * s(j)
					g = sn * s(j + 1)
					s(j + 1) = cs * s(j + 1)
					if (wantv) {
						for (i <- 0 until n) {
							t = cs * _V(i)(j) + sn * _V(i)(j + 1)
							_V(i)(j + 1) = -sn * _V(i)(j) + cs * _V(i)(j + 1)
							_V(i)(j) = t
						}
					}
					t = JamaSVD.hypot(f, g)
					cs = f / t
					sn = g / t
					s(j) = t
					f = cs * e(j) + sn * s(j + 1)
					s(j + 1) = -sn * e(j) + cs * s(j + 1)
					g = sn * e(j + 1)
					e(j + 1) = cs * e(j + 1)
					if (wantu && (j < m - 1)) {
						for (i <- 0 until m) {
							t = cs * _U(i)(j) + sn * _U(i)(j + 1)
							_U(i)(j + 1) = -sn * _U(i)(j) + cs * _U(i)(j + 1)
							_U(i)(j) = t
						}
					}
				}
				e(p - 2) = f
				iter = iter + 1
			case 4 =>
				if (s(k) <= 0.0) {
					s(k) = if (s(k) < 0.0) -s(k) else 0.0
					if (wantv) {
						var i = 0
						while (i <= pp) {
							_V(i)(k) = -_V(i)(k)
							i += 1
						}
					}
				}
				breakable {
					while (k < pp) {
						if (s(k) >= s(k + 1)) {
							break
						}
						var t = s(k)
						s(k) = s(k + 1)
						s(k + 1) = t
						if (wantv && (k < n - 1)) {
							for (i <- 0 until n) {
								t = _V(i)(k + 1)
								_V(i)(k + 1) = _V(i)(k)
								_V(i)(k) = t
							}
						}
						if (wantu && (k < m - 1)) {
							for (i <- 0 until m) {
								t = _U(i)(k + 1)
								_U(i)(k + 1) = _U(i)(k)
								_U(i)(k) = t
							}
						}
						k += 1
					}
				}
				iter = 0
				p -= 1
		}
	}

	def U: Zmat = new Zmat(_U, Array.fill(_U.length, _U.head.length)(0d))

	def V: Zmat = new Zmat(_V, Array.fill(n, n)(0d)).transpose.conj

	def singularValues(): Array[Double] = s

	def S: Zdiagmat = new Zdiagmat(s, Array.fill(s.length)(0d))

	def norm2(): Double = s(0)

	def cond(): Double = s(0) / s(Math.min(m, n) - 1)

	def rank(): Int = {
		val eps = Math.pow(2.0, -52.0)
		val tol = Math.max(m, n) * s(0) * eps
		var r = 0
		for (i <- s.indices) {
			if (s(i) > tol) {
				r += 1
			}
		}
		r
	}
}

private object JamaSVD {

	def hypot(a: Double, b: Double): Double = {
		var r = 0d
		if (Math.abs(a) > Math.abs(b)) {
			r = b / a
			r = Math.abs(a) * Math.sqrt(1 + r * r)
		}
		else if (b != 0) {
			r = a / b
			r = Math.abs(b) * Math.sqrt(1 + r * r)
		}
		else {
			r = 0.0
		}
		r
	}
}

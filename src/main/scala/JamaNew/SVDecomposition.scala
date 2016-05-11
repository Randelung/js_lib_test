package JamaNew

import JamaNew.util.Maths

@SerialVersionUID(1)
class SVDecomposition(Arg: Matrix) extends java.io.Serializable {

	private val m: Int = Arg.getRowDimension

	private val n: Int = Arg.getColumnDimension

	val A = Arg.getArrayCopy

	val nu = Math.min(m, n)

	private val U: Array[Array[Double]] = Array.ofDim[Double](m, nu)

	private val V: Array[Array[Double]] = Array.ofDim[Double](n, n)

	private val s: Array[Double] = Array.ofDim[Double](Math.min(m + 1, n))

	val e = Array.ofDim[Double](n)

	val work = Array.ofDim[Double](m)

	val wantu = true

	val wantv = true

	val nct = Math.min(m - 1, n)

	val nrt = Math.max(0, Math.min(n - 2, m))

	for (k <- 0 until Math.max(nct, nrt)) {
		if (k < nct) {
			s(k) = 0
			for (i <- k until m) {
				s(k) = Maths.hypot(s(k), A(i)(k))
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
				U(i)(k) = A(i)(k)
			}
		}
		if (k < nrt) {
			e(k) = 0
			for (i <- k + 1 until n) {
				e(k) = Maths.hypot(e(k), e(i))
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
					V(i)(k) = e(i)
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
				U(i)(j) = 0.0
			}
			U(j)(j) = 1.0
		}
		var k = nct - 1
		while (k >= 0) {
			if (s(k) != 0.0) {
				for (j <- k + 1 until nu) {
					var t = 0d
					for (i <- k until m) {
						t += U(i)(k) * U(i)(j)
					}
					t = -t / U(k)(k)
					for (i <- k until m) {
						U(i)(j) += t * U(i)(k)
					}
				}
				for (i <- k until m) {
					U(i)(k) = -U(i)(k)
				}
				U(k)(k) = 1.0 + U(k)(k)
				for (i <- 0 until k - 1) {
					U(i)(k) = 0.0
				}
			}
			else {
				for (i <- 0 until m) {
					U(i)(k) = 0.0
				}
				U(k)(k) = 1.0
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
						t += V(i)(k) * V(i)(j)
					}
					t = -t / V(k + 1)(k)
					for (i <- k + 1 until n) {
						V(i)(j) += t * V(i)(k)
					}
				}
			}
			for (i <- 0 until n) {
				V(i)(k) = 0.0
			}
			V(k)(k) = 1.0
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
		while (k >= -1) {
			if (k == -1) {
				//break
			}
			if (Math.abs(e(k)) <= tiny + eps * (Math.abs(s(k)) + Math.abs(s(k + 1)))) {
				e(k) = 0.0
				//break
			}
			k -= 1
		}
		if (k == p - 2) {
			kase = 4
		}
		else {
			var ks: Int = 0
			ks = p - 1
			while (ks >= k) {
				if (ks == k) {
					//break
				}
				val t = (if (ks != p) Math.abs(e(ks)) else 0d) + (if (ks != k + 1) Math.abs(e(ks - 1)) else 0d)
				if (Math.abs(s(ks)) <= tiny + eps * t) {
					s(ks) = 0.0
					//break
				}
				ks -= 1
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
					var t = Maths.hypot(s(j), f)
					val cs = s(j) / t
					val sn = f / t
					s(j) = t
					if (j != k) {
						f = -sn * e(j - 1)
						e(j - 1) = cs * e(j - 1)
					}
					if (wantv) {
						for (i <- 0 until n) {
							t = cs * V(i)(j) + sn * V(i)(p - 1)
							V(i)(p - 1) = -sn * V(i)(j) + cs * V(i)(p - 1)
							V(i)(j) = t
						}
					}
					j -= 1
				}
			case 2 =>
				var f = e(k - 1)
				e(k - 1) = 0.0
				for (j <- k until p) {
					var t = Maths.hypot(s(j), f)
					val cs = s(j) / t
					val sn = f / t
					s(j) = t
					f = -sn * e(j)
					e(j) = cs * e(j)
					if (wantu) {
						for (i <- 0 until m) {
							t = cs * U(i)(j) + sn * U(i)(k - 1)
							U(i)(k - 1) = -sn * U(i)(j) + cs * U(i)(k - 1)
							U(i)(j) = t
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
					var t = Maths.hypot(f, g)
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
							t = cs * V(i)(j) + sn * V(i)(j + 1)
							V(i)(j + 1) = -sn * V(i)(j) + cs * V(i)(j + 1)
							V(i)(j) = t
						}
					}
					t = Maths.hypot(f, g)
					cs = f / t
					sn = g / t
					s(j) = t
					f = cs * e(j) + sn * s(j + 1)
					s(j + 1) = -sn * e(j) + cs * s(j + 1)
					g = sn * e(j + 1)
					e(j + 1) = cs * e(j + 1)
					if (wantu && (j < m - 1)) {
						for (i <- 0 until m) {
							t = cs * U(i)(j) + sn * U(i)(j + 1)
							U(i)(j + 1) = -sn * U(i)(j) + cs * U(i)(j + 1)
							U(i)(j) = t
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
							V(i)(k) = -V(i)(k)
							i += 1
						}
					}
				}
				while (k < pp) {
					if (s(k) >= s(k + 1)) {
						//break
					}
					var t = s(k)
					s(k) = s(k + 1)
					s(k + 1) = t
					if (wantv && (k < n - 1)) {
						for (i <- 0 until n) {
							t = V(i)(k + 1)
							V(i)(k + 1) = V(i)(k)
							V(i)(k) = t
						}
					}
					if (wantu && (k < m - 1)) {
						for (i <- 0 until m) {
							t = U(i)(k + 1)
							U(i)(k + 1) = U(i)(k)
							U(i)(k) = t
						}
					}
					k += 1
				}
				iter = 0
				p -= 1
		}
	}

	def getU: Matrix = new Matrix(U, m, Math.min(m + 1, n))

	def getV: Matrix = new Matrix(V, n, n)

	def getSingularValues: Array[Double] = s

	def getS: Matrix = {
		val X = new Matrix(n, n)
		val S = X.getArray
		for (i <- 0 until n) {
			for (j <- 0 until n) {
				S(i)(j) = 0.0
			}
			S(i)(i) = this.s(i)
		}
		X
	}

	def norm2(): Double = s(0)

	def cond(): Double = s(0) / s(Math.min(m, n) - 1)

	def rank(): Int = {
		val eps = Math.pow(2.0, -52.0)
		val tol = Math.max(m, n) * s(0) * eps
		var r = 0
		for (i <- s.indices if s(i) > tol) {
			r += 1
		}
		r
	}
}

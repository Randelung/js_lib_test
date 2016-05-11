package JamaNew

import JamaNew.util.Maths

import scala.util.control.Breaks._

@SerialVersionUID(1)
class EigenvalueDecomposition(Arg: Matrix) extends java.io.Serializable {

	private val n: Int = Arg.getColumnDimension

	private var issymmetric: Boolean = true

	private val d: Array[Double] = Array.ofDim[Double](n)

	private val e: Array[Double] = Array.ofDim[Double](n)

	private val V: Array[Array[Double]] = Array.ofDim[Double](n, n)

	private var H: Array[Array[Double]] = _

	private var ort: Array[Double] = _

	val A = Arg.getArray

	var j = 0
	while ((j < n) & issymmetric) {
		var i = 0
		while ((i < n) & issymmetric) {
			issymmetric = A(i)(j) == A(j)(i)
			i += 1
		}
		j += 1
	}

	if (issymmetric) {
		for (i <- 0 until n; j <- 0 until n) {
			V(i)(j) = A(i)(j)
		}
		tred2()
		tql2()
	}
	else {
		H = Array.ofDim[Double](n, n)
		ort = Array.ofDim[Double](n)
		for (j <- 0 until n; i <- 0 until n) {
			H(i)(j) = A(i)(j)
		}
		orthes()
		hqr2()
	}

	private def tred2() {
		for (j <- 0 until n) {
			d(j) = V(n - 1)(j)
		}
		for (i <- n - 1 until 0 by -1) {
			var scale = 0d
			var h = 0d
			for (k <- 0 until i) {
				scale += math.abs(d(k))
			}
			if (scale == 0d) {
				e(i) = d(i - 1)
				for (j <- 0 until i) {
					d(j) = V(i - 1)(j)
					V(i)(j) = 0d
					V(j)(i) = 0d
				}
			}
			else {
				for (k <- 0 until i) {
					d(k) /= scale
					h += d(k) * d(k)
				}
				var f = d(i - 1)
				var g = math.sqrt(h)
				if (f > 0) {
					g = -g
				}
				e(i) = scale * g
				h = h - f * g
				d(i - 1) = f - g
				for (j <- 0 until i) {
					e(j) = 0d
				}
				for (j <- 0 until i) {
					f = d(j)
					V(j)(i) = f
					g = e(j) + V(j)(j) * f
					for (k <- j + 1 until i) {
						g += V(k)(j) * d(k)
						e(k) += V(k)(j) * f
					}
					e(j) = g
				}
				f = 0d
				for (j <- 0 until i) {
					e(j) /= h
					f += e(j) * d(j)
				}
				val hh = f / (h + h)
				for (j <- 0 until i) {
					e(j) -= hh * d(j)
				}
				for (j <- 0 until i) {
					f = d(j)
					g = e(j)
					for (k <- j until i) {
						V(k)(j) -= f * e(k) + g * d(k)
					}
					d(j) = V(i - 1)(j)
					V(i)(j) = 0d
				}
			}
			d(i) = h
		}
		for (i <- 0 until n - 1) {
			V(n - 1)(i) = V(i)(i)
			V(i)(i) = 1d
			val h = d(i + 1)
			if (h != 0d) {
				for (k <- 0 to i) {
					d(k) = V(k)(i + 1) / h
				}
				for (j <- 0 to i) {
					var g = 0d
					for (k <- 0 to i) {
						g += V(k)(i + 1) * V(k)(j)
					}
					for (k <- 0 to i) {
						V(k)(j) -= g * d(k)
					}
				}
			}
			for (k <- 0 to i) {
				V(k)(i + 1) = 0d
			}
		}
		for (j <- 0 until n) {
			d(j) = V(n - 1)(j)
			V(n - 1)(j) = 0d
		}
		V(n - 1)(n - 1) = 1d
		e(0) = 0d
	}

	private def tql2() {
		for (i <- 1 until n) {
			e(i - 1) = e(i)
		}
		e(n - 1) = 0d
		var f = 0d
		var tst1 = 0d
		val eps = math.pow(2d, -52d)
		for (l <- 0 until n) {
			tst1 = math.max(tst1, math.abs(d(l)) + math.abs(e(l)))
			var m = l
			breakable {
				while (m < n) {
					if (math.abs(e(m)) <= eps * tst1) {
						break
					}
					m += 1
				}
			}

			if (m > l) {
				var iter = 0
				do {
					iter += 1
					var g = d(l)
					var p = (d(l + 1) - g) / (2d * e(l))
					var r = Maths.hypot(p, 1d)
					if (p < 0) {
						r = -r
					}
					d(l) = e(l) / (p + r)
					d(l + 1) = e(l) * (p + r)
					val dl1 = d(l + 1)
					var h = g - d(l)
					for (i <- l + 2 until n) {
						d(i) -= h
					}
					f = f + h
					p = d(m)
					var c = 1d
					var c2 = c
					var c3 = c
					val el1 = e(l + 1)
					var s = 0d
					var s2 = 0d
					for (i <- m - 1 to 1 by -1) {
						c3 = c2
						c2 = c
						s2 = s
						g = c * e(i)
						h = c * p
						r = Maths.hypot(p, e(i))
						e(i + 1) = s * r
						s = e(i) / r
						c = p / r
						p = c * d(i) - s * g
						d(i + 1) = h + s * (c * g + s * d(i))
						for (k <- 0 until n) {
							h = V(k)(i + 1)
							V(k)(i + 1) = s * V(k)(i) + c * h
							V(k)(i) = c * V(k)(i) - s * h
						}
					}
					p = -s * s2 * c3 * el1 * e(l) / dl1
					e(l) = s * p
					d(l) = c * p
				}
				while (math.abs(e(l)) > eps * tst1)
			}
			d(l) = d(l) + f
			e(l) = 0d
		}
		for (i <- 0 until n - 1) {
			var k = i
			var p = d(i)
			for (j <- i + 1 until n) {
				if (d(j) < p) {
					k = j
					p = d(j)
				}
			}
			if (k != i) {
				d(k) = d(i)
				d(i) = p
				for (j <- 0 until n) {
					p = V(j)(i)
					V(j)(i) = V(j)(k)
					V(j)(k) = p
				}
			}
		}
	}

	private def orthes() {
		val low = 0
		val high = n - 1
		for (m <- low + 1 until high) {
			var scale = 0d
			for (i <- m to high) {
				scale += math.abs(H(i)(m - 1))
			}
			if (scale != 0d) {
				var h = 0d
				for (i <- high to m by -1) {
					ort(i) = H(i)(m - 1) / scale
					h += ort(i) * ort(i)
				}
				var g = math.sqrt(h)
				if (ort(m) > 0) {
					g = -g
				}
				h = h - ort(m) * g
				ort(m) = ort(m) - g
				for (j <- m until n) {
					var f = 0d
					for (i <- high to m by -1) {
						f += ort(i) * H(i)(j)
					}
					f = f / h
					for (i <- m to high) {
						H(i)(j) -= f * ort(i)
					}
				}
				for (i <- 0 to high) {
					var f = 0d
					for (j <- high to m by -1) {
						f += ort(j) * H(i)(j)
					}
					f = f / h
					for (j <- m to high) {
						H(i)(j) -= f * ort(j)
					}
				}
				ort(m) = scale * ort(m)
				H(m)(m - 1) = scale * g
			}
		}
		for (i <- 0 until n; j <- 0 until n) {
			V(i)(j) = if (i == j) 1d else 0d
		}
		for (m <- high - 1 until low by -1) {
			if (H(m)(m - 1) != 0d) {
				for (i <- m + 1 to high) {
					ort(i) = H(i)(m - 1)
				}
				for (j <- m to high) {
					var g = 0d
					for (i <- m to high) {
						g += ort(i) * V(i)(j)
					}
					g = (g / ort(m)) / H(m)(m - 1)
					for (i <- m to high) {
						V(i)(j) += g * ort(i)
					}
				}
			}
		}
	}

	@transient private var cdivr: Double = _

	@transient private var cdivi: Double = _

	private def cdiv(xr: Double,
					 xi: Double,
					 yr: Double,
					 yi: Double) {
		var r: Double = 0d
		var d: Double = 0d
		if (math.abs(yr) > math.abs(yi)) {
			r = yi / yr
			d = yr + r * yi
			cdivr = (xr + r * xi) / d
			cdivi = (xi - r * xr) / d
		}
		else {
			r = yr / yi
			d = yi + r * yr
			cdivr = (r * xr + xi) / d
			cdivi = (r * xi - xr) / d
		}
	}

	private def hqr2() {
		val nn = this.n
		var n = nn - 1
		val low = 0
		val high = nn - 1
		val eps = math.pow(2, -52)
		var exshift = 0d
		var p = 0d
		var q = 0d
		var r = 0d
		var s = 0d
		var z = 0d
		var t = 0d
		var w = 0d
		var x = 0d
		var y = 0d
		var norm = 0d
		for (i <- 0 until nn) {
			if (i < low | i > high) {
				d(i) = H(i)(i)
				e(i) = 0d
			}
			for (j <- math.max(i - 1, 0) until nn) {
				norm += math.abs(H(i)(j))
			}
		}
		var iter = 0
		while (n >= low) {
			var l = n
			breakable {
				while (l > low) {
					s = math.abs(H(l - 1)(l - 1)) + math.abs(H(l)(l))
					if (s == 0d) {
						s = norm
					}
					if (math.abs(H(l)(l - 1)) < eps * s) {
						break
					}
					l -= 1
				}
			}
			if (l == n) {
				H(n)(n) = H(n)(n) + exshift
				d(n) = H(n)(n)
				e(n) = 0d
				n -= 1
				iter = 0
			}
			else if (l == n - 1) {
				w = H(n)(n - 1) * H(n - 1)(n)
				p = (H(n - 1)(n - 1) - H(n)(n)) / 2d
				q = p * p + w
				z = math.sqrt(math.abs(q))
				H(n)(n) += exshift
				H(n - 1)(n - 1) += exshift
				x = H(n)(n)
				if (q >= 0) {
					z = p + (if (p >= 0) z else -z)
					d(n - 1) = x + z
					d(n) = d(n - 1)
					if (z != 0d) {
						d(n) = x - w / z
					}
					e(n - 1) = 0d
					e(n) = 0d
					x = H(n)(n - 1)
					s = math.abs(x) + math.abs(z)
					p = x / s
					q = z / s
					r = math.sqrt(p * p + q * q)
					p = p / r
					q = q / r
					for (j <- n - 1 until nn) {
						z = H(n - 1)(j)
						H(n - 1)(j) = q * z + p * H(n)(j)
						H(n)(j) = q * H(n)(j) - p * z
					}
					for (i <- 0 to n) {
						z = H(i)(n - 1)
						H(i)(n - 1) = q * z + p * H(i)(n)
						H(i)(n) = q * H(i)(n) - p * z
					}
					for (i <- low to high) {
						z = V(i)(n - 1)
						V(i)(n - 1) = q * z + p * V(i)(n)
						V(i)(n) = q * V(i)(n) - p * z
					}
				}
				else {
					d(n - 1) = x + p
					d(n) = x + p
					e(n - 1) = z
					e(n) = -z
				}
				n = n - 2
				iter = 0
			}
			else {
				x = H(n)(n)
				y = 0d
				w = 0d
				if (l < n) {
					y = H(n - 1)(n - 1)
					w = H(n)(n - 1) * H(n - 1)(n)
				}
				if (iter == 10) {
					exshift += x
					for (i <- low to n) {
						H(i)(i) -= x
					}
					s = math.abs(H(n)(n - 1)) + math.abs(H(n - 1)(n - 2))
					y = 0.75 * s
					x = y
					w = -0.4375 * s * s
				}
				if (iter == 30) {
					s = (y - x) / 2d
					s = s * s + w
					if (s > 0) {
						s = math.sqrt(s)
						if (y < x) {
							s = -s
						}
						s = x - w / ((y - x) / 2d + s)
						for (i <- low to n) {
							H(i)(i) -= s
						}
						exshift += s
						w = 0.964
						y = w
						x = y
					}
				}
				iter += 1
				var m = n - 2
				breakable {
					while (m >= l) {
						z = H(m)(m)
						r = x - z
						s = y - z
						p = (r * s - w) / H(m + 1)(m) + H(m)(m + 1)
						q = H(m + 1)(m + 1) - z - r - s
						r = H(m + 2)(m + 1)
						s = math.abs(p) + math.abs(q) + math.abs(r)
						p = p / s
						q = q / s
						r = r / s
						if (m == l) {
							break
						}
						if (math.abs(H(m)(m - 1)) * (math.abs(q) + math.abs(r)) <
							eps * (math.abs(p) * (math.abs(H(m - 1)(m - 1)) + math.abs(z) +
								math.abs(H(m + 1)(m + 1))))) {
							break
						}
						m -= 1
					}
				}
				for (i <- m + 2 to n) {
					H(i)(i - 2) = 0d
					if (i > m + 2) {
						H(i)(i - 3) = 0d
					}
				}
				for (k <- m until n) {
					val notlast = k != n - 1
					var x = 0d
					if (k != m) {
						p = H(k)(k - 1)
						q = H(k + 1)(k - 1)
						r = if (notlast) H(k + 2)(k - 1) else 0d
						x = math.abs(p) + math.abs(q) + math.abs(r)
					}
					if (k == m || x != 0d) {
						if (k != m) {
							p = p / x
							q = q / x
							r = r / x
						}
						s = math.sqrt(p * p + q * q + r * r)
						if (p < 0) {
							s = -s
						}
						if (s != 0) {
							if (k != m) {
								H(k)(k - 1) = -s * x
							}
							else if (l != m) {
								H(k)(k - 1) *= -1
							}
							p = p + s
							x = p / s
							y = q / s
							z = r / s
							q = q / p
							r = r / p
							for (j <- k until nn) {
								p = H(k)(j) + q * H(k + 1)(j)
								if (notlast) {
									p += r * H(k + 2)(j)
									H(k + 2)(j) -= p * z
								}
								H(k)(j) -= p * x
								H(k + 1)(j) -= p * y
							}
							for (i <- 0 to math.min(n, k + 3)) {
								p = x * H(i)(k) + y * H(i)(k + 1)
								if (notlast) {
									p += z * H(i)(k + 2)
									H(i)(k + 2) -= p * r
								}
								H(i)(k) -= p
								H(i)(k + 1) -= p * q
							}
							for (i <- low to high) {
								p = x * V(i)(k) + y * V(i)(k + 1)
								if (notlast) {
									p += z * V(i)(k + 2)
									V(i)(k + 2) -= p * r
								}
								V(i)(k) -= p
								V(i)(k + 1) -= p * q
							}
						}
					}
				}
			}
		}
		if (norm == 0d) {
			return
		}
		for (n <- nn - 1 to 0 by -1) {
			p = d(n)
			q = e(n)
			if (q == 0) {
				var l = n
				H(n)(n) = 1d
				for (i <- n - 1 to 0 by -1) {
					w = H(i)(i) - p
					r = 0d
					for (j <- l to n) {
						r += H(i)(j) * H(j)(n)
					}
					if (e(i) < 0d) {
						z = w
						s = r
					}
					else {
						l = i
						if (e(i) == 0d) {
							H(i)(n) = -r / (if (w != 0d) w else eps * norm)
						}
						else {
							x = H(i)(i + 1)
							y = H(i + 1)(i)
							q = (d(i) - p) * (d(i) - p) + e(i) * e(i)
							t = (x * s - z * r) / q
							H(i)(n) = t
							H(i + 1)(n) = if (math.abs(x) > math.abs(z)) (-r - w * t) / x else (-s - y * t) / z
						}
						t = math.abs(H(i)(n))
						if ((eps * t) * t > 1) {
							for (j <- i to n) {
								H(j)(n) = H(j)(n) / t
							}
						}
					}
				}
			}
			else if (q < 0) {
				var l = n - 1
				if (math.abs(H(n)(n - 1)) > math.abs(H(n - 1)(n))) {
					H(n - 1)(n - 1) = q / H(n)(n - 1)
					H(n - 1)(n) = -(H(n)(n) - p) / H(n)(n - 1)
				}
				else {
					cdiv(0d, -H(n - 1)(n), H(n - 1)(n - 1) - p, q)
					H(n - 1)(n - 1) = cdivr
					H(n - 1)(n) = cdivi
				}
				H(n)(n - 1) = 0d
				H(n)(n) = 1d
				for (i <- n - 2 to 0 by -1) {
					var ra = 0d
					var sa = 0d
					var vr = 0d
					var vi = 0d
					ra = 0d
					sa = 0d
					for (j <- 1 to n) {
						ra = ra + H(i)(j) * H(j)(n - 1)
						sa = sa + H(i)(j) * H(j)(n)
					}
					w = H(i)(i) - p
					if (e(i) < 0d) {
						z = w
						r = ra
						s = sa
					}
					else {
						l = i
						if (e(i) == 0) {
							cdiv(-ra, -sa, w, q)
							H(i)(n - 1) = cdivr
							H(i)(n) = cdivi
						}
						else {
							x = H(i)(i + 1)
							y = H(i + 1)(i)
							vr = (d(i) - p) * (d(i) - p) + e(i) * e(i) - q * q
							vi = (d(i) - p) * 2d * q
							if (vr == 0d & vi == 0d) {
								vr = eps * norm *
									(math.abs(w) + math.abs(q) + math.abs(x) + math.abs(y) +
										math.abs(z))
							}
							cdiv(x * r - z * ra + q * sa, x * s - z * sa - q * ra, vr, vi)
							H(i)(n - 1) = cdivr
							H(i)(n) = cdivi
							if (math.abs(x) > (math.abs(z) + math.abs(q))) {
								H(i + 1)(n - 1) = (-ra - w * H(i)(n - 1) + q * H(i)(n)) / x
								H(i + 1)(n) = (-sa - w * H(i)(n) - q * H(i)(n - 1)) / x
							}
							else {
								cdiv(-r - y * H(i)(n - 1), -s - y * H(i)(n), z, q)
								H(i + 1)(n - 1) = cdivr
								H(i + 1)(n) = cdivi
							}
						}
						t = math.max(math.abs(H(i)(n - 1)), math.abs(H(i)(n)))
						if ((eps * t) * t > 1) {
							for (j <- i to n) {
								H(j)(n - 1) = H(j)(n - 1) / t
								H(j)(n) = H(j)(n) / t
							}
						}
					}
				}
			}
		}
		for (i <- 0 until nn) {
			if (i < low | i > high) {
				for (j <- i until nn) {
					V(i)(j) = H(i)(j)
				}
			}
		}
		for (j <- nn - 1 to low by -1; i <- low to high) {
			z = 0d
			for (k <- low to math.min(j, high)) {
				z += V(i)(k) * H(k)(j)
			}
			V(i)(j) = z
		}
	}

	def getV: Matrix = new Matrix(V, n, n)

	def getRealEigenvalues: Array[Double] = d

	def getImagEigenvalues: Array[Double] = e

	def getD: Matrix = {
		val X = new Matrix(n, n)
		val D = X.getArray
		for (i <- 0 until n) {
			for (j <- 0 until n) {
				D(i)(j) = 0d
			}
			D(i)(i) = d(i)
			if (e(i) > 0) {
				D(i)(i + 1) = e(i)
			}
			else if (e(i) < 0) {
				D(i)(i - 1) = e(i)
			}
		}
		X
	}
}

package JampackNew

import scala.util.control.Breaks._

object Print {

	def o(k: Int): String = {
		o(k, Parameters.OutputFieldWidth)
	}

	def o(k: Int, w: Int): String = {
		var stringBuilder = new StringBuilder
		stringBuilder += '\n'
		var num = k.toString
		while (num.length < w) num = " " + num
		stringBuilder ++= num
		stringBuilder += '\n'
		stringBuilder.toString
	}

	def o(ia: Array[Int]): String = {
		o(ia, Parameters.OutputFieldWidth)
	}

	def o(ia: Array[Int], w: Int): String = {
		var stringBuilder = new StringBuilder
		val n = ia.length
		val ncp = Parameters.PageWidth / w
		var jl = 0
		while (jl < n) {
			val ju = Math.min(n, jl + ncp)
			stringBuilder += '\n'
			for (j <- jl until ju) {
				var num = ia(j).toString
				while (num.length < w) num = " " + num
				stringBuilder ++= num
			}
			jl = jl + ncp
		}
		stringBuilder += '\n'
		stringBuilder.toString
	}

	def o(a: Double): String = {
		o(a, Parameters.OutputFieldWidth, Parameters.OutputFracPlaces)
	}

	def o(a: Double, w: Int, d: Int): String = {
		var stringBuilder = new StringBuilder
		stringBuilder += '\n'
		stringBuilder ++= DoubletoEstring(a, w, d)
		stringBuilder += '\n'
		stringBuilder.toString
	}

	def o(a: Array[Double]): String = {
		o(a, Parameters.OutputFieldWidth, Parameters.OutputFracPlaces)
	}

	def o(a: Array[Double], w: Int, d: Int): String = {
		var stringBuilder = new StringBuilder
		val nc = a.length
		val ncp = Parameters.PageWidth / w
		var jl = 0
		while (jl < nc) {
			val ju = Math.min(nc, jl + ncp)
			stringBuilder += '\n'
			for (j <- jl until ju) {
				var head = j.toString
				while (head.length < w) head = " " + head
				stringBuilder ++= head
			}
			stringBuilder += '\n'
			for (j <- jl until ju) stringBuilder ++= DoubletoEstring(a(j), w, d)
			stringBuilder += '\n'
			jl = jl + ncp
		}
		stringBuilder.toString
	}

	def o(A: Array[Array[Double]]): String = {
		o(A, Parameters.OutputFieldWidth, Parameters.OutputFracPlaces)
	}

	def o(A: Array[Array[Double]], w: Int, d: Int): String = {
		var stringBuilder = new StringBuilder
		val nr = A.length
		val nc = A(0).length
		val temp = (nr - 1).toString
		val rfw = temp.length + 1
		val ncp = (Parameters.PageWidth - rfw) / w
		var jl = 0
		while (jl < nc) {
			val ju = Math.min(nc, jl + ncp)
			stringBuilder += '\n'
			var head = ""
			while (head.length < rfw) head = head + " "
			stringBuilder ++= head
			for (j <- jl until ju) {
				head = j.toString
				while (head.length < w) head = " " + head
				stringBuilder ++= head
			}
			stringBuilder += '\n'
			for (i <- 0 until nr) {
				var row = i.toString
				while (row.length < rfw) row = " " + row
				stringBuilder ++= row
				for (j <- jl until ju) stringBuilder ++= DoubletoEstring(A(i)(j), w, d)
				stringBuilder += '\n'
			}
			jl = jl + ncp
		}
		stringBuilder.toString
	}

	def o(a: Z): String = {
		o(a, Parameters.OutputFieldWidth, Parameters.OutputFracPlaces)
	}

	def o(a: Z, w: Int, d: Int): String = {
		var stringBuilder = new StringBuilder
		stringBuilder += '\n'
		stringBuilder ++= ZtoEstring(a, w, d)
		stringBuilder += '\n'
		stringBuilder.toString
	}

	def o(a: Array[Z]): String = {
		o(a, Parameters.OutputFieldWidth, Parameters.OutputFracPlaces)
	}

	def o(a: Array[Z], w: Int, d: Int): String = {
		var stringBuilder = new StringBuilder
		val n = a.length
		val ww = w + d + 10
		val ncp = Parameters.PageWidth / ww
		var jl = 0
		while (jl < n) {
			val ju = Math.min(n, jl + ncp)
			stringBuilder += '\n'
			var head = ""
			for (j <- jl until ju) {
				head = j.toString
				while (head.length < ww) head = " " + head
				stringBuilder ++= head
			}
			stringBuilder += '\n'
			for (j <- jl until ju) {
				stringBuilder ++= ZtoEstring(a(j), w, d)
			}
			stringBuilder += '\n'
			jl = jl + ncp
		}
		stringBuilder.toString
	}

	def o(A: Array[Array[Z]]): String = {
		o(A, Parameters.OutputFieldWidth, Parameters.OutputFracPlaces)
	}

	def o(A: Array[Array[Z]], w: Int, d: Int): String = {
		var stringBuilder = new StringBuilder
		val nr = A.length
		val nc = A(0).length
		val temp = (nr - 1).toString
		val rfw = temp.length + 1
		val ww = w + d + 10
		val ncp = (Parameters.PageWidth - rfw) / ww
		var jl = 0
		while (jl < nc) {
			val ju = Math.min(nc, jl + ncp)
			stringBuilder += '\n'
			var head = ""
			while (head.length < rfw) head = head + " "
			stringBuilder ++= head
			for (j <- jl until ju) {
				head = j.toString
				while (head.length < ww) head = " " + head
				stringBuilder ++= head
			}
			stringBuilder += '\n'
			for (i <- 0 until nr) {
				var row = i.toString
				while (row.length < rfw) row = " " + row
				stringBuilder ++= row
				for (j <- jl until ju) {
					var snum = DoubletoEstring(A(i)(j).re, w, d)
					snum = if (A(i)(j).im < 0) snum + " - " + DoubletoEstring(-A(i)(j).im, d + 6, d) +
						"i"
					else snum + " + " + DoubletoEstring(A(i)(j).im, d + 6, d) +
						"i"
					stringBuilder ++= snum
				}
				stringBuilder += '\n'
			}
			jl = jl + ncp
		}
		stringBuilder.toString
	}

	def o(z: Z1): String = {
		o(z, Parameters.OutputFieldWidth, Parameters.OutputFracPlaces)
	}

	def o(z: Z1, w: Int, d: Int): String = {
		var stringBuilder = new StringBuilder
		val n = z.n
		val ww = w + d + 10
		val ncp = Parameters.PageWidth / ww
		var jl = 0
		while (jl < n) {
			val ju = Math.min(n, jl + ncp)
			stringBuilder += '\n'
			var head = ""
			for (j <- jl until ju) {
				head = j.toString
				while (head.length < ww) head = " " + head
				stringBuilder ++= head
			}
			stringBuilder += '\n'
			for (j <- jl until ju) {
				stringBuilder ++= ZtoEstring(new Z(z.re(j), z.im(j)), w, d)
			}
			stringBuilder += '\n'
			jl = jl + ncp
		}
		stringBuilder.toString
	}

	def o(A: Zmat): String = {
		o(A, Parameters.OutputFieldWidth, Parameters.OutputFracPlaces)
	}

	def o(A: Zmat, w: Int, d: Int): String = {
		var stringBuilder = new StringBuilder
		val nr = A.nrow
		val nc = A.ncol
		A.loadProperties()
		var real = true
		breakable {
			for (i <- A.baseIndex to A.rx; j <- A.baseIndex to A.cx) {
				if (A.im(i - A.baseIndex)(j - A.baseIndex) != 0) {
					real = false
					break
				}
			}
		}
		if (!real) {
			val temp = (nr + A.baseIndex - 1).toString
			val rfw = temp.length + 1
			val ww = w + d + 10
			val ncp = (Parameters.PageWidth - rfw) / ww
			var jl = 0
			while (jl < nc) {
				val ju = Math.min(nc, jl + ncp)
				stringBuilder += '\n'
				var head = ""
				while (head.length < rfw) head = head + " "
				stringBuilder ++= head
				for (j <- jl until ju) {
					head = (j + A.baseIndex).toString
					while (head.length < ww) head = " " + head
					stringBuilder ++= head
				}
				stringBuilder += '\n'
				for (i <- 0 until nr) {
					var row = (i + A.baseIndex).toString
					while (row.length < rfw) row = " " + row
					stringBuilder ++= row
					for (j <- jl until ju) {
						var snum = DoubletoEstring(A.re(i)(j), w, d)
						snum = if (A.im(i)(j) < 0) snum + " - " + DoubletoEstring(-A.im(i)(j), d + 6, d) +
							"i"
						else snum + " + " + DoubletoEstring(A.im(i)(j), d + 6, d) +
							"i"
						stringBuilder ++= snum
					}
					stringBuilder += '\n'
				}
				jl = jl + ncp
			}
		}
		else {
			val temp = A.rx.toString
			val rfw = temp.length + 1
			val ncp = (Parameters.PageWidth - rfw) / w
			var jl = A.baseIndex
			while (jl <= A.cx) {
				val ju = Math.min(A.cx, jl + ncp - 1)
				stringBuilder += '\n'
				var head = ""
				while (head.length < rfw) head = head + " "
				stringBuilder ++= head
				for (j <- jl to ju) {
					head = j.toString
					while (head.length < w) head = " " + head
					stringBuilder ++= head
				}
				stringBuilder += '\n'
				for (i <- A.baseIndex to A.rx) {
					var row = i.toString
					while (row.length < rfw) row = " " + row
					stringBuilder ++= row
					for (j <- jl to ju) {
						stringBuilder ++= DoubletoEstring(A.re(i - A.baseIndex)(j - A.baseIndex), w, d)
					}
					stringBuilder += '\n'
				}
				jl = jl + ncp
			}
		}
		stringBuilder.toString
	}

	def o(D: Zdiagmat): String = {
		o(D, Parameters.OutputFieldWidth, Parameters.OutputFracPlaces)
	}

	def o(D: Zdiagmat, w: Int, d: Int): String = {
		var stringBuilder = new StringBuilder
		val n = D.order
		val ww = w + d + 10
		val ncp = Parameters.PageWidth / ww
		var jl = 0
		while (jl < n) {
			val ju = Math.min(n, jl + ncp)
			stringBuilder += '\n'
			var head = ""
			for (j <- jl until ju) {
				head = (j + D.bx).toString
				while (head.length < ww) head = " " + head
				stringBuilder ++= head
			}
			stringBuilder += '\n'
			for (j <- jl until ju) {
				stringBuilder ++= ZtoEstring(new Z(D.re(j), D.im(j)), w, d)
			}
			stringBuilder += '\n'
			jl = jl + ncp
		}
		stringBuilder.toString
	}

	def DoubletoEstring(_num: Double, w: Int, d: Int): String = {
		var num = _num
		var minusf = false
		var minuse = false
		var snum: String = null
		if (java.lang.Double.isNaN(num)) {
			snum = "NaN"
		}
		else if (java.lang.Double.isInfinite(num)) {
			snum = "Infty"
		}
		else if (num == 0.0) {
			snum = "e+00"
			for (i <- 0 until d) snum = "0" + snum
			snum = "0." + snum
		}
		else {
			if (num < 0) {
				minusf = true
				num = -num
			}
			var exp = (Math.log(num) / Math.log(10.0)).toInt
			if (num < 1) {
				exp = exp - 1
			}
			var frac = num / Math.pow(10, exp)
			if (frac > 10 - Math.pow(10, -d)) {
				frac = frac / 10
				exp = exp + 1
			}
			var fmt = s"%.${d}f"
			val sfrac = fmt.format(frac)
			if (exp < 0) {
				minuse = true
				exp = -exp
			}
			val sexp = java.lang.Integer.toString(exp)
			snum = sexp
			if (snum.length < 2) snum = "0" + snum
			snum = if (minuse) "e-" + snum else "e+" + snum
			snum = sfrac + snum
			if (minusf) snum = "-" + snum
		}
		while (snum.length < w) snum = " " + snum
		snum
	}

	def ZtoEstring(num: Z, w: Int, d: Int): String = {
		var snum = DoubletoEstring(num.re, w, d)
		snum = if (num.im < 0) snum + " - " + DoubletoEstring(-num.im, d + 6, d) + "i"
		else snum + " + " + DoubletoEstring(num.im,
			d + 6, d) + "i"
		snum
	}
}

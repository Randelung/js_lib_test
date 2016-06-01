package JampackNew

import scala.language.implicitConversions

object Z {

	val ONE = new Z(1, 0)

	val ZERO = new Z(0, 0)

	val I = new Z(0, 1)

	def abs1(z: Z): Double = Math.abs(z.re) + Math.abs(z.im)

	def IsEqual(z1: Z, z2: Z): Boolean = {
		if (z1.re == z2.re && z1.im == z2.im) {
			true
		}
		else {
			false
		}
	}

	implicit def fromInt(x: Int): Z = new Z(x, 0)

	implicit def fromDouble(x: Double): Z = new Z(x, 0)

	implicit def fromLong(x: Long): Z = new Z(x, 0)

	implicit def fromShort(x: Short): Z = new Z(x, 0)

	implicit def fromFloat(x: Float): Z = new Z(x, 0)

	implicit object ZAsNumeric extends Numeric[Z] {

		override def plus(x: Z, y: Z): Z = x + y

		override def toDouble(x: Z): Double = throw new IllegalArgumentException("Can't transform Z to Double.")

		override def toFloat(x: Z): Float = throw new IllegalArgumentException("Can't transform Z to Float.")

		override def toInt(x: Z): Int = throw new IllegalArgumentException("Can't transform Z to Int.")

		override def negate(x: Z): Z = -x

		override def toLong(x: Z): Long = throw new IllegalArgumentException("Can't transform Z to Long.")

		override def times(x: Z, y: Z): Z = x * y

		override def minus(x: Z, y: Z): Z = x - y

		override def compare(x: Z, y: Z): Int = throw new IllegalArgumentException("Z are not ordered.")

		override def fromInt(x: Int): Z = new Z(x)
	}
}

case class Z(re: Double, im: Double) {

	def this(x: Double) {
		this(x, 0)
	}

	def abs: Double = {
		var are: Double = 0.0
		var aim: Double = 0.0
		var rho: Double = 0.0
		are = re.abs
		aim = im.abs
		if (are + aim == 0) 0
		else if (are >= aim) {
			rho = aim / are
			are * math.sqrt(1 + rho * rho)
		}
		else {
			rho = are / aim
			aim * math.sqrt(1 + rho * rho)
		}
	}

	def conj = {
		new Z(re, -im)
	}

	def unary_- = {
		new Z(-re, -im)
	}

	def +(that: Z): Z = {
		new Z(re + that.re, im + that.im)
	}

	def -(that: Z): Z = {
		new Z(re - that.re, im - that.im)
	}

	def *(that: Z): Z = {
		new Z(re * that.re - im * that.im, re * that.im + im * that.re)
	}

	def *(that: Double): Z = {
		new Z(re * that, im * that)
	}

	def /(that: Z): Z = {
		this * that.inv
	}

	def /(that: Double): Z = {
		if (that == 0) {
			throw new JampackException("Divide by zero.")
		}
		new Z(re / that, im / that)
	}

	def inv: Z = {
		conj / (re * re + im * im)
	}

	def sqrt: Z = {
		var t: Double = 0.0
		var tre: Double = 0.0
		var tim: Double = 0.0
		t = abs
		if (Math.abs(re) <= Math.abs(im)) {
			tre = Math.sqrt(0.5 * (t + re))
			tim = Math.sqrt(0.5 * (t - re))
		}
		else {
			if (re > 0) {
				tre = t + re
				tim = Math.abs(im) * Math.sqrt(0.5 / tre)
				tre = Math.sqrt(0.5 * tre)
			}
			else {
				tim = t - re
				tre = Math.abs(im) * Math.sqrt(0.5 / tim)
				tim = Math.sqrt(0.5 * tim)
			}
		}
		if (im < 0) tim = -tim
		new Z(tre, tim)
	}

	def exp: Z = {
		val r = math.exp(re)
		new Z(r * math.cos(im), r * math.sin(im))
	}

	def round(precision: Int): Z = {
		val factor = math.pow(10, precision)
		new Z((re * factor).round / factor, (im * factor).round / factor)
	}
}

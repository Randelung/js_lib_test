package ch.ethz.ipes.buschr.maths

import scala.language.implicitConversions

case class Complex(re: Double = 0, im: Double = 0) {

	def +(that: Complex): Complex = new Complex(re + that.re, im + that.im)

	def -(that: Complex): Complex = new Complex(re - that.re, im - that.im)

	def *(that: Complex): Complex = new Complex(re * that.re - im * that.im, im * that.re + re * that.im)

	def /(that: Complex): Complex = this * that.inv

	def exp: Complex = new Complex(math.exp(re) * math.cos(im), math.exp(re) * math.sin(im))

	def inv: Complex = {
		require(re != 0 || im != 0, "Magnitude can't be zero.")
		val r = re * re + im * im
		new Complex(re / r, -im / r)
	}

	def conjugate: Complex = new Complex(re, -im)

	def unary_- = new Complex(-re, -im)

	def abs = math.sqrt(re * re + im * im)
}

object Complex {

	val i = new Complex(0, 1)

	implicit def fromDouble(d: Double): Complex = new Complex(d, 0)

	implicit def fromInt(i: Int): Complex = new Complex(i, 0)

	implicit def fromLong(l: Long): Complex = new Complex(l, 0)

	implicit def fromFloat(f: Float): Complex = new Complex(f, 0)

	implicit def fromShort(s: Short): Complex = new Complex(s, 0)
}

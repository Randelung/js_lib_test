package ch.ethz.ipes.buschr.maths

import scala.language.implicitConversions

/** Vector geometry is easier than x/y calculations. */
case class Vector2D(x: Double, y: Double) {

	lazy val length = math.sqrt(x * x + y * y)

	lazy val normed = Vector2D(x / length, y / length)

	lazy val perpendicular = Vector2D(-y, x)

	def +(that: Vector2D) = Vector2D(x + that.x, y + that.y)

	def -(that: Vector2D) = Vector2D(x - that.x, y - that.y)

	def unary_- = Vector2D(-x, -y)

	def *(that: Double) = Vector2D(x * that, y * that)

	def /(that: Double) = Vector2D(x / that, y / that)

	def scalarProduct(that: Vector2D) = x * that.x + y * that.y

	def angleTo(that: Vector2D) = math.acos(this scalarProduct that / (length * that.length))
}

/** Implicits for easy usage */
object Vector2D {

	implicit def fromTuple2DD(i: (Double, Double)): Vector2D = new Vector2D(i._1, i._2)

	implicit def fromTuple2II(i: (Int, Int)): Vector2D = new Vector2D(i._1, i._2)

	implicit def fromTuple2DI(i: (Int, Double)): Vector2D = new Vector2D(i._1, i._2)

	implicit def fromTuple2ID(i: (Double, Int)): Vector2D = new Vector2D(i._1, i._2)
}

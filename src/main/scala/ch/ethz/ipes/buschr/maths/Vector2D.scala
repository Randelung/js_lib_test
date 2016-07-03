package ch.ethz.ipes.buschr.maths

/**
  * Created by Randolph Busch on 03/07/16.
  */
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

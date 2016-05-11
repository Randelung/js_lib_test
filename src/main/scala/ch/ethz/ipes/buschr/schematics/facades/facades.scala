package ch.ethz.ipes.buschr.schematics.facades

import ch.ethz.ipes.buschr.schematics.maths.Matrix

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

object Implicits {

	implicit class IntAsMatrix(x: Int) {

		def toSquareMatrix(size: Int): Matrix = {
			val matrix = Matrix.zeros(size)
			for (i <- 0 until size) {
				matrix(i, i) = x
			}
			matrix
		}
	}

	implicit class DoubleAsMatrix(x: Double) {

		def toSquareMatrix(size: Int): Matrix = {
			val matrix = Matrix.zeros(size)
			for (i <- 0 until size) {
				matrix(i, i) = x
			}
			matrix
		}
	}

	/*implicit class FractionAsMatrix(x: Fraction) {

		def toSquareMatrix(size: Int): Matrix = {
			val matrix = Matrix.zeros(size)
			for (i <- 0 until size) {
				matrix(i, i) = x
			}
			matrix
		}
	}*/

	implicit def IntAsFraction(x: Int): Fraction = new Fraction(x, 1)

	implicit def FractionAsExpression(x: Fraction): Expression = new Expression(x)

	implicit def StringAsExpression(x: String): Expression = new Expression(x)

	implicit def IntAsExpression(x: Int): Expression = new Expression(x)
}

@JSName("algebra.Fraction")
@js.native
class Fraction(numerator: Int, denominator: Int) extends js.Any {

	def copy(): Fraction = js.native

	def reduce(): Fraction = js.native

	def equalTo(that: Fraction): Boolean = js.native

	def add(that: Fraction, simplify: Boolean = true): Fraction = js.native

	def add(that: Int, simplify: Boolean): Fraction = js.native

	def add(that: Int): Fraction = js.native

	def subtract(that: Fraction, simplify: Boolean = true): Fraction = js.native

	def subtract(that: Int, simplify: Boolean): Fraction = js.native

	def subtract(that: Int): Fraction = js.native

	def multiply(that: Fraction, simplify: Boolean = true): Fraction = js.native

	def multiply(that: Int, simplify: Boolean): Fraction = js.native

	def multiply(that: Int): Fraction = js.native

	def divide(that: Fraction, simplify: Boolean = true): Fraction = js.native

	def divide(that: Int, simplify: Boolean): Fraction = js.native

	def divide(that: Int): Fraction = js.native

	def pow(exponent: Double, simplify: Boolean = true): Fraction = js.native

	def abs(): Fraction = js.native

	def valueOf(): Double = js.native

	override def toString(): String = js.native

	def toTex(): String = js.native
}

@JSName("algebra.Expression")
@js.native
class Expression private(variable: js.Any) extends js.Any {

	def this(variable: String) = this(variable.asInstanceOf[js.Any])

	def this(variable: Int) = this(variable.asInstanceOf[js.Any])

	def this(variable: Fraction) = this(variable.asInstanceOf[js.Any])

	def this() = this(null.asInstanceOf[js.Any])

	def constant(): Fraction = js.native

	def simplify(): Expression = js.native

	def copy(): Expression = js.native

	def add(that: Expression, simplify: Boolean): Expression = js.native

	def add(that: Expression): Expression = js.native

	def add(that: Fraction, simplify: Boolean): Expression = js.native

	def add(that: Fraction): Expression = js.native

	def add(that: Int, simplify: Boolean): Expression = js.native

	def add(that: Int): Expression = js.native

	def add(that: String, simplify: Boolean = true): Expression = js.native

	def subtract(that: Expression, simplify: Boolean): Expression = js.native

	def subtract(that: Expression): Expression = js.native

	def subtract(that: Fraction, simplify: Boolean): Expression = js.native

	def subtract(that: Fraction): Expression = js.native

	def subtract(that: Int, simplify: Boolean): Expression = js.native

	def subtract(that: Int): Expression = js.native

	def subtract(that: String, simplify: Boolean = true): Expression = js.native

	def multiply(that: Expression, simplify: Boolean): Expression = js.native

	def multiply(that: Expression): Expression = js.native

	def multiply(that: Fraction, simplify: Boolean): Expression = js.native

	def multiply(that: Fraction): Expression = js.native

	def multiply(that: Int, simplify: Boolean): Expression = js.native

	def multiply(that: Int): Expression = js.native

	def multiply(that: String, simplify: Boolean = true): Expression = js.native

	def divide(that: Fraction, simplify: Boolean): Expression = js.native

	def divide(that: Fraction): Expression = js.native

	def divide(that: Int, simplify: Boolean = true): Expression = js.native

	def pow(exponent: Int, simplify: Boolean = true): Expression = js.native

	def eval(values: js.Dynamic, simplify: Boolean = true): js.Any = js.native

	def summation(varName: String, lowerBound: Int, upperBound: Int, simplify: Boolean = true): Expression = js.native

	override def toString(): String = js.native

	def toTex(dict: js.Dynamic): String = js.native
}

@JSName("algebra.Equation")
@js.native
class Equation private(exp1: Expression, exp2: js.Any) extends js.Any {

	def this(exp1: Expression, exp2: Int) = this(exp1, exp2.asInstanceOf[js.Any])

	def this(exp1: Expression, exp2: Expression) = this(exp1, exp2.asInstanceOf[js.Any])

	def this(exp1: Expression, exp2: Fraction) = this(exp1, exp2.asInstanceOf[js.Any])

	def solveFor(varName: String): js.Any = js.native

	def eval(values: js.Dynamic): Equation = js.native

	override def toString(): String = js.native

	def toTex(): String = js.native
}

@JSName("algebra")
@js.native
object algebra extends js.Any {

	def parse(expression: String): js.Any = js.native

	def toTex(input: Fraction): String = js.native

	def toTex(input: Expression): String = js.native

	def toTex(input: Equation): String = js.native
}

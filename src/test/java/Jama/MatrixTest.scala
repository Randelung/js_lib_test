package Jama

import ch.ethz.ipes.buschr.maths.{Complex, DiffEquation}
import org.scalatest.{FlatSpec, Matchers}

class MatrixTest extends FlatSpec with Matchers {

	behavior of "A JAMA Matrix"

	it should "be able to solve a system" in {

		val matrix = Matrix.random(3, 3)
		val vector = new Matrix(Seq(Seq(1d).toArray, Seq(1d).toArray, Seq(1d).toArray).toArray)
		val solution = matrix.solve(vector)
		//println(s"$matrix\n*\n$solution\n=\n${matrix * solution}\n=?\n$vector")
	}

	it should "be able to do eigenvalue decomposition" in {

		val matrix = Matrix.random(3, 3)
		val eigenvalueDecomposition = new EigenvalueDecomposition(matrix)
		//println(s"eig($matrix)\n=\n${eigenvalueDecomposition.getRealEigenvalues.deep.mkString("\n")}\n${eigenvalueDecomposition.getImagEigenvalues.deep.mkString("\n")}")
		//println(s"${eigenvalueDecomposition.getV.getArray.deep.mkString("\n")}")
	}

	it should "be able to do QR decomposition" in {

		val matrix = Matrix.random(3, 3)
		val qRDecomposition = new QRDecomposition(matrix)
		//println(s"${qRDecomposition.getQ}\n*\n$matrix\n=\n${qRDecomposition.getR}")
	}

	def round(x: Complex, precision: Int): Complex = {
		val factor = math.pow(10, precision)
		new Complex((x.re * factor).round / factor, (x.im * factor).round / factor)
	}

	behavior of "A DiffEquation"

	it should "be able to find a general solution for the equation" in {

		val diffEquation = new DiffEquation(Array(5, -6))
	}

	it should "be able to apply starting conditions to a general solution to obtain a specific solution" in {

		val diffEquation = new DiffEquation(Array(5, -6))
		diffEquation.applyStartingConditions(Array(1, 1))
		round(diffEquation.solution(0), 5) shouldEqual round(1, 5)
		round(diffEquation.solution(1), 5) shouldEqual round(2 * math.exp(2) - math.exp(3), 5)
		round(diffEquation.solution(2), 5) shouldEqual round(2 * math.exp(4) - math.exp(6), 5)
	}

	it should "also work for complex eigenvalues" in {

		val diffEquation = new DiffEquation(Array(0, -1))
		diffEquation.applyStartingConditions(Array(1, 1))
		round(diffEquation.solution(0), 5) shouldEqual round(1, 5)
		round(diffEquation.solution(math.Pi), 5) shouldEqual round(-1, 5)
		round(diffEquation.solution(2 * math.Pi), 5) shouldEqual round(1, 5)
	}
}

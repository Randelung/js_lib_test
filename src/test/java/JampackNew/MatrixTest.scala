package JampackNew

import ch.ethz.ipes.buschr.maths.DiffEquation
import org.scalatest.{FlatSpec, Matchers}

class MatrixTest extends FlatSpec with Matchers {

	behavior of "A JAMA Matrix"

	it should "be able to solve a system" in {

		val matrix = Zmat.random(3, 3)
		val vector = Zmat.random(3, 1)
		(matrix * matrix.solve(vector)).round(5) shouldEqual vector.round(5)
	}

	it should "be able to do eigenvalue decomposition" in {


		val matrix = new Zmat(Array[Array[Double]](Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 10)))
		val eigenvalueDecomposition = matrix.eig()
		eigenvalueDecomposition.D.round(4) shouldEqual new Zdiagmat(new Zmat(Array(Array(16.7075, 0, 0), Array(0, -0.9057, 0), Array(0, 0, 0.1982))))
	}

	it should "be able to do QR decomposition" in {

		val matrix = Zmat.random(3, 3)
		val qRDecomposition = matrix.qr()
		qRDecomposition.qb(qRDecomposition.R).round(4) shouldEqual matrix.round(4)
		qRDecomposition.qhb(matrix).round(4) shouldEqual qRDecomposition.R.round(4)
	}

	behavior of "A DiffEquation"

	it should "be able to find a general solution for the equation" in {

		new DiffEquation(Array(5, -6))
	}

	it should "be able to apply starting conditions to a general solution to obtain a specific solution" in {

		val diffEquation = new DiffEquation(Array(5, -6))
		//eigenvalues 2 and 3, eigenvectors (1, 2) and (1, 3)
		diffEquation.applyStartingConditions(Array(1, 1))
		val solution = diffEquation.solution()
		solution(0).round(5) shouldEqual implicitly[Z](1)
		solution(1).round(5) shouldEqual implicitly[Z](2 * math.exp(2) - math.exp(3)).round(5)
		solution(2).round(5) shouldEqual implicitly[Z](2 * math.exp(4) - math.exp(6)).round(5)
	}

	it should "also work for complex eigenvalues" in {

		val diffEquation = new DiffEquation(Array(0, -1))
		// eigenvalues i and -i, eigenvectors (1, i) and (i, 1)
		diffEquation.applyStartingConditions(Array(1, 1))
		diffEquation.solution()(0).round(5) shouldEqual implicitly[Z](1)
		diffEquation.solution()(math.Pi).round(5) shouldEqual implicitly[Z](-1)
		diffEquation.solution()(2 * math.Pi).round(5) shouldEqual implicitly[Z](1)
		diffEquation.solution()(3).round(5) shouldEqual implicitly[Z](-0.84887)
	}

	it should "be able to handle eigenvalues with multiplicity > 1" in {

		val diffEquation = new DiffEquation(Array(6, -9))
		// eigenvalues 3 (x2), eigenvectors (1, 3) (x2)
		// generalized eigenvectors (1, 3) and (-3, 1)
		diffEquation.applyStartingConditions(Array(1, 1))
		diffEquation.solution()(0).round(5) shouldEqual implicitly[Z](1)
		diffEquation.solution()(1).round(5) shouldEqual -implicitly[Z](3).exp.round(5)
	}

	it should "be able to do the same for a data point with t != 0" in {

		var diffEquation = new DiffEquation(Array(5, -6))
		//eigenvalues 2 and 3, eigenvectors (1, 2) and (1, 3)
		diffEquation.applyDataPoint(2, Array(2, 2))
		val solution = diffEquation.solution()
		solution(0).round(4) shouldEqual implicitly[Z](0.0683)
		solution(1).round(4) shouldEqual implicitly[Z](0.4418)
		solution(2).round(5) shouldEqual implicitly[Z](2)

		diffEquation = new DiffEquation(Array(0, -1))
		// eigenvalues i and -i, eigenvectors (1, i) and (i, 1)
		diffEquation.applyDataPoint(1, Array(1, 1))
		diffEquation.solution()(0).round(4) shouldEqual implicitly[Z](-0.3012)
		diffEquation.solution()(math.Pi).round(4) shouldEqual implicitly[Z](0.3012)
		diffEquation.solution()(2 * math.Pi).round(4) shouldEqual implicitly[Z](-0.3012)
		diffEquation.solution()(3).round(4) shouldEqual implicitly[Z](0.4932)

		diffEquation = new DiffEquation(Array(6, -9))
		// eigenvalues 3 (x2), eigenvectors (1, 3) (x2)
		// generalized eigenvectors (1, 3) and (-3, 1)
		diffEquation.applyDataPoint(5, Array(3, 4))
		diffEquation.solution()(5).round(4) shouldEqual implicitly[Z](3)
		diffEquation.solution()(3).round(4) shouldEqual implicitly[Z](0.0322)
		diffEquation.solution()(6).round(4) shouldEqual implicitly[Z](-40.1711)
	}
}

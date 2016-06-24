package ch.ethz.ipes.buschr.maths

import JampackNew.{Z, Zmat}
import org.scalatest.{FlatSpec, Matchers}

class DiffEquationTest extends FlatSpec with Matchers {

	behavior of "A DiffEquation"

	it should "be able to find a general solution for the equation" in {

		DiffEquation(Array(5, -6))
	}

	it should "be able to apply starting conditions to a general solution to obtain a specific solution" in {

		val diffEquation = DiffEquation(Array(5, -6))
		//eigenvalues 2 and 3, eigenvectors (1, 2) and (1, 3)
		diffEquation.applyStartingConditions(Array(1, 1))
		diffEquation.solution(0).round(5) shouldEqual implicitly[Z](1)
		diffEquation.solution(1).round(5) shouldEqual implicitly[Z](2 * math.exp(2) - math.exp(3)).round(5)
		diffEquation.solution(2).round(5) shouldEqual implicitly[Z](2 * math.exp(4) - math.exp(6)).round(5)
	}

	it should "also work for complex eigenvalues" in {

		val diffEquation = DiffEquation(Array(0, -1))
		// eigenvalues i and -i, eigenvectors (1, i) and (i, 1)
		diffEquation.applyStartingConditions(Array(1, 1))
		diffEquation.solution(0).round(5) shouldEqual implicitly[Z](1)
		diffEquation.solution(math.Pi).round(5) shouldEqual implicitly[Z](-1)
		diffEquation.solution(2 * math.Pi).round(5) shouldEqual implicitly[Z](1)
		diffEquation.solution(3).round(5) shouldEqual implicitly[Z](-0.84887)
	}

	it should "be able to handle eigenvalues with multiplicity > 1" in {

		val diffEquation = DiffEquation(Array(6, -9))
		// eigenvalues 3 (x2), eigenvectors (1, 3) (x2)
		// generalized eigenvectors (1, 3) and (-3, 1)
		diffEquation.applyStartingConditions(Array(1, 1))
		diffEquation.solution(0).round(5) shouldEqual implicitly[Z](1)
		diffEquation.solution(1).round(5) shouldEqual -implicitly[Z](3).exp.round(5)
	}

	it should "be able to do the same for a data point with t != 0" in {

		var diffEquation = DiffEquation(Array(5, -6))
		//eigenvalues 2 and 3, eigenvectors (1, 2) and (1, 3)
		diffEquation.applyDataPoint(2, Array(2, 2))
		diffEquation.solution(0).round(4) shouldEqual implicitly[Z](0.0683)
		diffEquation.solution(1).round(4) shouldEqual implicitly[Z](0.4418)
		diffEquation.solution(2).round(5) shouldEqual implicitly[Z](2)

		diffEquation = DiffEquation(Array(0, -1))
		// eigenvalues i and -i, eigenvectors (1, i) and (i, 1)
		diffEquation.applyDataPoint(1, Array(1, 1))
		diffEquation.solution(0).round(4) shouldEqual implicitly[Z](-0.3012)
		diffEquation.solution(math.Pi).round(4) shouldEqual implicitly[Z](0.3012)
		diffEquation.solution(2 * math.Pi).round(4) shouldEqual implicitly[Z](-0.3012)
		diffEquation.solution(3).round(4) shouldEqual implicitly[Z](0.4932)

		diffEquation = DiffEquation(Array(6, -9))
		// eigenvalues 3 (x2), eigenvectors (1, 3) (x2)
		// generalized eigenvectors (1, 3) and (-3, 1)
		diffEquation.applyDataPoint(5, Array(3, 4))
		diffEquation.solution(5).round(4) shouldEqual implicitly[Z](3)
		diffEquation.solution(3).round(4) shouldEqual implicitly[Z](0.0322)
		diffEquation.solution(6).round(4) shouldEqual implicitly[Z](-40.1711)
	}

	//TODO the efficient route would require a special implementation of the sylvester solution for triangular matrices only.
	// possibly worth looking into later.
	it should "compare the two sylvester equation techniques" in {

		var t = System.nanoTime()
		DiffEquation.solveSylvesterEquation(new Zmat(Array[Array[Z]](Array(1, 0), Array(0, 2))),
			new Zmat(Array[Array[Z]](Array(1, 0), Array(0, 2))),
			new Zmat(Array[Array[Z]](Array(1, 1), Array(1, 1))))
		val t1 = System.nanoTime() - t
		t = System.nanoTime()
		DiffEquation.solveSylvesterEquationEfficiently(new Zmat(Array[Array[Z]](Array(1, 0), Array(0, 2))),
			new Zmat(Array[Array[Z]](Array(1, 0), Array(0, 2))),
			new Zmat(Array[Array[Z]](Array(1, 1), Array(1, 1))))
		val t2 = System.nanoTime() - t
		println(s"time 1: $t1\ntime 2: $t2")
	}

	it should "solve the sylvester equation" in {

		DiffEquation.solveSylvesterEquationEfficiently(new Zmat(Array[Array[Z]](Array(1, 0), Array(0, 2))),
			new Zmat(Array[Array[Z]](Array(1, 0), Array(0, 2))),
			new Zmat(Array[Array[Z]](Array(1, 1), Array(1, 1))))
		//TODO doesn't work - why? any reasoning to exclude the failing cases from definition range?
		/*println(
			DiffEquation.solveSylvesterEquationEfficiently(new Zmat(Array[Array[Z]](Array(1, 0), Array(0, 2))),
				-new Zmat(Array[Array[Z]](Array(1, 0), Array(0, 2))),
				new Zmat(Array[Array[Z]](Array(1, 1), Array(1, 1))))
		)*/
	}

	it should "be able to solve equations with constant inhomogeneities and starting conditions" in {

		val diffEquation = DiffEquation(Array(5, -6))
		diffEquation.applyInhomogeneity(new Zmat(Array[Array[Z]](Array(0), Array(1))), null)
		diffEquation.applyStartingConditions(Array(1, 1))
		diffEquation.solution(0).round(4) shouldEqual implicitly[Z](1)
		diffEquation.solution(1).round(4) shouldEqual implicitly[Z](-2.1401)
		diffEquation.solution(2).round(4) shouldEqual implicitly[Z](-186.8886)
	}

	it should "be able to solve equations with exponential inhomogeneities and starting conditions" in {

		val diffEquation = DiffEquation(Array(5, -6))
		diffEquation.applyInhomogeneity(new Zmat(Array[Array[Z]](Array(0, 0), Array(Z(0, 0.5), Z(0, -0.5)))),
			new Zmat(Array[Array[Z]](Array(Z(0, -1)), Array(Z(0, 1)))))
		diffEquation.applyStartingConditions(Array(1, 1))
		diffEquation.solution(0).round(4) shouldEqual implicitly[Z](1)
		diffEquation.solution(1).round(4) shouldEqual Z(-4.6385, 0)
		diffEquation.solution(2).round(4) shouldEqual Z(-264.7599, 0)
	}

	it should "be able to solve equations with polynomial inhomogeneities and starting conditions" in {

		val diffEquation = DiffEquation(Array(5, -6))
		diffEquation.applyInhomogeneity(new Zmat(Array[Array[Z]](Array(0, 0), Array(1, 1))), null)
		diffEquation.applyStartingConditions(Array(1, 1))
		diffEquation.solution(0).round(4) shouldEqual implicitly[Z](1)
		diffEquation.solution(1).round(4) shouldEqual Z(-1.4501, 0)
		diffEquation.solution(2).round(4) shouldEqual Z(-155.2405, 0)
	}

	it should "be able to solve equations with constant inhomogeneities and a data point" in {

		val diffEquation = DiffEquation(Array(5, -6))
		diffEquation.applyInhomogeneity(new Zmat(Array[Array[Z]](Array(0), Array(1))), null)
		diffEquation.applyDataPoint(1, Array(1, 1))
		diffEquation.solution(0).round(4) shouldEqual implicitly[Z](0.3365)
		diffEquation.solution(1).round(4) shouldEqual implicitly[Z](1)
		diffEquation.solution(2).round(4) shouldEqual implicitly[Z](-2.1401)
	}

	it should "be able to solve equations with exponential inhomogeneities and a data point" in {

		val diffEquation = DiffEquation(Array(5, -6))
		diffEquation.applyInhomogeneity(new Zmat(Array[Array[Z]](Array(0, 0), Array(Z(0, 0.5), Z(0, -0.5)))),
			new Zmat(Array[Array[Z]](Array(Z(0, -1)), Array(Z(0, 1)))))
		diffEquation.applyDataPoint(1, Array(1, 1))
		diffEquation.solution(0).round(4) shouldEqual implicitly[Z](0.2760)
		diffEquation.solution(1).round(4) shouldEqual implicitly[Z](1)
		diffEquation.solution(2).round(4) shouldEqual implicitly[Z](-2.3880)
	}

	it should "be able to solve equations with polynomial inhomogeneities and a data point" in {

		val diffEquation = DiffEquation(Array(5, -6))
		diffEquation.applyInhomogeneity(new Zmat(Array[Array[Z]](Array(0, 0), Array(1, 1))), null)
		diffEquation.applyDataPoint(1, Array(1, 1))
		diffEquation.solution(0).round(4) shouldEqual implicitly[Z](0.3960)
		diffEquation.solution(1).round(4) shouldEqual implicitly[Z](1)
		diffEquation.solution(2).round(4) shouldEqual implicitly[Z](1.7172)
	}

	it should "do the same for ODE systems" in {

		val diffEquation = new DiffEquation(new Zmat(Array[Array[Z]](Array(0, -1), Array(1, 0))))
		diffEquation.applyStartingConditions(Array(1, 0))
		diffEquation.solutionVector(0).round(4) shouldEqual new Zmat(Array[Array[Z]](Array(1), Array(0)))
		diffEquation.solutionVector(0.5 * math.Pi).round(4) shouldEqual new Zmat(Array[Array[Z]](Array(0), Array(1)))
		diffEquation.solutionVector(math.Pi).round(4) shouldEqual new Zmat(Array[Array[Z]](Array(-1), Array(0)))
		diffEquation.solutionVector(1.5 * math.Pi).round(4) shouldEqual new Zmat(Array[Array[Z]](Array(0), Array(-1)))
		diffEquation.solutionVector(2 * math.Pi).round(4) shouldEqual new Zmat(Array[Array[Z]](Array(1), Array(0)))
	}

	it should "be reusable with different time points" in {
		val diffEquation = new DiffEquation(new Zmat(Array[Array[Z]](Array(0, -1), Array(1, 0))))
		diffEquation.applyStartingConditions(Array(1, 0))
		val diffEquation2 = diffEquation.clone()
		diffEquation2.applyStartingConditions(Array(2, 0))
		diffEquation.solutionVector(0) shouldEqual new Zmat(Array[Array[Z]](Array(1), Array(0)))
		diffEquation2.solutionVector(0) shouldEqual new Zmat(Array[Array[Z]](Array(2), Array(0)))
	}

}

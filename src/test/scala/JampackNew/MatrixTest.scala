package JampackNew

import ch.ethz.ipes.buschr.maths.JamaSVD
import org.scalatest.{FlatSpec, Matchers}

class MatrixTest extends FlatSpec with Matchers {

	"A JamaSVD" should "be able to SVD a matrix" in {

		val matrix = new Zmat(Array[Array[Z]](Array(0, 1), Array(0, 0)))
		val svd = new JamaSVD(matrix)
		svd.U * svd.S * svd.V.transpose.conj shouldEqual new Zmat(Array[Array[Z]](Array(0, 1), Array(0, 0)))
		svd.S shouldEqual new Zdiagmat(Array[Double](1, 0), Array.fill(2)(0d))
		matrix.pinv shouldEqual new Zmat(Array[Array[Z]](Array(0, 0), Array(1, 0)))
	}

	behavior of "A JamPack Matrix"

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
}

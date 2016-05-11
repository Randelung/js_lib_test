package JamaNew

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

	it should "be able to create a diff equation matrix from a linear homogeneous differential equation" in {

		val matrix = Matrix.fromDiffEquation(Array(5, -6))
		matrix shouldEqual new Matrix(Array(Array(0, 1), Array(-6, 5)))
		val eigenvalueDecomposition = new EigenvalueDecomposition(matrix)
		println(eigenvalueDecomposition.getRealEigenvalues.deep.mkString("\n"))
		println(eigenvalueDecomposition.getV)
	}

}

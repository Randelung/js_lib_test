package ch.ethz.ipes.buschr.maths

import JampackNew.Zmat

/** Adapted from Numerical Recipes, second edition, svdcmp.cpp
  *
  * @param matrix Matrix to be decomposed
  */
class mySVD(matrix: Zmat) {

	private var flag: Boolean = _
	private var i, its, j, jj, k, l, nm: Int = _
	private val m = matrix.nrow
	private val n = matrix.ncol
	val S = matrix.clone()
	val U = new Zmat(m, m)
	val V = new Zmat(n, n)
	private var anorm, c, f, g, h, s, scale, x, y, z: Double = _
	val rv1 = Array.fill(n)(0d)
	g = 0
	scale = 0

	for (i <- 0 until n) {
		l = i + 2
		rv1(i) = scale * g
		scale = 0
		s = 0
		g = 0
		if (i < m) {
			for (k <- i until m) {
				//scale += fabs(S(k)(i))
			}
			if (scale != 0)
				for (k <- i until m) {
					S(k)(i) /= scale
					//s += S(k)(i) * S(k)(i)
				}
		}
	}
}

package JampackNew

object Block {

	def o(A: Zmat, ii: Array[Int], jj: Array[Int]): Array[Array[Zmat]] = {
		var i = 0
		var j = 0
		A.loadProperties()
		val m = ii.length
		val n = jj.length
		if (ii(0) < A.baseIndex || ii(m - 1) > A.rx + 1) {
			throw new JampackException("Illegal row array.")
		}
		for (i <- 1 until m) {
			if (ii(i - 1) >= ii(i)) {
				throw new JampackException("Illegal row array.")
			}
		}
		if (jj(0) < A.baseIndex || jj(n - 1) > A.cx + 1) {
			throw new JampackException("Illegal column array.")
		}
		for (j <- 1 until n) {
			if (jj(j - 1) >= jj(j)) {
				throw new JampackException("Illegal column array.")
			}
		}
		val B = Array.ofDim[Zmat](m - 1, n - 1)
		for (i <- 0 until m - 1; j <- 0 until n - 1) {
			B(i)(j) = A.get(ii(i), ii(i + 1) - 1, jj(j), jj(j + 1) - 1)
		}
		B
	}
}

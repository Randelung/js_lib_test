package JampackNew

class Zchol(A: Zmat) {

	A.loadProperties()

	if (A.nr != A.nc) {
		throw new JampackException("Matrix not square.")
	}

	var n: Int = A.nr

	var R: Zutmat = new Zutmat(A)

	var mu: Double = 0.0

	for (i <- 0 until n) {
		if (R.im(i)(i) != 0) {
			throw new JampackException("Matrix not Hermitian")
		}
		for (j <- 0 until i) {
			if (R.re(i)(j) != R.re(j)(i) || R.im(i)(j) != -R.im(j)(i)) {
				throw new JampackException("Matrix not Hermitian")
			}
			R.im(i)(j) = 0
			R.re(i)(j) = 0
		}
	}

	for (k <- 0 until n) {
		if (R.re(k)(k) <= 0) {
			throw new JampackException("Nonpositive diagonal entry during reduction.")
		}
		R.re(k)(k) = Math.sqrt(R.re(k)(k))
		mu = 1 / R.re(k)(k)
		for (j <- k + 1 until n) {
			R.re(k)(j) = mu * R.re(k)(j)
			R.im(k)(j) = mu * R.im(k)(j)
		}
		for (i <- k + 1 until n) {
			for (j <- i until n) {
				R.re(i)(j) = R.re(i)(j) - R.re(k)(i) * R.re(k)(j) - R.im(k)(i) * R.im(k)(j)
				R.im(i)(j) = R.im(i)(j) - R.re(k)(i) * R.im(k)(j) + R.im(k)(i) * R.re(k)(j)
			}
			R.im(i)(i) = 0
		}
	}
}

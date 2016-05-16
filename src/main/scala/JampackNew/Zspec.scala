package JampackNew

class Zspec(AA: Zmat) {

	val A = new Zmat(AA)

	val S = new Schur(A)

	var U: Zmat = S.U

	var D: Zdiagmat = new Zdiagmat(S.T)

	if (AA.nrow != AA.ncol) {
		throw new RuntimeException("Matrix not square.")
	}

	for (i <- 0 until A.nrow) {
		if (A.im(i)(i) != 0) {
			throw new JampackException("Matrix not Hermitian")
		}
		for (j <- 0 until i) {
			if (A.re(i)(j) != A.re(j)(i) || A.im(i)(j) != -A.im(j)(i)) {
				throw new JampackException("Matrix not Hermitian")
			}
		}
	}

	for (i <- 0 until D.n) {
		D.im(i) = 0
	}
}

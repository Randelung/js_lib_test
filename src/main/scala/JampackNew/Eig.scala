package JampackNew

@throws[JampackException]
class Eig(val A: Zmat) {
	var i = 0
	var j = 0
	var k = 0
	var norm = 0d
	var scale = 0d
	var z: Z = null
	var d: Z = null
	A.loadProperties()
	if (A.nr != A.nc) {
		throw new JampackException("Matrix not square.")
	}
	val n = A.nr
	val S = new Schur(A)
	val T = S.T
	var D = new Zdiagmat(T)
	norm = Norm.fro(A)
	var X = new Zmat(n, n)
	for (k <- n - 1 to 0 by -1) {
		d = T.get0(k, k)
		X.re(k)(k) = 1
		X.im(k)(k) = 0
		for (i <- k - 1 to 0 by -1) {
			X.re(i)(k) = -T.re(i)(k)
			X.im(i)(k) = -T.im(i)(k)
			for (j <- i + 1 until k) {
				X.re(i)(k) = X.re(i)(k) - T.re(i)(j) * X.re(j)(k) + T.im(i)(j) * X.im(j)(k)
				X.im(i)(k) = X.im(i)(k) - T.re(i)(j) * X.im(j)(k) - T.im(i)(j) * X.re(j)(k)
			}
			z = T.get0(i, i)
			z -= d
			if (z.re == 0 && z.im == 0) {
				z = new Z(1e-16 * norm, z.im)
			}
			z = X.get0(i, k) / z
			X.put0(i, k, z)
		}
		scale = 1d / Norm.fro(X, X.baseIndex, X.rx, X.baseIndex + k, X.baseIndex + k)
		for (i <- 0 until X.nr) {
			X.re(i)(k) = scale * X.re(i)(k)
			X.im(i)(k) = scale * X.im(i)(k)
		}
	}

	X = Times.o(S.U, X)
}



package JampackNew

@throws[JampackException]
class Eig(private var A: Zmat) {
	private var i = 0
	private var j = 0
	private var k = 0
	private var norm = 0d
	private var scale = 0d
	private var z: Z = null
	private var d: Z = null
	A.loadProperties()
	if (A.nr != A.nc) {
		throw new JampackException("Matrix not square.")
	}
	private var n = A.nr
	private var S = new Schur(A)
	private var T = S.T
	private var _D = new Zdiagmat(T)
	norm = Norm.fro(A)
	private var _X = new Zmat(n, n)
	for (k <- n - 1 to 0 by -1) {
		d = T.get0(k, k)
		_X.re(k)(k) = 1
		_X.im(k)(k) = 0
		for (i <- k - 1 to 0 by -1) {
			_X.re(i)(k) = -T.re(i)(k)
			_X.im(i)(k) = -T.im(i)(k)
			for (j <- i + 1 until k) {
				_X.re(i)(k) = _X.re(i)(k) - T.re(i)(j) * _X.re(j)(k) + T.im(i)(j) * _X.im(j)(k)
				_X.im(i)(k) = _X.im(i)(k) - T.re(i)(j) * _X.im(j)(k) - T.im(i)(j) * _X.re(j)(k)
			}
			z = T.get0(i, i)
			z -= d
			if (z.re == 0 && z.im == 0) {
				z = new Z(1e-16 * norm, z.im)
			}
			z = _X.get0(i, k) / z
			_X.put0(i, k, z)
		}
		scale = 1d / Norm.fro(_X, _X.baseIndex, _X.rx, _X.baseIndex + k, _X.baseIndex + k)
		for (i <- 0 until _X.nr) {
			_X.re(i)(k) = scale * _X.re(i)(k)
			_X.im(i)(k) = scale * _X.im(i)(k)
		}
	}

	_X = Times.o(S.U, _X)

	def D = _D.clone()

	def X = _X.clone()

	override def clone: Eig = {
		val copy = new Eig(new Zmat(Array[Array[Z]](Array(1))))
		copy.A = A.clone()
		copy.i = i
		copy.j = j
		copy.k = k
		copy.norm = norm
		copy.scale = scale
		copy.z = z
		copy.d = d
		copy.n = n
		copy.S = S
		copy.T = T
		copy._D = _D
		copy._X = _X
		copy
	}
}



package JampackNew

class Zludpp(A: Zmat) {

	A.getProperties()

	var absi: Double = 0.0
	var mx: Double = 0.0
	var t: Double = 0.0

	var nr = A.nr
	var nc = A.nc
	var nrl = nr
	var ncl = math.min(A.nr, A.nc)
	var nru = ncl
	var ncu = nc

	var L = new Zltmat(nrl, ncl)
	var U = new Zutmat(nru, ncu)
	var pvt = Array.ofDim[Int](ncl)

	var T = if (nrl >= ncu) L else U

	for (i <- 0 until nr; j <- 0 until nc) {
		T.re(i)(j) = A.re(i)(j)
		T.im(i)(j) = A.im(i)(j)
	}

	var Tk = Array.ofDim[Z](nrl)

	for (k <- 0 until math.min(nr, nc)) {
		mx = 0
		pvt(k) = k
		for (i <- k until nr) {
			Tk(i) = T.get0(i, k)
			absi = Tk(i).abs
			if (absi > mx) {
				pvt(k) = i
				mx = absi
			}
		}

		if (mx != 0.0) {
			val temp = Tk(k)
			Tk(k) = Tk(pvt(k))
			Tk(pvt(k)) = temp
			for (j <- 0 until nc) {
				t = T.re(k)(j)
				T.re(k)(j) = T.re(pvt(k))(j)
				T.re(pvt(k))(j) = t
				t = T.im(k)(j)
				T.im(k)(j) = T.im(pvt(k))(j)
				T.im(pvt(k))(j) = t
			}
			for (i <- k + 1 until nr) {
				Tk(i) /= Tk(k)
				T.put0(i, k, Tk(i))
				for (j <- k + 1 until nc) {
					T.re(i)(j) = T.re(i)(j) - T.re(i)(k) * T.re(k)(j) + T.im(i)(k) * T.im(k)(j)
					T.im(i)(j) = T.im(i)(j) - T.im(i)(k) * T.re(k)(j) - T.re(i)(k) * T.im(k)(j)
				}
			}
		}
	}

	if (nr >= nc)
		for (i <- 0 until nc) {
			for (j <- 0 until nc) {
				if (i > j) {
					U.re(i)(j) = 0.0
					U.im(i)(j) = 0.0
				}
				else {
					U.re(i)(j) = T.re(i)(j)
					U.im(i)(j) = T.im(i)(j)
					L.re(i)(j) = 0.0
					L.im(i)(j) = 0.0
				}
			}
			L.re(i)(i) = 1.0
			L.im(i)(i) = 0.0
		}
	else
		for (i <- 0 until nr) {
			for (j <- 0 until nr) {
				if (i > j) {
					L.re(i)(j) = T.re(i)(j)
					L.im(i)(j) = T.im(i)(j)
					U.re(i)(j) = 0.0
					U.im(i)(j) = 0.0
				}
				else {
					L.re(i)(j) = 0.0
					L.im(i)(j) = 0.0
				}
			}
			L.re(i)(i) = 1.0
			L.im(i)(i) = 0.0
		}
}

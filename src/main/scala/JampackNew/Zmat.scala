package JampackNew

import ch.ethz.ipes.buschr.maths.JamaSVD

class Zmat(_re: Array[Array[Double]], _im: Array[Array[Double]]) {

	Parameters.BaseIndexNotChangeable = true

	var basex = Parameters.BaseIndex

	var nrow = _re.length

	var ncol = _re(0).length

	if (nrow != _im.length || ncol != _im(0).length) {
		throw new JampackException("Inconsistent array dimensions")
	}

	var baseIndex: Int = _

	var rx: Int = _

	var cx: Int = _

	var nr: Int = _

	var nc: Int = _

	loadProperties()

	var re = Array.ofDim[Double](nr, nc)

	var im = Array.ofDim[Double](nr, nc)

	for (i <- 0 until nr; j <- 0 until nc) {
		re(i)(j) = _re(i)(j)
		im(i)(j) = _im(i)(j)
	}

	var dirty: Boolean = _

	var LU: Zludpp = _

	var HQR: Zhqrd = _

	var CHOL: Zchol = _

	def this(A: Array[Array[Z]]) {
		this(A.map(_.map(_.re)), A.map(_.map(_.im)))
	}

	def this(A: Array[Array[Double]]) {
		this(A, Array.fill[Double](A.length, A.head.length)(0))
	}

	def this(A: Zmat) {
		this(A.re, A.im)
	}

	def this(A: Z1) {
		this(A.re.map(Array(_)), A.im.map(Array(_)))
	}

	def this(D: Zdiagmat) {
		this(Array.ofDim[Double](1, 0), Array.ofDim[Double](1, 0))
		nrow = D.n
		ncol = D.n
		loadProperties()
		re = Array.ofDim[Double](nr, nc)
		im = Array.ofDim[Double](nr, nc)
		for (i <- 0 until nr) {
			re(i)(i) = D.re(i)
			im(i)(i) = D.im(i)
		}
	}

	def this(nrow: Int, ncol: Int) {
		this(Array.fill[Double](nrow, ncol)(0), Array.fill[Double](nrow, ncol)(0))
	}

	def apply(i: Int, j: Int) = new Z(re(i)(j), im(i)(j))

	def update(i: Int, j: Int, value: Z) = {
		re(i)(j) = value.re
		im(i)(j) = value.im
	}

	def apply(i: Int): Array[Z] = getZ(i)

	def update(i: Int, values: Array[Z]) = {
		for (j <- values.indices) {
			re(i)(j) = values(j).re
			im(i)(j) = values(j).im
		}
	}

	def loadProperties(): Unit = {
		baseIndex = basex
		rx = baseIndex + nrow - 1
		cx = baseIndex + ncol - 1
		nr = nrow
		nc = ncol
	}

	def getRe: Array[Array[Double]] = {
		val A = Array.ofDim[Double](nrow, ncol)
		for (i <- 0 until nrow; j <- 0 until ncol) A(i)(j) = re(i)(j)
		A
	}

	def getIm: Array[Array[Double]] = {
		val A = Array.ofDim[Double](nrow, ncol)
		for (i <- 0 until nrow; j <- 0 until ncol) A(i)(j) = im(i)(j)
		A
	}

	def getZ: Array[Array[Z]] = {
		val A = Array.ofDim[Z](nrow, ncol)
		for (i <- 0 until nrow; j <- 0 until ncol) {
			A(i)(j) = new Z(re(i)(j), im(i)(j))
		}
		A
	}

	def get(ii: Int, jj: Int): Z = {
		new Z(re(ii - basex)(jj - basex), im(ii - basex)(jj - basex))
	}

	def get0(i: Int, j: Int): Z = new Z(re(i)(j), im(i)(j))

	def put(ii: Int, jj: Int, a: Z) {
		dirty = true
		re(ii - basex)(jj - basex) = a.re
		im(ii - basex)(jj - basex) = a.im
	}

	def put0(i: Int, j: Int, a: Z) {
		dirty = true
		re(i)(j) = a.re
		im(i)(j) = a.im
	}

	def get(ii1: Int,
			ii2: Int,
			jj1: Int,
			jj2: Int): Zmat = {
		val nrow = ii2 - ii1 + 1
		val ncol = jj2 - jj1 + 1
		val A = new Zmat(nrow, ncol)
		for (i <- 0 until nrow; j <- 0 until ncol) {
			A.re(i)(j) = re(i + ii1 - basex)(j + jj1 - basex)
			A.im(i)(j) = im(i + ii1 - basex)(j + jj1 - basex)
		}
		A
	}

	def put(ii1: Int,
			ii2: Int,
			jj1: Int,
			jj2: Int,
			A: Zmat) {
		dirty = true
		val nrow = ii2 - ii1 + 1
		val ncol = jj2 - jj1 + 1
		for (i <- 0 until nrow; j <- 0 until ncol) {
			re(i + ii1 - basex)(j + jj1 - basex) = A.re(i)(j)
			im(i + ii1 - basex)(j + jj1 - basex) = A.im(i)(j)
		}
	}

	def get(ii: Array[Int], jj1: Int, jj2: Int): Zmat = {
		val nrow = ii.length
		val ncol = jj2 - jj1 + 1
		val A = new Zmat(nrow, ncol)
		for (i <- 0 until nrow; j <- 0 until ncol) {
			A.re(i)(j) = re(ii(i) - basex)(j + jj1 - basex)
			A.im(i)(j) = im(ii(i) - basex)(j + jj1 - basex)
		}
		A
	}

	def put(ii: Array[Int],
			jj1: Int,
			jj2: Int,
			A: Zmat) {
		dirty = true
		val nrow = ii.length
		val ncol = jj2 - jj1 + 1
		for (i <- 0 until nrow; j <- 0 until ncol) {
			re(ii(i) - basex)(j + jj1 - basex) = A.re(i)(j)
			im(ii(i) - basex)(j + jj1 - basex) = A.im(i)(j)
		}
	}

	def get(ii1: Int, ii2: Int, jj: Array[Int]): Zmat = {
		val nrow = ii2 - ii1 + 1
		val ncol = jj.length
		val A = new Zmat(nrow, ncol)
		for (i <- 0 until nrow; j <- 0 until ncol) {
			A.re(i)(j) = re(i + ii1 - basex)(jj(j) - basex)
			A.im(i)(j) = im(i + ii1 - basex)(jj(j) - basex)
		}
		A
	}

	def put(ii1: Int,
			ii2: Int,
			jj: Array[Int],
			A: Zmat) {
		dirty = true
		val nrow = ii2 - ii1 + 1
		val ncol = jj.length
		for (i <- 0 until nrow; j <- 0 until ncol) {
			re(i + ii1 - basex)(jj(j) - basex) = A.re(i)(j)
			im(i + ii1 - basex)(jj(j) - basex) = A.im(i)(j)
		}
	}

	def get(ii: Array[Int], jj: Array[Int]): Zmat = {
		val nrow = ii.length
		val ncol = jj.length
		val A = new Zmat(nrow, ncol)
		for (i <- 0 until nrow; j <- 0 until ncol) {
			A.re(i)(j) = re(ii(i) - basex)(jj(j) - basex)
			A.im(i)(j) = im(ii(i) - basex)(jj(j) - basex)
		}
		A
	}

	def put(ii: Array[Int], jj: Array[Int], A: Zmat) {
		dirty = true
		val nrow = ii.length
		val ncol = jj.length
		for (i <- 0 until nrow; j <- 0 until ncol) {
			re(ii(i) - basex)(jj(j) - basex) = A.re(i)(j)
			im(ii(i) - basex)(jj(j) - basex) = A.im(i)(j)
		}
	}

	def getLU: Zludpp = {
		clean()
		val temp = LU
		LU = null
		temp
	}

	def getHQR: Zhqrd = {
		clean()
		val temp = HQR
		HQR = null
		temp
	}

	def getCHOL: Zchol = {
		clean()
		val temp = CHOL
		CHOL = null
		temp
	}

	def clean() {
		if (dirty) {
			LU = null
			HQR = null
			CHOL = null
			dirty = false
		}
	}

	def solve(vector: Zmat): Zmat = Solve.aib(this, vector)

	def eig(): Eig = new Eig(this)

	def qr(): Zhqrd = new Zhqrd(this)

	def vectorNorm: Double = {
		require(ncol == 1, "Matrix must be a cloumn vector.")
		math.sqrt(getZ.flatten.map(i => (i * i.conj).re).sum)
	}

	def conj: Zmat = {
		new Zmat(re, im.map(_.map(-_)))
	}

	def transpose: Zmat = {
		val m = new Zmat(re.head.length, re.length)
		for (i <- re.indices; j <- re.head.indices) {
			m.re(j)(i) = re(i)(j)
			m.im(j)(i) = im(i)(j)
		}
		m
	}

	def rank: Int = new JamaSVD(this).rank()

	def pinv: Zmat = {
		val svd = new JamaSVD(this)
		svd.V * svd.S.pinv * svd.U.conj.transpose
	}

	override def clone(): Zmat = {
		new Zmat(re, im)
	}

	override def toString: String = {
		Print.o(this)
	}

	def inv: Zmat = Inv.o(this)

	def *(that: Z) = Times.o(that, this)

	def *(that: Zmat) = Times.o(this, that)

	def *(that: Zdiagmat) = Times.o(this, that)

	def /(that: Double) = new Zmat(re.map(_.map(_ / that)), im.map(_.map(_ / that)))

	def +(that: Zmat) = Plus.o(this, that)

	def +(that: Zdiagmat) = Plus.o(this, that)

	def -(that: Zmat) = Minus.o(this, that)

	def -(that: Zdiagmat) = Minus.o(this, that)

	def unary_- = Minus.o(this)

	def round(precision: Int = 0): Zmat = {
		val factor = math.pow(10, precision)
		new Zmat(re.map(_.map(d => (d * factor).round / factor)), im.map(_.map(d => (d * factor).round / factor)))
	}

	override def equals(obj: scala.Any): Boolean = obj match {
		case _: Zmat =>
			re.flatten.sameElements(obj.asInstanceOf[Zmat].re.flatten[Double]) &&
				im.flatten.sameElements(obj.asInstanceOf[Zmat].im.flatten[Double])
		case _ => false
	}
}

object Zmat {

	def Eye(n: Int) = JampackNew.Eye.o(n)

	def Eye(m: Int, n: Int) = JampackNew.Eye.o(m, n)

	def random(n: Int) = Rand.uzmat(n, n)

	def random(m: Int, n: Int) = Rand.uzmat(m, n)
}

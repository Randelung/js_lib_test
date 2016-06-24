package JampackNew

import scala.language.implicitConversions

class Zdiagmat private() {

	var order: Int = _

	var basex: Int = _

	var re: Array[Double] = _

	var im: Array[Double] = _

	var n: Int = _

	var bx: Int = _

	var dx: Int = _

	Parameters.BaseIndexNotChangeable = true

	loadProperties()

	def this(order: Int) {
		this()
		Parameters.BaseIndexNotChangeable = true
		basex = Parameters.BaseIndex
		this.order = order
		loadProperties()
		re = Array.ofDim[Double](n)
		im = Array.ofDim[Double](n)
	}

	def this(order: Int, `val`: Z) {
		this()
		Parameters.BaseIndexNotChangeable = true
		basex = Parameters.BaseIndex
		this.order = order
		loadProperties()
		re = Array.ofDim[Double](n)
		im = Array.ofDim[Double](n)
		for (i <- 0 until n) {
			re(i) = `val`.re
			im(i) = `val`.im
		}
	}

	def this(`val`: Z1) {
		this()
		Parameters.BaseIndexNotChangeable = true
		bx = Parameters.BaseIndex
		order = `val`.re.length
		loadProperties()
		re = Array.ofDim[Double](n)
		im = Array.ofDim[Double](n)
		for (i <- 0 until n) {
			re(i) = `val`.re(i)
			im(i) = `val`.im(i)
		}
	}

	def this(re: Array[Double], im: Array[Double]) {
		this()
		require(re.length == im.length, "Array sizes must agree.")
		Parameters.BaseIndexNotChangeable = true
		bx = Parameters.BaseIndex
		order = re.length
		loadProperties()
		this.re = re.clone()
		this.im = im.clone()
	}

	def this(A: Zmat, _k: Int) {
		this()
		var k = _k
		Parameters.BaseIndexNotChangeable = true
		basex = Parameters.BaseIndex
		if (k >= 0) {
			if (k >= A.ncol) {
				throw new JampackException("Diagonal out of range.")
			}
			order = Math.min(A.nrow, A.ncol - k)
			re = Array.ofDim[Double](order)
			im = Array.ofDim[Double](order)
			for (i <- 0 until order) {
				re(i) = A.re(i)(i + k)
				im(i) = A.im(i)(i + k)
			}
		}
		else {
			k = -k
			if (k >= A.nrow) {
				throw new JampackException("Diagonal out of range.")
			}
			order = Math.min(A.nrow - k, A.ncol)
			re = Array.ofDim[Double](order)
			im = Array.ofDim[Double](order)
			for (i <- 0 until order) {
				re(i) = A.re(i + k)(i)
				im(i) = A.im(i + k)(i)
			}
		}
		loadProperties()
	}

	def this(A: Zmat) {
		this(A, 0)
	}

	def this(D: Zdiagmat) {
		this()
		Parameters.BaseIndexNotChangeable = true
		basex = Parameters.BaseIndex
		order = D.order
		loadProperties()
		re = Array.ofDim[Double](n)
		im = Array.ofDim[Double](n)
		for (i <- 0 until n) {
			re(i) = D.re(i)
			im(i) = D.im(i)
		}
	}

	def loadProperties(): Unit = {
		bx = basex
		dx = bx + order - 1
		n = order
	}

	def get(ii: Int): Z = new Z(re(ii - bx), im(ii - bx))

	def get0(i: Int): Z = new Z(re(i), im(i))

	def put(ii: Int, `val`: Z) {
		re(ii - bx) = `val`.re
		im(ii - bx) = `val`.im
	}

	def put0(i: Int, `val`: Z) {
		re(i) = `val`.re
		im(i) = `val`.im
	}

	def round(precision: Int): Zdiagmat = {
		val factor = math.pow(10, precision)
		val d = new Zdiagmat(this)
		d.re = re.map(i => (i * factor).round / factor)
		d.im = im.map(i => (i * factor).round / factor)
		d
	}

	def pinv: Zdiagmat = {
		new Zdiagmat(re.map(i => if ((i * 1e5).round != 0) 1 / i else 0), im.map(i => if ((i * 1e5).round != 0) 1 / i else 0))
	}

	def unary_- = new Zdiagmat(re.map(-_), im.map(-_))

	override def toString: String = Print.o(this)

	override def equals(obj: scala.Any): Boolean = obj match {
		case _: Zdiagmat =>
			obj.asInstanceOf[Zdiagmat].re.sameElements(re) && obj.asInstanceOf[Zdiagmat].im.sameElements(im)
		case _ => false
	}

	override def clone: Zdiagmat = new Zdiagmat(this)
}

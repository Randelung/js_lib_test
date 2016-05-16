package JampackNew

import scala.util.Random

object Rand {

	private val R: Random = new Random()

	def setSeed(seed: Long) {
		R.setSeed(seed)
	}

	def ud(): Double = R.nextDouble()

	def udary(n: Int): Array[Double] = {
		val d = Array.ofDim[Double](n)
		for (i <- 0 until n) {
			d(i) = R.nextDouble()
		}
		d
	}

	def udary(m: Int, n: Int): Array[Array[Double]] = {
		val d = Array.ofDim[Double](m, n)
		for (i <- 0 until m; j <- 0 until n) {
			d(i)(j) = R.nextDouble()
		}
		d
	}

	def uz(): Z = new Z(R.nextDouble(), R.nextDouble())

	def uz1(n: Int): Z1 = {
		val zone = new Z1(n)
		for (i <- 0 until n) {
			zone.re(i) = R.nextDouble()
			zone.im(i) = R.nextDouble()
		}
		zone
	}

	def uzmat(m: Int, n: Int): Zmat = {
		val zm = new Zmat(m, n)
		for (i <- 0 until m; j <- 0 until n) {
			zm.re(i)(j) = R.nextDouble()
			zm.im(i)(j) = R.nextDouble()
		}
		zm
	}

	def nd(): Double = R.nextGaussian()

	def ndary(n: Int): Array[Double] = {
		val d = Array.ofDim[Double](n)
		for (i <- 0 until n) {
			d(i) = R.nextGaussian()
		}
		d
	}

	def ndary(m: Int, n: Int): Array[Array[Double]] = {
		val d = Array.ofDim[Double](m, n)
		for (i <- 0 until m; j <- 0 until n) {
			d(i)(j) = R.nextGaussian()
		}
		d
	}

	def nz(): Z = {
		new Z(R.nextGaussian(), R.nextGaussian())
	}

	def nz1(n: Int): Z1 = {
		val zone = new Z1(n)
		for (i <- 0 until n) {
			zone.re(i) = R.nextGaussian()
			zone.im(i) = R.nextGaussian()
		}
		zone
	}

	def nzmat(m: Int, n: Int): Zmat = {
		val zm = new Zmat(m, n)
		for (i <- 0 until m; j <- 0 until n) {
			zm.re(i)(j) = R.nextGaussian()
			zm.im(i)(j) = R.nextGaussian()
		}
		zm
	}
}

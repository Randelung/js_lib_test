package ch.ethz.ipes.buschr.maths.root

/** Basic Newton-Raphson algorithm to find a root using a function and its derivative.
  *
  */
object NewtonRaphson {

	def findRoot(f: (Double => Double), df: (Double => Double), x0: Double = 0, maxIterations: Int = 50): Double = {

		var x = x0

		for (i <- 0 until maxIterations) {

			val fx = f(x)
			if (fx.abs < 1e-10) {
				return x
			}
			x = x - fx / df(x)
		}

		throw MaximumIterationsReachedException()
	}

	case class MaximumIterationsReachedException(message: String = "") extends Exception(message)

}

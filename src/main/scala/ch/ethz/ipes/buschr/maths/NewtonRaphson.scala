package ch.ethz.ipes.buschr.maths

/**
  * Created by Randolph Busch on 13/07/16.
  */
object NewtonRaphson {

	def findRoot(f: (Double => Double), df: (Double => Double), x0: Double = 0, maxIterations: Int = 50): Double = {

		var x = x0

		for (i <- 0 until maxIterations) {

			val fx = f(x)
			if (fx < 1e-10)
				return x
			x = x - fx / df(x)
		}

		throw MaximumIterationsReachedException()
	}

	case class MaximumIterationsReachedException(message: String = "") extends Exception(message)

}

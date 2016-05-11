package ch.ethz.ipes.buschr.schematics.components

/**
  * Created by Randolph Busch on 27/04/16.
  */
class Inductor(var source: (Int, Int), var sink: (Int, Int), var inductance: Double) extends Component {}

object Inductor {

	def apply(source: (Int, Int), sink: (Int, Int), inductance: Double) = new Inductor(source, sink, inductance)
}

package ch.ethz.ipes.buschr.schematics.components

/**
  * Created by Randolph Busch on 27/04/16.
  */
class Resistor(var source: (Int, Int), var sink: (Int, Int), var resistance: Double) extends Component {}

object Resistor {

	def apply(source: (Int, Int), sink: (Int, Int), resistance: Double) = new Resistor(source, sink, resistance)
}

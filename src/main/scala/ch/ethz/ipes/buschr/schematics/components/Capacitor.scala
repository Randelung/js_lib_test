package ch.ethz.ipes.buschr.schematics.components

/**
  * Created by Randolph Busch on 27/04/16.
  */
class Capacitor(var source: (Int, Int), var sink: (Int, Int), var capacitance: Double) extends Component {}

object Capacitor {

	def apply(source: (Int, Int), sink: (Int, Int), capacitance: Double) = new Capacitor(source, sink, capacitance)
}

package ch.ethz.ipes.buschr.maths

import ch.ethz.ipes.buschr.schematics.components.Component

/**
  * Created by Randolph Busch on 27/04/16.
  */
class MNA {

	val nodes = List.empty[(Int, Int)]

	def addComponent(component: Component): Unit = {
		if (nodes contains component.source) {

		}
	}
}

package ch.ethz.ipes.buschr.schematics

import ch.ethz.ipes.buschr.schematics.components.Component

import scala.collection.mutable

/**
  * Root element for a sheet of a schematic.
  * Possibly a future super element will enable linking of multiple sheets using node IDs.
  *
  * Created by Randolph Busch on 27/04/16.
  */
class Sheet {

	val componentList = mutable.Buffer.empty[Component]

	def clear(): Unit = {
		componentList.clear()
	}

	def addComponent(component: Component): Unit = {
		componentList += component
	}

	def removeComponent(component: Component): Unit = {
		componentList -= component
	}

	def addComponents(components: List[Component]): Unit = {
		componentList ++= components
	}

	def getComponentsBySourceNode(xyCoordinates: (Int, Int)): List[Component] = {
		componentList.filter(c => c.source._1 == xyCoordinates._1 && c.source._2 == xyCoordinates._2).toList
	}

	def getComponentsBySinkNode(xyCoordinates: (Int, Int)): List[Component] = {
		componentList.filter(c => c.sink._1 == xyCoordinates._1 && c.sink._2 == xyCoordinates._2).toList
	}

	def getComponentsByNode(xyCoordinates: (Int, Int)): List[Component] = {
		getComponentsBySinkNode(xyCoordinates) ++ getComponentsBySourceNode(xyCoordinates)
	}

	def getComponents: List[Component] = {
		componentList.toList
	}
}

object Sheet {

	def apply = new Sheet()

	def apply(component: Component) = {
		val sheet = new Sheet()
		sheet.addComponent(component)
		sheet
	}

	def apply(components: List[Component]) = {
		val sheet = new Sheet()
		sheet.addComponents(components)
		sheet
	}
}

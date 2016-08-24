package ch.ethz.ipes.buschr.schematics

import ch.ethz.ipes.buschr.maths.MNA
import ch.ethz.ipes.buschr.maths.MNA.NetList
import org.scalajs.dom.{Event, XMLHttpRequest, html}

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSExport
import scala.util.control.Breaks._
import scalatags.JsDom.all._

/** Demo application that creates two canvasses, one for the circuit illustration, one for the plot.
  *
  * @param divCircuit A div element inside which the circuit illustration is to be done
  * @param urlNetlist The url to the netlist so that the XHR will recognise its location
  * @param divGraph A div element inside which the graph plot is to be done
  * @param urlElementMap The url to the element map so that the XHR will ecognise its location
  */
@JSExport
class CircuitAnalyzer(divCircuit: html.Div, urlNetlist: String, divGraph: html.Div = null, urlElementMap: String = null) {
	if (divGraph != null) require(urlElementMap != null, "divGraph and urlNodemap must be given together.")
	if (urlElementMap != null) require(divGraph != null, "divGraph and urlNodemap must be given together.")
	require(divCircuit != null && urlNetlist != null, "Arguments can't be null.")

	// create canvasses
	val canvasCircuit = canvas(id := "canvasCircuit")("Get a proper browser!").render
	val canvasGraph = canvas(id := "canvasGraph")("Get a proper browser!").render

	// create XHR object
	val xhr = new XMLHttpRequest()
	xhr.onreadystatechange = (e: Event) => {

		if (xhr.readyState == 4 && xhr.status == 200) {
			CircuitAnalyzer.netlist = NetList.fromJSON(xhr.responseText)
			if (urlElementMap != null) {
				xhr.onreadystatechange = (event: Event) => {

					if (xhr.readyState == 4 && xhr.status == 200) {
						val (gridWidth, gridHeight, elementMap, nodeMap) = CircuitAnalyzer.elementMapFromJSON(xhr.responseText)
						CircuitIllustrator.drawCircuit(canvasCircuit, gridWidth, gridHeight, elementMap, nodeMap, CircuitAnalyzer.netlist)
					}
				}
				// fetch element map
				xhr.open("GET", urlElementMap, async = true)
				xhr.send()
			}
			// parse JSON into netlist and MNA
			CircuitAnalyzer.MNATreeRoot = CircuitAnalyzer.MNATree(CircuitAnalyzer.netlist, diodeConducting = null, diodeBlocking = null)

			// create all combinations of open/closed diodes
			CircuitAnalyzer.buildTree(CircuitAnalyzer.MNATreeRoot)

			try {
				CircuitAnalyzer.graph = GraphPlotter(canvasGraph, CircuitAnalyzer.MNATreeRoot, 8 * math.Pi)
			}
			catch {
				case e: Exception => Console.err.println(e.getMessage)
			}
		}
	}
	// fetch netlist
	xhr.open("GET", urlNetlist, async = true)
	xhr.send()

	// put circuit canvas in webpage and set coordinate system
	divCircuit.appendChild(canvasCircuit)
	canvasCircuit.style.height = divCircuit.style.maxHeight
	canvasCircuit.style.width = divCircuit.style.maxWidth
	canvasCircuit.height = canvasCircuit.getBoundingClientRect().height.toInt
	canvasCircuit.width = canvasCircuit.getBoundingClientRect().width.toInt
	// put graph canvas into webpage and set coordinate system
	if (divGraph != null) {
		divGraph.appendChild(canvasGraph)
		canvasGraph.style.height = divGraph.style.maxHeight
		canvasGraph.style.width = divCircuit.style.maxWidth
		canvasGraph.height = canvasGraph.getBoundingClientRect().height.toInt
		canvasGraph.width = canvasGraph.getBoundingClientRect().width.toInt
	}
}

/** Convenience methods to allow external access to functionality. Object is available as "button". */
@JSExport("button")
object CircuitAnalyzer {

	@JSExport
	var graph: GraphPlotter = _
	var netlist: NetList = _
	var MNATreeRoot: MNATree = _

	/** parse element map from JSON to a map from element to corner positions.
	  *
	  * @param json String to parse
	  * @return
	  */
	def elementMapFromJSON(json: String): (Int, Int, Map[String, (Int, Int, Int, Int)], Map[Int, js.Array[(Int, Int)]]) = {

		val parsedJSON = JSON.parse(json)
		var elementMap = Map[String, (Int, Int, Int, Int)]()
		parsedJSON.map.asInstanceOf[js.Array[js.Dynamic]].foreach(i => {
			elementMap += i.elementName.asInstanceOf[String] ->
				(i.startX.asInstanceOf[Int], i.startY.asInstanceOf[Int], i.endX.asInstanceOf[Int], i.endY.asInstanceOf[Int])
		})
		var nodeMap = Map[Int, js.Array[(Int, Int)]]()
		parsedJSON.spreadNodes.asInstanceOf[js.Array[js.Dynamic]].foreach(i => {
			nodeMap += i.node.asInstanceOf[Int] ->
				i.locations.asInstanceOf[js.Array[js.Dynamic]].map(j => (j.x.asInstanceOf[Int], j.y.asInstanceOf[Int]))
		})
		(parsedJSON.grid.width.asInstanceOf[Int], parsedJSON.grid.height.asInstanceOf[Int], elementMap, nodeMap)
	}

	/** Holding class for a tree to keep track of diode replacements
	  *
	  * @param netlist         Netlist pertaining to the MNA
	  * @param diodeConducting Child with the first diode replaced by a low resistor
	  * @param diodeBlocking   Child with the first diode replaced by a high resistor
	  */
	case class MNATree(netlist: NetList, var diodeConducting: MNATree, var diodeBlocking: MNATree) {

		lazy val mna = new MNA(netlist)
	}

	/** New way to analyze diodes by using static state information for each state (blocking or conducting).
	  *
	  * The new approach uses 0 resistance and open connections instead of 1e15 and 1e-15 ohm resistors. An internalDiode
	  * class tracks the current state of each diode and allows thus addition in the MNA. This improves matrix size and
	  * thus speed, besides getting rid of one approximation.
	  *
	  * @param currentNode Root of the tree to be created.
	  */
	private def buildTree(currentNode: CircuitAnalyzer.MNATree): Unit = {

		// find first non-internalDiode diode to replace
		var i = 0
		breakable {
			while (i < currentNode.netlist.diodes.length) {
				if (!currentNode.netlist.diodes(i).isInstanceOf[MNA.InternalDiode]) {
					break
				}

				i += 1
			}
		}
		if (i == netlist.diodes.length) {
			return
		}

		val netlistConducting = currentNode.netlist.copy
		val netlistBlocking = currentNode.netlist.copy
		netlistConducting.diodes(i) = MNA.InternalDiode(currentNode.netlist.diodes(i).name, currentNode.netlist.diodes(i).startNode,
			currentNode.netlist.diodes(i).endNode, conducting = true)
		netlistBlocking.diodes(i) = MNA.InternalDiode(currentNode.netlist.diodes(i).name, currentNode.netlist.diodes(i).startNode,
			currentNode.netlist.diodes(i).endNode, conducting = false)
		currentNode.diodeBlocking = CircuitAnalyzer.MNATree(netlistBlocking, null, null)
		currentNode.diodeConducting = CircuitAnalyzer.MNATree(netlistConducting, null, null)
		buildTree(currentNode.diodeConducting)
		buildTree(currentNode.diodeBlocking)
	}

}

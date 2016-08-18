package ch.ethz.ipes.buschr.schematics

import ch.ethz.ipes.buschr.maths.MNA
import ch.ethz.ipes.buschr.maths.MNA.NetList
import org.scalajs.dom.{Event, XMLHttpRequest, html}

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSExport
import scala.util.control.Breaks._
import scalatags.JsDom.all._

@JSExport
class CircuitAnalyzer(divCircuit: html.Div, urlNetlist: String, divGraph: html.Div = null, urlNodemap: String = null) {
	if (divGraph != null) require(urlNodemap != null, "divGraph and urlNodemap must be given together.")
	if (urlNodemap != null) require(divGraph != null, "divGraph and urlNodemap must be given together.")
	require(divCircuit != null && urlNetlist != null, "Arguments can't be null.")

	val canvasCircuit = canvas(id := "canvasCircuit")("Get a proper browser!").render
	val canvasGraph = canvas(id := "canvasGraph")("Get a proper browser!").render

	val xhr = new XMLHttpRequest()
	xhr.onreadystatechange = (e: Event) => {

		if (xhr.readyState == 4 && xhr.status == 200) {
			CircuitAnalyzer.netlist = NetList.fromJSON(xhr.responseText)
			if (urlNodemap != null) {
				xhr.onreadystatechange = (event: Event) => {

					if (xhr.readyState == 4 && xhr.status == 200) {
						val (gridWidth, gridHeight, elementMap, nodeMap) = CircuitAnalyzer.elementMapFromJSON(xhr.responseText)
						CircuitIllustrator.drawCircuit(canvasCircuit, gridWidth, gridHeight, elementMap, nodeMap, CircuitAnalyzer.netlist)
					}
				}
				xhr.open("GET", urlNodemap, async = true)
				xhr.send()
			}
			CircuitAnalyzer.MNATreeRoot = CircuitAnalyzer.MNATree(CircuitAnalyzer.netlist, diodeConducting = null, diodeBlocking = null)

			CircuitAnalyzer.buildTree(CircuitAnalyzer.MNATreeRoot)

			try {
				CircuitAnalyzer.graph = GraphPlotter(canvasGraph, CircuitAnalyzer.MNATreeRoot, 8 * math.Pi)
			}
			catch {
				case e: Exception => e.printStackTrace()
			}
		}
	}
	xhr.open("GET", urlNetlist, async = true)
	xhr.send()

	divCircuit.appendChild(canvasCircuit)
	canvasCircuit.style.height = divCircuit.style.maxHeight
	canvasCircuit.style.width = divCircuit.style.maxWidth
	canvasCircuit.height = canvasCircuit.getBoundingClientRect().height.toInt
	canvasCircuit.width = canvasCircuit.getBoundingClientRect().width.toInt
	if (divGraph != null) {
		divGraph.appendChild(canvasGraph)
		canvasGraph.style.height = divGraph.style.maxHeight
		canvasGraph.style.width = divCircuit.style.maxWidth
		canvasGraph.height = canvasGraph.getBoundingClientRect().height.toInt
		canvasGraph.width = canvasGraph.getBoundingClientRect().width.toInt
	}
}

@JSExport("button")
object CircuitAnalyzer {

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
	  */

	/** Creates a tree of diode replacements starting with the given node.
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
		/*netlistConducting.resistors += Resistor(netlistConducting.diodes.head.name, netlistConducting.diodes.head.startNode,
			netlistConducting.diodes.head.endNode, 0)
		netlistBlocking.resistors += Resistor(netlistBlocking.diodes.head.name, netlistBlocking.diodes.head.startNode,
			netlistBlocking.diodes.head.endNode, 1e15)
		netlistConducting.diodes.remove(0)
		netlistBlocking.diodes.remove(0)*/
		currentNode.diodeBlocking = CircuitAnalyzer.MNATree(netlistBlocking, null, null)
		currentNode.diodeConducting = CircuitAnalyzer.MNATree(netlistConducting, null, null)
		buildTree(currentNode.diodeConducting)
		buildTree(currentNode.diodeBlocking)
	}

	private var plotStatesVoltages = Map[String, Boolean]()

	@JSExport
	def plotVoltageButton(name: String) = {

		try {
			plotStatesVoltages += name -> (plotStatesVoltages(name) match {
				case true =>
					graph.disableFunction("u_" + name)
					false
				case false =>
					graph.enableFunction("u_" + name)
					true
			})
		}
		catch {
			case _: NoSuchElementException =>
				graph.plotElementVoltage(name)
				plotStatesVoltages += name -> true
		}
	}

	private var plotStatesCurrents = Map[String, Boolean]()

	@JSExport
	def plotCurrentButton(name: String) = {

		try {
			plotStatesCurrents += name -> (plotStatesCurrents(name) match {
				case true =>
					graph.disableFunction("i_" + name)
					false
				case false =>
					graph.enableFunction("i_" + name)
					true
			})
		}
		catch {
			case _: NoSuchElementException =>
				graph.plotElementCurrent(name)
				plotStatesCurrents += name -> true
		}
	}

	private var plotStatesDerivedVoltages = Map[String, Boolean]()

	@JSExport
	def plotDerivedVoltageButton(name: String) = {

		try {
			plotStatesDerivedVoltages += name -> (plotStatesDerivedVoltages(name) match {
				case true =>
					graph.disableFunction("du_" + name + "/dt")
					false
				case false =>
					graph.enableFunction("du_" + name + "/dt")
					true
			})
		}
		catch {
			case _: NoSuchElementException =>
				graph.plotDerivedElementVoltage(name)
				plotStatesDerivedVoltages += name -> true
		}
	}

	private var plotStatesDerivedCurrents = Map[String, Boolean]()

	@JSExport
	def plotDerivedCurrentButton(name: String) = {

		try {
			plotStatesDerivedCurrents += name -> (plotStatesDerivedCurrents(name) match {
				case true =>
					graph.disableFunction("di_" + name + "/dt")
					false
				case false =>
					graph.enableFunction("di_" + name + "/dt")
					true
			})
		}
		catch {
			case _: NoSuchElementException =>
				graph.plotDerivedElementCurrent(name)
				plotStatesDerivedCurrents += name -> true
		}
	}

}

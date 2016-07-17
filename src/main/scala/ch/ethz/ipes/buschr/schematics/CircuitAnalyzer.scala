package ch.ethz.ipes.buschr.schematics

import ch.ethz.ipes.buschr.maths.MNA
import ch.ethz.ipes.buschr.maths.MNA.{NetList, Resistor}
import org.scalajs.dom.{Event, XMLHttpRequest, html}

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSExport
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

			CircuitAnalyzer.graph = GraphPlotter(canvasGraph, CircuitAnalyzer.MNATreeRoot, 8 * math.Pi)
		}
	}
	xhr.open("GET", urlNetlist, async = true)
	xhr.send()

	divCircuit.appendChild(canvasCircuit)
	canvasCircuit.style.height = divCircuit.style.maxHeight
	canvasCircuit.style.width = divCircuit.style.maxWidth
	canvasCircuit.height = canvasCircuit.getBoundingClientRect().height.toInt
	canvasCircuit.width = canvasCircuit.getBoundingClientRect().width.toInt
	divGraph.appendChild(canvasGraph)
	canvasGraph.style.height = divGraph.style.maxHeight
	canvasGraph.style.width = divCircuit.style.maxWidth
	canvasGraph.height = canvasGraph.getBoundingClientRect().height.toInt
	canvasGraph.width = canvasGraph.getBoundingClientRect().width.toInt
}

@JSExport("button")
object CircuitAnalyzer {

	var mna: MNA = _
	var graph: GraphPlotter = _
	var netlist: NetList = _
	var MNATreeRoot: MNATree = _

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

	case class MNATree(netlist: NetList, var diodeConducting: MNATree, var diodeBlocking: MNATree) {

		lazy val mna = new MNA(netlist)
	}

	def buildTree(currentNode: CircuitAnalyzer.MNATree): Unit = {

		if (currentNode.netlist.diodes.nonEmpty) {
			val netlistConducting = currentNode.netlist.copy
			val netlistBlocking = currentNode.netlist.copy
			netlistConducting.resistors += Resistor(netlistConducting.diodes.head.name, netlistConducting.diodes.head.startNode,
				netlistConducting.diodes.head.endNode, 1e-15)
			netlistBlocking.resistors += Resistor(netlistBlocking.diodes.head.name, netlistBlocking.diodes.head.startNode,
				netlistBlocking.diodes.head.endNode, 1e15)
			netlistConducting.diodes.remove(0)
			netlistBlocking.diodes.remove(0)
			currentNode.diodeBlocking = CircuitAnalyzer.MNATree(netlistBlocking, null, null)
			currentNode.diodeConducting = CircuitAnalyzer.MNATree(netlistConducting, null, null)
			buildTree(currentNode.diodeConducting)
			buildTree(currentNode.diodeBlocking)
		}
	}

	private var button1pressed = 0

	@JSExport
	def button1(): Unit = {
		button1pressed = button1pressed match {
			case 0 =>
				CircuitAnalyzer.graph.plotElementVoltage(netlist.capacitors.head.name)
				1
			case 1 =>
				graph.disableFunction("u_" + netlist.capacitors.head.name)
				2
			case 2 =>
				graph.enableFunction("u_" + netlist.capacitors.head.name)
				1
		}
	}

	private var button2pressed = 0

	@JSExport
	def button2(): Unit = {
		button2pressed = button2pressed match {
			case 0 =>
				CircuitAnalyzer.graph.plotElementCurrent(netlist.capacitors.head.name)
				1
			case 1 =>
				graph.disableFunction("i_" + netlist.capacitors.head.name)
				2
			case 2 =>
				graph.enableFunction("i_" + netlist.capacitors.head.name)
				1
		}
	}

	private var button3pressed = 0

	@JSExport
	def button3(): Unit = {
		button3pressed = button3pressed match {
			case 0 =>
				CircuitAnalyzer.graph.plotElementVoltage(netlist.inductors.head.name)
				1
			case 1 =>
				graph.disableFunction("u_" + netlist.inductors.head.name)
				2
			case 2 =>
				graph.enableFunction("u_" + netlist.inductors.head.name)
				1
		}
	}

	private var button4pressed = 0

	@JSExport
	def button4(): Unit = {
		button4pressed = button4pressed match {
			case 0 =>
				CircuitAnalyzer.graph.plotElementCurrent(netlist.inductors.head.name)
				1
			case 1 =>
				graph.disableFunction("i_" + netlist.inductors.head.name)
				2
			case 2 =>
				graph.enableFunction("i_" + netlist.inductors.head.name)
				1
		}
	}

	private var button5pressed = 0

	@JSExport
	def button5(): Unit = {
		button5pressed = button5pressed match {
			case 0 =>
				CircuitAnalyzer.graph.plotDerivedElementVoltage(netlist.inductors.head.name)
				1
			case 1 =>
				graph.disableFunction("du_" + netlist.inductors.head.name + "/dt")
				2
			case 2 =>
				graph.enableFunction("du_" + netlist.inductors.head.name + "/dt")
				1
		}
	}

	private var button6pressed = 0

	@JSExport
	def button6(): Unit = {
		button6pressed = button6pressed match {
			case 0 =>
				CircuitAnalyzer.graph.plotDerivedElementCurrent(netlist.inductors.head.name)
				1
			case 1 =>
				graph.disableFunction("di_" + netlist.inductors.head.name + "/dt")
				2
			case 2 =>
				graph.enableFunction("di_" + netlist.inductors.head.name + "/dt")
				1
		}
	}

	private var button7pressed = 0

	@JSExport
	def button7(): Unit = {
		button7pressed = button7pressed match {
			case 0 =>
				CircuitAnalyzer.graph.plotElementVoltage(netlist.diodes.head.name)
				1
			case 1 =>
				graph.disableFunction(s"u_${netlist.diodes.head.name}")
				2
			case 2 =>
				graph.enableFunction(s"u_${netlist.diodes.head.name}")
				1
		}
	}

	private var button8pressed = 0

	@JSExport
	def button8(): Unit = {
		button8pressed = button8pressed match {
			case 0 =>
				CircuitAnalyzer.graph.plotElementCurrent(netlist.diodes.head.name)
				1
			case 1 =>
				graph.disableFunction(s"i_${netlist.diodes.head.name}")
				2
			case 2 =>
				graph.enableFunction(s"i_${netlist.diodes.head.name}")
				1
		}
	}

	private var button9pressed = 0

	@JSExport
	def button9(): Unit = {
		button9pressed = button9pressed match {
			case 0 =>
				CircuitAnalyzer.graph.plotElementVoltage(netlist.resistors.head.name)
				1
			case 1 =>
				graph.disableFunction(s"i_${netlist.diodes.head.name}")
				2
			case 2 =>
				graph.enableFunction(s"i_${netlist.diodes.head.name}")
				1
		}
	}

}

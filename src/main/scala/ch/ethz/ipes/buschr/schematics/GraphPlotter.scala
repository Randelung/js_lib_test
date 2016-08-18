package ch.ethz.ipes.buschr.schematics

import ch.ethz.ipes.buschr.maths.MNA.InternalDiode
import ch.ethz.ipes.buschr.maths.root.NewtonRaphson
import ch.ethz.ipes.buschr.maths.root.NewtonRaphson.MaximumIterationsReachedException
import ch.ethz.ipes.buschr.maths.{MNA, Vector2D}
import org.scalajs.dom._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by Randolph Busch on 03/07/16.
  */
class GraphPlotter(canvas: html.Canvas, mnaTreeRoot: CircuitAnalyzer.MNATree, startX: Double, endX: Double, startY: Double, endY: Double, autoAdjustY: Boolean) {
	require(startX < endX, "Graphing range must be greater than 0.")
	require(autoAdjustY || startY < endY, "Graphing range must be greater than 0.")

	private val context = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
	private val topMargin = 20
	private val rightMargin = 20
	private val leftMargin = 50
	private val bottomMargin = 30
	private val pixelWidth = canvas.width - leftMargin - rightMargin
	private val unitWidth = endX - startX
	private val pixelsPerUnit = pixelWidth / unitWidth
	private val mnaTracker = Array.ofDim[MNA](pixelWidth)
	private val stepSize = unitWidth / pixelWidth

	private var zeroVector = Vector2D(leftMargin, canvas.height - bottomMargin)
	private var maxValue = 1d
	private var minValue = 0d
	private var results = ArrayBuffer[Array[Double]]()
	private var colors = ArrayBuffer[String]()
	private var names = ArrayBuffer[String]()
	private var enabled = ArrayBuffer[Boolean]()
	private var _nextDefaultColor = 0

	context.lineWidth = 2
	context.lineCap = "square"
	context.font = "12px Arial"

	// find first stable state. Can't just assume any diode state as it might separate the circuit.
	private var t = 0d
	private val currentMNAmask = Array.ofDim[Boolean](mnaTreeRoot.netlist.diodes.length)
	private var currentMNA: MNA = _
	println(s"t = $t")

	private var markedDiodes = ListBuffer[Int]()
	mnaTreeRoot.netlist.diodes.indices.copyToBuffer(markedDiodes)

	if (markedDiodes.isEmpty) {
		mnaTracker(0) = mnaTreeRoot.mna
		try {
			mnaTracker(0).applyStartingConditions(null)
		}
		catch {
			case e: Exception => println(s"Failed to apply starting conditions: ${e.getMessage}")
		}
	}
	else {
		println(s"Added all diodes to list of marked diodes as initialization.")

		var possibleMNAs = ListBuffer[MNA]()
		for (j <- 0 until 1 << markedDiodes.length) {
			var currentTree = mnaTreeRoot
			var shift = 0
			for (k <- currentMNAmask.indices) {
				if (!markedDiodes.contains(k)) {
					currentTree = if (currentMNAmask(k)) currentTree.diodeConducting else currentTree.diodeBlocking
				}
				else {
					currentTree = if (((j >> shift) & 1) == 1) currentTree.diodeConducting else currentTree.diodeBlocking
					shift += 1
				}
			}
			try {
				currentTree.mna.applyStartingConditions(null)

				if (markedDiodes.forall(k => {
					((currentTree.mna.elementCurrent(currentTree.mna.netlist(mnaTreeRoot.netlist.diodes(k).name), 0) >= 0) &&
						currentTree.mna.netlist(mnaTreeRoot.netlist.diodes(k).name).asInstanceOf[InternalDiode].conducting) ||
						((currentTree.mna.elementCurrent(currentTree.mna.netlist(mnaTreeRoot.netlist.diodes(k).name), 0) <= 0) &&
							(!currentTree.mna.netlist(mnaTreeRoot.netlist.diodes(k).name).asInstanceOf[InternalDiode].conducting))
				})) {
					println("possible: " + currentTree.mna.netlist)
					possibleMNAs += currentTree.mna
				}
			}
			catch {
				case e: JampackNew.JampackException =>
				case e: Exception => println(e.getMessage)
			}
		}
		if (possibleMNAs.isEmpty) {
			Console.err.println("no mna found")
		}
		else {
			if (possibleMNAs.length > 1) {
				println("more than one viable mna found")
			}
			println("accepted: " + possibleMNAs.head.netlist)
			currentMNA = possibleMNAs.head
			for (j <- currentMNAmask.indices) {
				currentMNAmask(j) = currentMNA.netlist.diodes(j).asInstanceOf[InternalDiode].conducting
			}
			println(s"New currentMNAmask = ${currentMNAmask.deep}")
			mnaTracker(0) = currentMNA.clone()
		}
	}

	t += stepSize

	// fill diodeCurrentData and mnaTracker
	for (i <- 1 until pixelWidth) {
		println(s"t = $t")

		markedDiodes = ListBuffer[Int]()

		mnaTreeRoot.netlist.diodes.indices.withFilter(j => currentMNAmask(j)).foreach(d => {
			if (currentMNA.elementCurrent(currentMNA.netlist(mnaTreeRoot.netlist.diodes(d).name), t) <= 0) {
				println(s"Added $d (${mnaTreeRoot.netlist.diodes(d).name}) to list of marked diodes (from conducting to blocking).")
				markedDiodes += d
			}
		})
		mnaTreeRoot.netlist.diodes.indices.withFilter(j => !currentMNAmask(j)).foreach(d => {
			if (currentMNA.elementVoltage(currentMNA.netlist(mnaTreeRoot.netlist.diodes(d).name), t) > 0) {
				println(s"Added $d (${mnaTreeRoot.netlist.diodes(d).name}) to list of marked diodes (from blocking to conducting).")
				markedDiodes += d
			}
		})

		if (markedDiodes.isEmpty) {
			mnaTracker(i) = mnaTracker(i - 1)
		}
		else {
			var tZero = t
			markedDiodes.foreach(d => {
				try {
					val temp = NewtonRaphson.findRoot(time => currentMNA.elementCurrent(currentMNA.netlist(mnaTreeRoot.netlist.diodes(d).name), time),
						time => currentMNA.derivedElementCurrent(currentMNA.netlist(mnaTreeRoot.netlist.diodes(d).name), time), t)
					if (temp < tZero) {
						tZero = temp
						println(s"NewtonRaphson: new tZero = $tZero")
					}
				}
				catch {
					case _: MaximumIterationsReachedException => println("NewtonRaphson: maximum iteration count exceeded.")
				}
			})

			var possibleMNAs = ListBuffer[MNA]()
			for (j <- 0 until 1 << markedDiodes.length) {
				var currentTree = mnaTreeRoot
				var shift = 0
				currentMNAmask.indices.foreach(k => {
					if (!markedDiodes.contains(k)) {
						currentTree = if (currentMNAmask(k)) currentTree.diodeConducting else currentTree.diodeBlocking
					}
					else {
						currentTree = if (((j >> shift) & 1) == 1) currentTree.diodeConducting else currentTree.diodeBlocking
						shift += 1
					}
				})
				try {
					currentTree.mna.applyDataPoint(tZero, mnaTracker(i - 1).getStateVector(tZero))

					if (markedDiodes.forall(k => {
						((currentTree.mna.derivedElementVoltage(currentTree.mna.netlist(mnaTreeRoot.netlist.diodes(k).name), tZero) >= 0) &&
							currentTree.mna.netlist(mnaTreeRoot.netlist.diodes(k).name).asInstanceOf[InternalDiode].conducting) ||
							((currentTree.mna.derivedElementVoltage(currentTree.mna.netlist(mnaTreeRoot.netlist.diodes(k).name), tZero) <= 0) &&
								(!currentTree.mna.netlist(mnaTreeRoot.netlist.diodes(k).name).asInstanceOf[InternalDiode].conducting))
					})) {
						println("possible: " + currentTree.mna.netlist)
						possibleMNAs += currentTree.mna
					}
				}
				catch {
					case e: JampackNew.JampackException =>
					case e: Exception => println(e.getMessage)
				}
			}
			if (possibleMNAs.isEmpty) {
				Console.err.println("no mna found")
			}
			else {
				if (possibleMNAs.length > 1) {
					println("more than one viable mna found")
				}
				println("accepted: " + possibleMNAs.head.netlist)
				currentMNA = possibleMNAs.head
				for (j <- currentMNAmask.indices) {
					currentMNAmask(j) = currentMNA.netlist.diodes(j).asInstanceOf[InternalDiode].conducting
				}
				println(s"New currentMNAmask = ${currentMNAmask.deep}")
				mnaTracker(i) = currentMNA.clone()
			}
		}

		t += stepSize
	}

	private def nextDefaultColor = _nextDefaultColor

	//noinspection ScalaUnusedSymbol
	private def nextDefaultColor_=(value: Int) = _nextDefaultColor = value % GraphPlotter.defaultColors.length

	redrawAxes()

	def plotElementCurrent(elementName: String, color: String = null): Unit = {

		val result = Array.ofDim[Double](pixelWidth)
		var t = startX
		var j = 0
		while (j < pixelWidth) {
			try {
				result(j) = mnaTracker(j).elementCurrent(mnaTracker(j).netlist(elementName), t)
			}
			catch {
				case e: Exception => println(e.getMessage)
			}
			t += stepSize
			j += 1
		}
		results += result

		if (color == null) {
			colors += GraphPlotter.defaultColors(nextDefaultColor)
			nextDefaultColor += 1
		}
		else {
			colors += color
		}

		names += s"i_$elementName"
		enabled += true
		adjustYLimits()
	}

	def plotElementVoltage(elementName: String, color: String = null): Unit = {

		if (color == null) {
			colors += GraphPlotter.defaultColors(nextDefaultColor)
			nextDefaultColor += 1
		}
		else {
			colors += color
		}

		names += s"u_$elementName"
		enabled += true
		val result = Array.ofDim[Double](pixelWidth)
		var t = startX
		var j = 0
		while (j < pixelWidth) {
			try {
				result(j) = mnaTracker(j).elementVoltage(mnaTracker(j).netlist(elementName), t)
			}
			catch {
				case e: Exception => println(e.getMessage)
			}
			t += stepSize
			j += 1
		}
		results += result
		adjustYLimits()
	}

	def plotDerivedElementCurrent(elementName: String, color: String = null): Unit = {

		if (color == null) {
			colors += GraphPlotter.defaultColors(nextDefaultColor)
			nextDefaultColor += 1
		}
		else {
			colors += color
		}

		names += s"di_$elementName/dt"
		enabled += true
		val result = Array.ofDim[Double](pixelWidth)
		var t = startX
		var j = 0
		while (j < pixelWidth) {
			result(j) = mnaTracker(j).derivedElementCurrent(mnaTracker(j).netlist(elementName), t)
			t += stepSize
			j += 1
		}
		results += result
		adjustYLimits()
	}

	def plotDerivedElementVoltage(elementName: String, color: String = null): Unit = {

		if (color == null) {
			colors += GraphPlotter.defaultColors(nextDefaultColor)
			nextDefaultColor += 1
		}
		else {
			colors += color
		}

		names += s"du_$elementName/dt"
		enabled += true
		val result = Array.ofDim[Double](pixelWidth)
		var t = startX
		var j = 0
		while (j < pixelWidth) {
			result(j) = mnaTracker(j).derivedElementVoltage(mnaTracker(j).netlist(elementName), t)
			t += stepSize
			j += 1
		}
		results += result
		adjustYLimits()
	}

	def plotFunction(f: Double => Double, name: String, color: String = null): Unit = {

		if (color == null) {
			colors += GraphPlotter.defaultColors(nextDefaultColor)
			nextDefaultColor += 1
		}
		else {
			colors += color
		}

		names += name
		enabled += true
		val result = Array.ofDim[Double](pixelWidth)
		var t = startX
		var j = 0
		while (j < pixelWidth) {
			result(j) = f(t)
			t += stepSize
			j += 1
		}
		results += result
		adjustYLimits()
	}

	private def adjustYLimits(): Unit = {

		if (autoAdjustY) {
			if (!results.exists(i => enabled(results.indexOf(i)))) {
				maxValue = 1
				minValue = 0
				zeroVector = Vector2D(leftMargin, canvas.height - bottomMargin)
			}
			else {
				maxValue = results.withFilter(i => enabled(results.indexOf(i))).map(_.max).max
				minValue = results.withFilter(i => enabled(results.indexOf(i))).map(_.min).min
				if (minValue > 0) {
					zeroVector = Vector2D(leftMargin, canvas.height - bottomMargin)
				}
				else if (maxValue < 0) {
					zeroVector = Vector2D(leftMargin, topMargin)
				}
				else {
					zeroVector = Vector2D(leftMargin, canvas.height - bottomMargin - minValue.abs * (canvas.height - topMargin - bottomMargin - 20) / (maxValue - minValue) - 10)
				}
			}
		}
		redrawData()
	}

	private def redrawData(): Unit = {

		context.clearRect(0, 0, canvas.width, canvas.height)
		val horizontalScale = (canvas.height - topMargin - bottomMargin - 20) / (maxValue - minValue)
		context.textAlign = "start"
		for (i <- results.indices) {
			if (enabled(i)) {
				context.strokeStyle = colors(i)
				context.beginPath()
				context.moveTo(leftMargin, zeroVector.y - results(i).head * horizontalScale)
				for (j <- 1 until results(i).length) {
					context.lineTo(leftMargin + j, zeroVector.y - results(i)(j) * horizontalScale)
				}
				context.moveTo(leftMargin + 10 + i * 80, canvas.height - bottomMargin + 10)
				context.lineTo(leftMargin + 30 + i * 80, canvas.height - bottomMargin + 10)
				context.stroke()
				context.fillText(names(i), leftMargin + 35 + i * 80, canvas.height - bottomMargin + 14)
			}
		}
		redrawAxes()
	}

	private def redrawAxes(): Unit = {

		context.strokeStyle = "#000000"
		context.beginPath()
		context.moveTo(leftMargin, topMargin)
		context.lineTo(leftMargin, canvas.height - bottomMargin)
		context.moveTo(zeroVector)
		context.lineTo(canvas.width - rightMargin, zeroVector.y)
		context.stroke()
		context.textAlign = "right"
		if (minValue < 0 && maxValue > 0) {
			context.fillText("%.2f".format(maxValue), leftMargin - 2, topMargin + 15)
			context.fillText("%.2f".format(minValue), leftMargin - 2, canvas.height - bottomMargin - 5)
			context.fillText("%.2f".format(0.0), zeroVector + Vector2D(-2, 5))
		}
		else if (minValue >= 0 && maxValue >= 0) {
			context.fillText("%.2f".format(maxValue), leftMargin - 2, topMargin + 15)
			context.fillText("%.2f".format(minValue), leftMargin - 2, canvas.height - bottomMargin + 5)
			context.fillText("%.2f".format((maxValue - minValue) / 2), leftMargin - 2, (canvas.height + topMargin - bottomMargin) / 2 + 5)
		}
		else {
			context.fillText("%.2f".format(maxValue), leftMargin - 2, topMargin + 5)
			context.fillText("%.2f".format(minValue), leftMargin - 2, canvas.height - bottomMargin - 15)
			context.fillText("%.2f".format((maxValue - minValue) / 2), leftMargin - 2, (canvas.height + topMargin - bottomMargin) / 2 - 5)
		}
		context.textAlign = "center"
		for (i <- 1 to endX.toInt) {
			context.fillText("%d".format((endX - startX).toInt / 4 * i), leftMargin + (endX - startX).toInt / 4 * i * pixelsPerUnit, zeroVector.y + 15)
		}
	}

	def enableFunction(name: String): Unit = {

		enabled(names.indexOf(name)) = true
		adjustYLimits()
	}

	def disableFunction(name: String): Unit = {

		enabled(names.indexOf(name)) = false
		adjustYLimits()
	}
}

object GraphPlotter {

	def apply(canvas: html.Canvas, mnaTreeRoot: CircuitAnalyzer.MNATree, endX: Double) =
		new GraphPlotter(canvas, mnaTreeRoot, 0, endX, 0, 0, true)

	def apply(canvas: html.Canvas, mnaTreeRoot: CircuitAnalyzer.MNATree, startX: Double, endX: Double) =
		new GraphPlotter(canvas, mnaTreeRoot, startX, endX, 0, 0, true)

	def apply(canvas: html.Canvas, mnaTreeRoot: CircuitAnalyzer.MNATree, endX: Double, startY: Double, endY: Double) =
		new GraphPlotter(canvas, mnaTreeRoot, 0, endX, startY, endY, false)

	def apply(canvas: html.Canvas, mnaTreeRoot: CircuitAnalyzer.MNATree, startX: Double, endX: Double, startY: Double, endY: Double) =
		new GraphPlotter(canvas, mnaTreeRoot, startX, endX, startY, endY, false)

	val defaultColors = Array(
		"#0072BD",
		"#D95319",
		"#EDB120",
		"#7E2F8E",
		"#77AC30",
		"#4DBEEE",
		"#A2142F"
	)

}

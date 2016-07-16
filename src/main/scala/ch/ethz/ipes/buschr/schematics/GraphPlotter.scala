package ch.ethz.ipes.buschr.schematics

import ch.ethz.ipes.buschr.maths.Vector2D
import org.scalajs.dom._

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Randolph Busch on 03/07/16.
  */
class GraphPlotter(canvas: html.Canvas, startX: Double, endX: Double, startY: Double, endY: Double, autoAdjustY: Boolean) {
	require(startX < endX, "Graphing range must be greater than 0.")
	require(autoAdjustY || startY < endY, "Graphing range must be greater than 0.")

	val context = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
	context.lineWidth = 2
	context.lineCap = "square"
	context.font = "12px Arial"
	val topMargin = 20
	val rightMargin = 20
	val leftMargin = 50
	val bottomMargin = 30
	var zeroVector = Vector2D(leftMargin, canvas.height - bottomMargin)
	context.beginPath()
	context.moveTo(zeroVector)
	context.lineTo(leftMargin, topMargin)
	context.stroke()

	private var maxValue = 1d
	private var minValue = 0d
	private var results = ArrayBuffer[Array[Double]]()
	private var colors = ArrayBuffer[String]()
	private var names = ArrayBuffer[String]()
	private var enabled = ArrayBuffer[Boolean]()
	private var _nextDefaultColor = 0

	private def nextDefaultColor = _nextDefaultColor

	// IntelliJ warns of unused method, but it's _nextDefaultColor's setter, and the project doesn't compile without it,
	// so believe me, it's needed.
	private def nextDefaultColor_=(value: Int) = _nextDefaultColor = value % GraphPlotter.defaultColors.length

	val pixelsPerUnit = (canvas.width - leftMargin - rightMargin) / (endX - startX)

	redrawAxes()

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
		val result = Array.ofDim[Double](canvas.width - leftMargin - rightMargin)
		val stepSize = (endX - startX) / (canvas.width - leftMargin - rightMargin)
		var i = startX
		var j = 0
		while (i < endX) {
			result(j) = f(i)
			i += stepSize
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
		context.fillText("%.2f".format(maxValue), leftMargin - 2, topMargin + 5)
		context.fillText("%.2f".format(minValue), leftMargin - 2, canvas.height - bottomMargin + 5)
		if (minValue < 0 && maxValue > 0) {
			context.fillText("%.2f".format(0.0), zeroVector + Vector2D(-2, 5))
		}
		else {
			context.fillText("%.2f".format((maxValue - minValue) / 2), leftMargin - 2, canvas.height / 2 + 5)
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

	def apply(canvas: html.Canvas, endX: Double) = new GraphPlotter(canvas, 0, endX, 0, 0, true)

	def apply(canvas: html.Canvas, startX: Double, endX: Double) = new GraphPlotter(canvas, startX, endX, 0, 0, true)

	def apply(canvas: html.Canvas, endX: Double, startY: Double, endY: Double) =
		new GraphPlotter(canvas, 0, endX, startY, endY, false)

	def apply(canvas: html.Canvas, startX: Double, endX: Double, startY: Double, endY: Double) =
		new GraphPlotter(canvas, startX, endX, startY, endY, false)

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

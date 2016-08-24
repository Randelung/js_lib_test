package ch.ethz.ipes.buschr.schematics

import JampackNew.Z
import ch.ethz.ipes.buschr.maths.MNA.Input.{InhomogeneityType, InputType}
import ch.ethz.ipes.buschr.maths.MNA.NetList
import ch.ethz.ipes.buschr.maths.Vector2D
import org.scalajs.dom._

import scala.scalajs.js

/**
  * Draws a circuit using a node map and netlist.
  */
object CircuitIllustrator {

	def drawCircuit(canvas: html.Canvas, gridWidth: Int, gridHeight: Int, elementMap: Map[String, (Int, Int, Int, Int)],
					nodeMap: Map[Int, js.Array[(Int, Int)]], netList: NetList): Unit = {

		val context = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
		context.lineWidth = 2
		context.lineCap = "square"
		context.font = "16pt Arial"
		val widthFactor = canvas.width / (gridWidth + 1)
		val heightFactor = canvas.height / (gridHeight + 1)
		context.beginPath()

		netList.capacitors.foreach(i => {

			val startVector = Vector2D(widthFactor * elementMap(i.name)._1, heightFactor * elementMap(i.name)._2)
			val endVector = Vector2D(widthFactor * elementMap(i.name)._3, heightFactor * elementMap(i.name)._4)
			val directionalVector = endVector - startVector
			val width = 20
			context.moveTo(startVector)
			context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 3)
			context.moveTo(endVector)
			context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 3)
			context.moveTo(startVector + directionalVector * 0.5 - directionalVector.normed * 3 +
				directionalVector.perpendicular.normed * width)
			context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 3 -
				directionalVector.perpendicular.normed * width)
			context.moveTo(startVector + directionalVector * 0.5 + directionalVector.normed * 3 +
				directionalVector.perpendicular.normed * width)
			context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 3 -
				directionalVector.perpendicular.normed * width)
			if (startVector.y > endVector.y) {
				context.fillText(i.name, startVector + directionalVector * 0.5 + directionalVector.normed * 8 +
					directionalVector.perpendicular.normed * 30)
			}
			else {
				context.fillText(i.name, startVector + directionalVector * 0.5 + directionalVector.normed * 8 -
					directionalVector.perpendicular.normed * 30)
			}
		})

		netList.resistors.foreach(i => {

			val startVector = Vector2D(widthFactor * elementMap(i.name)._1, heightFactor * elementMap(i.name)._2)
			val endVector = Vector2D(widthFactor * elementMap(i.name)._3, heightFactor * elementMap(i.name)._4)
			if (i.value == Z(0, 0)) {
				context.moveTo(startVector)
				context.lineTo(endVector)
			}
			else {
				val directionalVector = endVector - startVector
				val width = 8
				context.moveTo(startVector)
				context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 20)
				context.moveTo(endVector)
				context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 20)
				context.moveTo(startVector + directionalVector * 0.5 - directionalVector.normed * 20 + directionalVector.perpendicular.normed * width)
				context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 20 - directionalVector.perpendicular.normed * width)
				context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 20 - directionalVector.perpendicular.normed * width)
				context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 20 + directionalVector.perpendicular.normed * width)
				context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 20 + directionalVector.perpendicular.normed * width)
				if (startVector.y > endVector.y) {
					context.fillText(i.name, startVector + directionalVector * 0.5 - directionalVector.normed * 8 +
						directionalVector.perpendicular.normed * 16)
				}
				else {
					context.fillText(i.name, startVector + directionalVector * 0.5 + directionalVector.normed * 8 -
						directionalVector.perpendicular.normed * 16)
				}
			}
		})

		netList.inductors.foreach(i => {

			val startVector = Vector2D(widthFactor * elementMap(i.name)._1, heightFactor * elementMap(i.name)._2)
			val endVector = Vector2D(widthFactor * elementMap(i.name)._3, heightFactor * elementMap(i.name)._4)
			val directionalVector = endVector - startVector
			val width = 8
			context.moveTo(startVector)
			context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 20)
			context.moveTo(endVector)
			context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 20)
			context.stroke()
			context.beginPath()
			context.moveTo(startVector + directionalVector * 0.5 - directionalVector.normed * 20 + directionalVector.perpendicular.normed * width)
			context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 20 - directionalVector.perpendicular.normed * width)
			context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 20 - directionalVector.perpendicular.normed * width)
			context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 20 + directionalVector.perpendicular.normed * width)
			context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 20 + directionalVector.perpendicular.normed * width)
			context.fill()
			context.beginPath()
			if (startVector.y > endVector.y) {
				context.fillText(i.name, startVector + directionalVector * 0.5 - directionalVector.normed * 8 +
					directionalVector.perpendicular.normed * 16)
			}
			else {
				context.fillText(i.name, startVector + directionalVector * 0.5 + directionalVector.normed * 8 -
					directionalVector.perpendicular.normed * 16)
			}
		})

		netList.diodes.foreach(i => {

			val startVector = Vector2D(widthFactor * elementMap(i.name)._1, heightFactor * elementMap(i.name)._2)
			val endVector = Vector2D(widthFactor * elementMap(i.name)._3, heightFactor * elementMap(i.name)._4)
			val directionalVector = endVector - startVector
			val width = 10
			context.moveTo(startVector)
			context.lineTo(endVector)
			context.moveTo(startVector + directionalVector * 0.5 + directionalVector.normed * width * 0.866 + directionalVector.perpendicular.normed * width)
			context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * width * 0.866 - directionalVector.perpendicular.normed * width)
			context.stroke()
			context.beginPath()
			context.moveTo(startVector + directionalVector * 0.5 + directionalVector.normed * width * 0.866)
			context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * width * 0.866 + directionalVector.perpendicular.normed * width)
			context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * width * 0.866 - directionalVector.perpendicular.normed * width)
			context.closePath()
			context.fill()
			context.beginPath()
			if (startVector.y > endVector.y) {
				context.fillText(i.name, startVector + directionalVector * 0.5 - directionalVector.normed * 8 +
					directionalVector.perpendicular.normed * 18)
			}
			else if ((startVector.y == endVector.y) && (startVector.x < endVector.x)) {
				context.fillText(i.name, startVector + directionalVector * 0.5 - directionalVector.perpendicular.normed * 18)
			}
			else if ((startVector.y == endVector.y) && (startVector.x > endVector.x)) {
				context.fillText(i.name, startVector + directionalVector * 0.5 + directionalVector.perpendicular.normed * 18)
			}
			else {
				context.fillText(i.name, startVector + directionalVector * 0.5 + directionalVector.normed * 8 -
					directionalVector.perpendicular.normed * 18)
			}
		})

		netList.inputs.foreach(i => {

			val startVector = Vector2D(widthFactor * elementMap(i.name)._1, heightFactor * elementMap(i.name)._2)
			val endVector = Vector2D(widthFactor * elementMap(i.name)._3, heightFactor * elementMap(i.name)._4)
			val directionalVector = endVector - startVector
			context.moveTo(startVector)
			i.inputType match {
				case InputType.currentSource =>
					i.inhomogeneityType match {
						case InhomogeneityType.zero =>
							if (startVector.y > endVector.y) {
								context.fillText(i.name, startVector + directionalVector * 0.5 +
									directionalVector.perpendicular.normed * 10)
							}
							else {
								context.fillText(i.name, startVector + directionalVector * 0.5 -
									directionalVector.perpendicular.normed * 10)
							}
						case InhomogeneityType.constant | InhomogeneityType.polynomial | InhomogeneityType.exponential =>
							context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 20)
							context.moveTo(endVector)
							context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 20)
							context.stroke()
							context.beginPath()
							context.arc(startVector + directionalVector * 0.5, 20, 0, 2 * math.Pi)
							context.stroke()
							context.beginPath()
							context.moveTo(startVector + directionalVector * 0.5 - directionalVector.normed * 13)
							context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 13)
							context.moveTo(startVector + directionalVector * 0.5 + directionalVector.normed * 8 +
								directionalVector.perpendicular.normed * 5)
							context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 13)
							context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 8 -
								directionalVector.perpendicular.normed * 5)
							if (startVector.y > endVector.y) {
								context.fillText(i.name, startVector + directionalVector * 0.5 +
									directionalVector.normed * 8 + directionalVector.perpendicular.normed * 30)
							}
							else {
								context.fillText(i.name, startVector + directionalVector * 0.5 +
									directionalVector.normed * 8 - directionalVector.perpendicular.normed * 30)
							}
					}
				case InputType.voltageSource =>
					i.inhomogeneityType match {
						case InhomogeneityType.zero =>
							context.lineTo(endVector)
							if (startVector.y > endVector.y) {
								context.fillText(i.name, startVector + directionalVector * 0.5 +
									directionalVector.normed * 8 + directionalVector.perpendicular.normed * 5)
							}
							else {
								context.fillText(i.name, startVector + directionalVector * 0.5 +
									directionalVector.normed * 8 - directionalVector.perpendicular.normed * 5)
							}
						case InhomogeneityType.constant =>
							context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 3)
							context.moveTo(endVector)
							context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 3)
							context.moveTo(startVector + directionalVector * 0.5 + directionalVector.normed * 3 +
								directionalVector.perpendicular.normed * 15)
							context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 3 -
								directionalVector.perpendicular.normed * 15)
							context.moveTo(startVector + directionalVector * 0.5 - directionalVector.normed * 3 -
								directionalVector.perpendicular.normed * 20)
							context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 3 +
								directionalVector.perpendicular.normed * 20)
							if (startVector.y > endVector.y) {
								context.fillText(i.name, startVector + directionalVector * 0.5 +
									directionalVector.normed * 8 + directionalVector.perpendicular.normed * 30)
							}
							else {
								context.fillText(i.name, startVector + directionalVector * 0.5 +
									directionalVector.normed * 8 - directionalVector.perpendicular.normed * 30)
							}
						case InhomogeneityType.exponential | InhomogeneityType.polynomial =>
							context.lineTo(startVector + directionalVector * 0.5 - directionalVector.normed * 20)
							context.moveTo(endVector)
							context.lineTo(startVector + directionalVector * 0.5 + directionalVector.normed * 20)
							context.stroke()
							context.beginPath()
							context.arc(startVector + directionalVector * 0.5, 20, 0, 2 * math.Pi)
							context.stroke()
							context.beginPath()
							val temp = context.font
							val temp2 = context.textAlign
							context.font = "36pt Arial"
							context.textAlign = "center"
							context.fillText("~", startVector + directionalVector * 0.5 + directionalVector.normed * 16)
							context.font = temp
							context.textAlign = temp2
							context.fillText(i.name, startVector + directionalVector * 0.5 + directionalVector.normed * 8 -
								directionalVector.perpendicular.normed * 25)
					}
			}
		})

		netList.grounds.foreach(i => {

			val groundNodeVector = Vector2D(widthFactor * elementMap("g_" + i)._1, heightFactor * elementMap("g_" + i)._2)
			context.beginPath()
			context.moveTo(groundNodeVector)
			context.lineTo(groundNodeVector +(0, 10))
			context.moveTo(groundNodeVector +(-10, 10))
			context.lineTo(groundNodeVector +(10, 10))
			context.moveTo(groundNodeVector +(-6, 14))
			context.lineTo(groundNodeVector +(6, 14))
			context.moveTo(groundNodeVector +(-2, 18))
			context.lineTo(groundNodeVector +(2, 18))
		})

		nodeMap.values.foreach(i => {

			if (i.length > 1) {
				context.moveTo(widthFactor * i.head._1, heightFactor * i.head._2)
				for (j <- 1 until i.length) {
					context.lineTo(widthFactor * i(j)._1, heightFactor * i(j)._2)
				}
			}
		})
		context.stroke()

		println("Circuit illustration done.")
	}

}

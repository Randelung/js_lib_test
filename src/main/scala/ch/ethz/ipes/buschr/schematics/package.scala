package ch.ethz.ipes.buschr

import ch.ethz.ipes.buschr.maths.Vector2D
import org.scalajs.dom._

/** Convenient implicits to use canvas with Vector2D */
package object schematics {

	implicit class Context2DWithVectors(context: CanvasRenderingContext2D) {

		def moveTo(vector: Vector2D) = context.moveTo(vector.x, vector.y)

		def lineTo(vector: Vector2D) = context.lineTo(vector.x, vector.y)

		def fillText(string: String, vector2D: Vector2D) = context.fillText(string, vector2D.x, vector2D.y)

		def arc(vector: Vector2D, radius: Double, startAngle: Double, endAngle: Double, anticlockwise: Boolean = false) =
			context.arc(vector.x, vector.y, radius, startAngle, endAngle, anticlockwise)
	}

}

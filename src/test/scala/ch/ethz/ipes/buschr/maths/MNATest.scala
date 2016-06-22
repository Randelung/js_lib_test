package ch.ethz.ipes.buschr.maths

import JampackNew.{Z, Zmat}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Randolph Busch on 06/06/16.
  */
class MNATest extends FlatSpec with Matchers {

	behavior of "An MNA"

	it should "read a JSON" in {

		val json =
			"{" +
				"\"capacitors\": [" +
				"{\"name\": \"c1\", \"startNode\": 0, \"endNode\": 1, \"realValue\": 100.0, \"imagValue\": 0.0}, " +
				"{\"name\": \"c2\", \"startNode\": 0, \"endNode\": 2, \"realValue\": 100.0, \"imagValue\": 0.0} " +
				"]," +
				"\"inductors\": [" +
				"{\"name\": \"l\", \"startNode\": 0, \"endNode\": 2, \"realValue\": 100.0, \"imagValue\": 0.0} " +
				"], " +
				"\"resistors\": [" +
				"{\"name\": \"r\", \"startNode\": 1, \"endNode\": 2, \"realValue\": 100.0, \"imagValue\": 0.0} " +
				"], " +
				"\"inputs\": [" +
				"{\"name\": \"u1\", \"startNode\": 0, \"endNode\": 2, " +
				"\"inputType\": \"voltageSource\", " +
				"\"inhomogeneityType\": \"exponential\", " +
				"\"coefficients\": [" +
				"{\"realValue\": 0.0, \"imagValue\": 0.5}," +
				"{\"realValue\": 0.0, \"imagValue\": -0.5}" +
				"], " +
				"\"exponents\": [" +
				"{\"realValue\": 0.0, \"imagValue\": -1.0}," +
				"{\"realValue\": 0.0, \"imagValue\": 1.0}" +
				"]" +
				"}" +
				"], " +
				"\"grounds\": [" +
				"]}"
		val mna = MNA.fromJSON(json)
		mna.A shouldEqual new Zmat(Array[Array[Z]](
			Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
			Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
			Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
			Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
			Array(-100, 100, 0, 0, 0, 0, 0, 0, 0, 0),
			Array(-100, 0, 100, 0, 0, 0, 0, 0, 0, 0),
			Array(0, 0, 0, 0, 0, 0, 100, 0, 0, 0),
			Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
			Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
			Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
		))
		mna.B shouldEqual new Zmat(Array[Array[Z]](
			Array(1, 0, 0, 0, -1, -1, -1, 0, 1, 0),
			Array(0, 0, 0, 0, 1, 0, 0, -1, 0, 0),
			Array(0, 0, 0, 0, 0, 1, 1, 1, 0, -1),
			Array(0, 0, 0, 0, 0, 0, 0, 0, -1, 1),
			Array(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
			Array(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
			Array(-1, 0, 1, 0, 0, 0, 0, 0, 0, 0),
			Array(0, -1, 1, 0, 0, 0, 0, 100, 0, 0),
			Array(1, 0, 0, -1, 0, 0, 0, 0, 0.01, 0),
			Array(0, 0, -1, 1, 0, 0, 0, 0, 0, 0)
		))
		(mna.U1, mna.U2) shouldEqual(new Zmat(Array[Array[Z]](
			Array(0, 0),
			Array(0, 0),
			Array(0, 0),
			Array(0, 0),
			Array(0, 0),
			Array(0, 0),
			Array(0, 0),
			Array(0, 0),
			Array(0, 0),
			Array(Z(0, 0.5), Z(0, -0.5))
		)), new Zmat(Array[Array[Z]](
			Array(Z(0, -1)),
			Array(Z(0, 1))
		)))
	}

}

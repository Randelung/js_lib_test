package ch.ethz.ipes.buschr.maths

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Randolph Busch on 06/06/16.
  */
class MNATest extends FlatSpec with Matchers {

	var json =
		"{" +
			"\"capacitors\": [" +
			"{\"name\": \"c1\", \"startNode\": 0, \"endNode\": 1, \"realValue\": 0.001, \"imagValue\": 0.0}, " +
			"{\"name\": \"c2\", \"startNode\": 0, \"endNode\": 2, \"realValue\": 0.001, \"imagValue\": 0.0} " +
			"]," +
			"\"inductors\": [" +
			"{\"name\": \"l\", \"startNode\": 0, \"endNode\": 2, \"realValue\": 0.001, \"imagValue\": 0.0} " +
			"], " +
			"\"resistors\": [" +
			"{\"name\": \"r1\", \"startNode\": 1, \"endNode\": 2, \"realValue\": 100.0, \"imagValue\": 0.0}, " +
			"{\"name\": \"r2\", \"startNode\": 3, \"endNode\": 0, \"realValue\": 0.01, \"imagValue\": 0.0} " +
			"], " +
			"\"inputs\": [" +
			"{\"name\": \"u1\", \"startNode\": 3, \"endNode\": 2, " +
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

	behavior of "An MNA"

	it should "read a JSON" in {
		MNA.fromJSON(json)
	}

	it should "yield solutions" in {

		/*json = "{\"capacitors\": [" +
			"{\"name\": \"C\", \"startNode\": 1, \"endNode\": 2, \"realValue\": 1, \"imagValue\": 0}" +
			"], \"inductors\": [], \"resistors\": [" +
			"{\"name\": \"R\", \"startNode\": 0, \"endNode\": 1, \"realValue\": 1, \"imagValue\": 0}" +
			"], \"inputs\": [" +
			"{\"name\": \"U\", \"startNode\": 0, \"endNode\": 2, " +
			"\"inputType\": \"voltageSource\", " +
			"\"inhomogeneityType\": \"constant\", " +
			"\"coefficients\": [" +
			"{\"realValue\": 1, \"imagValue\": 0}" +
			"], \"exponents\": []" +
			"}" +
			"], \"grounds\": [{\"node\": 2}]" +
			"}"*/
		val (mna, netlist) = MNA.fromJSON(json)
		var t = 0d
		print("u_C = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementVoltage(netlist.capacitors(0), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("u_R = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementVoltage(netlist.resistors(0), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("i_C = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementCurrent(netlist.capacitors(0), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("i_R = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementCurrent(netlist.resistors(0), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("i_I = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementCurrent(netlist.inputs(0), t)) + ",")
			t += 0.1
		}
		println("];")
	}

}

package ch.ethz.ipes.buschr.maths

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Randolph Busch on 06/06/16.
  */
class MNATest extends FlatSpec with Matchers {

	var json =
		"{" +
			"\"capacitors\": [" +
			"{\"name\": \"C1\", \"startNode\": 0, \"endNode\": 1, \"realValue\": 1, \"imagValue\": 0.0}, " +
			"{\"name\": \"C2\", \"startNode\": 0, \"endNode\": 2, \"realValue\": 1, \"imagValue\": 0.0} " +
			"]," +
			"\"inductors\": [" +
			"{\"name\": \"L\", \"startNode\": 0, \"endNode\": 2, \"realValue\": 1, \"imagValue\": 0.0} " +
			"], " +
			"\"resistors\": [" +
			"{\"name\": \"R1\", \"startNode\": 1, \"endNode\": 2, \"realValue\": 1, \"imagValue\": 0.0}, " +
			"{\"name\": \"R2\", \"startNode\": 3, \"endNode\": 0, \"realValue\": 1, \"imagValue\": 0.0} " +
			"], " +
			"\"inputs\": [" +
			"{\"name\": \"U\", \"startNode\": 3, \"endNode\": 2, " +
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
			"{\"node\": 2}" +
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
		val (netlist, mna) = MNA.fromJSON(json)
		mna.applyStartingConditions(null)
		var t = 0d
		print("u_C1 = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementVoltage(netlist.capacitors(0), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("u_C2 = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementVoltage(netlist.capacitors(1), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("u_R1 = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementVoltage(netlist.resistors(0), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("u_R2 = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementVoltage(netlist.resistors(1), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("u_L = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementVoltage(netlist.inductors(0), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("i_C1 = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementCurrent(netlist.capacitors(0), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("i_C2 = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementCurrent(netlist.capacitors(1), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("i_R1 = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementCurrent(netlist.resistors(0), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("i_R2 = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementCurrent(netlist.resistors(1), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("i_L = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementCurrent(netlist.inductors(0), t)) + ",")
			t += 0.1
		}
		println("];")
		t = 0
		print("i_U = [")
		while (t < 4 * math.Pi) {
			print("%.3f".format(mna.elementCurrent(netlist.inputs(0), t)) + ",")
			t += 0.1
		}
		println("];")
	}

}

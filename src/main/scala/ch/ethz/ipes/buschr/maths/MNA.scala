package ch.ethz.ipes.buschr.maths

import JampackNew.{Z, Zmat}

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.JSON

/** Parse Netlist into an MNA DAE system.
  *
  * @param netlist Netlist to parse into a DAE system
  * @author Randolph Busch
  */
class MNA(private val netlist: MNA.NetList) {

	val _netlist = netlist.copy()

	// enumerate nodes. nodes in the NetList don't necessarily correspond to IDs given here, since they could be anything
	// as long as they are consistent
	var nodes = ListBuffer[Int]()
	_netlist.capacitors.foreach(i => {
		if (!nodes.contains(i.startNode)) {
			nodes += i.startNode
		}
		if (!nodes.contains(i.endNode)) {
			nodes += i.endNode
		}
	})
	_netlist.inductors.foreach(i => {
		if (!nodes.contains(i.startNode)) {
			nodes += i.startNode
		}
		if (!nodes.contains(i.endNode)) {
			nodes += i.endNode
		}
	})
	_netlist.resistors.foreach(i => {
		if (!nodes.contains(i.startNode)) {
			nodes += i.startNode
		}
		if (!nodes.contains(i.endNode)) {
			nodes += i.endNode
		}
	})
	_netlist.inputs.foreach(i => {
		if (!nodes.contains(i.startNode)) {
			nodes += i.startNode
		}
		if (!nodes.contains(i.endNode)) {
			nodes += i.endNode
		}
	})
	require(_netlist.grounds.forall(nodes.contains(_)))

	// add additional resistor for every input
	var j = 0
	_netlist.inputs.foreach(i => {
		while (nodes.contains(j)) {
			j += 1
		}
		nodes += j
		var s = "r_" + i.name
		while ((_netlist.resistors.map(_.name) ++ _netlist.inductors.map(_.name) ++ _netlist.inputs.map(_.name) ++
			_netlist.capacitors.map(_.name)).contains(s)) {
			s += "1"
		}
		_netlist.resistors += new MNA.Resistor(s, j, i.startNode, Z(0.01, 0))
		_netlist.inputs(_netlist.inputs.indexOf(i)) = new MNA.Input(i.name, j, i.endNode, i.inhomogeneityType, i.inputType,
			i.coefficients, i.exponents)
	})

	// solution vector is stacked values of all node potentials and all element currents. sources are elements.
	val vectorLength = nodes.length + _netlist.capacitors.length + _netlist.resistors.length + _netlist.inductors.length +
		_netlist.inputs.length

	// enumerate input exponents to optimize input matrix sizes
	var exponents = ListBuffer[Z]()
	_netlist.inputs.foreach(_.exponents.foreach(i => {
		if (!exponents.contains(i)) {
			exponents += i
		}
	}))

	// fill A, B and U for Ay' + By = U
	val A = new Zmat(vectorLength, vectorLength)
	val B = new Zmat(vectorLength, vectorLength)
	val U1 = new Zmat(vectorLength, exponents.length)
	val U2 = new Zmat(exponents.map(Array(_)).toArray)

	// add grounds
	if (_netlist.grounds.isEmpty) {
		B(0, 0) = 1
	}
	else {
		for (i <- _netlist.grounds.indices) {
			B(_netlist.grounds(i), _netlist.grounds(i)) = 1
		}
	}

	// KCL consists of 1 and -1 only, current and voltage relations follow in later lines.
	for (i <- nodes.indices) {
		for (j <- _netlist.capacitors.indices) {
			if (_netlist.capacitors(j).startNode == nodes(i)) {
				B(i, nodes.length + j) = -1
			}
			else if (_netlist.capacitors(j).endNode == nodes(i)) {
				B(i, nodes.length + j) = 1
			}
		}
		for (j <- _netlist.inductors.indices) {
			if (_netlist.inductors(j).startNode == nodes(i)) {
				B(i, nodes.length + _netlist.capacitors.length + j) = -1
			}
			else if (_netlist.inductors(j).endNode == nodes(i)) {
				B(i, nodes.length + _netlist.capacitors.length + j) = 1
			}
		}
		for (j <- _netlist.resistors.indices) {
			if (_netlist.resistors(j).startNode == nodes(i)) {
				B(i, nodes.length + _netlist.capacitors.length + _netlist.inductors.length + j) = -1
			}
			else if (_netlist.resistors(j).endNode == nodes(i)) {
				B(i, nodes.length + _netlist.capacitors.length + _netlist.inductors.length + j) = 1
			}
		}
		for (j <- _netlist.inputs.indices) {
			if (_netlist.inputs(j).startNode == nodes(i)) {
				// input current is from end to start
				B(i, nodes.length + _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.length + j) = 1
			}
			else if (_netlist.inputs(j).endNode == nodes(i)) {
				B(i, nodes.length + _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.length + j) = -1
			}
		}
	}

	// KVL for capacitors: potential(startNode) - potential(endNode) = u_C = 1/iwC * i_C
	// therefore: iwC * u_C = i_C, or -iwC * u_C + i_C = 0
	// and finally: iwC * (potential(endNode) - potential(startNode) + i_C = 0 (resp. input if present)
	for (i <- _netlist.capacitors.indices) {
		val matrixIndex = nodes.length + i
		A(matrixIndex, nodes.indexOf(_netlist.capacitors(i).startNode)) = -_netlist.capacitors(i).value
		A(matrixIndex, nodes.indexOf(_netlist.capacitors(i).endNode)) = _netlist.capacitors(i).value
		B(matrixIndex, matrixIndex) = 1
	}

	// KVL for inductors: potential(startNode) - potential(endNode) = u_L = jwL * i_L
	// therefore: potential(endNode) - potential(startNode) + jwL * i_L = 0 (resp. input if present)
	for (i <- _netlist.inductors.indices) {
		val matrixIndex = nodes.length + _netlist.capacitors.length + i
		B(matrixIndex, nodes.indexOf(_netlist.inductors(i).startNode)) = -1
		B(matrixIndex, nodes.indexOf(_netlist.inductors(i).endNode)) = 1
		A(matrixIndex, matrixIndex) = _netlist.inductors(i).value
	}

	// KVL for resistors: potential(startNode) - potential(endNode) = u_R = R * i_R
	// therefore: potential(endNode) - potential(startNode) + R * i_R = 0 (resp. input if present)
	for (i <- _netlist.resistors.indices) {
		val matrixIndex = nodes.length + _netlist.capacitors.length + _netlist.inductors.length + i
		B(matrixIndex, nodes.indexOf(_netlist.resistors(i).startNode)) = -1
		B(matrixIndex, nodes.indexOf(_netlist.resistors(i).endNode)) = 1
		B(matrixIndex, matrixIndex) = _netlist.resistors(i).value
	}

	for (i <- _netlist.inputs.indices) {
		_netlist.inputs(i).inputType match {
			case MNA.Input.InputType.currentSource =>
				// remove current at startNode, add at endNode
				//TODO current source implementation
				throw new Exception("Current source not yet implemented.")
			case MNA.Input.InputType.voltageSource =>
				// KVL for inputs: potential(startNode) - potential(endNode) = u_I (which is input)
				val matrixIndex = nodes.length + _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.length + i
				B(matrixIndex, nodes.indexOf(_netlist.inputs(i).startNode)) = 1
				B(matrixIndex, nodes.indexOf(_netlist.inputs(i).endNode)) = -1
				for (j <- _netlist.inputs(i).coefficients.indices) {
					U1(matrixIndex, exponents.indexOf(_netlist.inputs(i).exponents(j))) = _netlist.inputs(i).coefficients(j)
				}
			case _ =>
				throw new Exception("Not a valid InputType.")
		}
	}

	// implementation of MATLAB code: DiffEquationCreator
	// rows and columns of differential variables
	var col = ListBuffer.empty[Int]
	var row = ListBuffer.empty[Int]
	for (i <- A.getZ.indices) {
		if (!A(i).forall(_ == Z(0, 0))) {
			row += i
		}
	}
	val Atranspose = A.transpose
	for (i <- A.transpose.getZ.indices) {
		if (!A.transpose(i).forall(_ == Z(0, 0)) && B(i, i) != Z(1, 0)) {
			col += i
		}
	}

	val P = new Zmat(row.length, A.ncol)
	val Q = new Zmat(A.nrow, col.length)
	for (i <- row.indices) {
		P(i, row(i)) = 1
	}
	for (i <- col.indices) {
		Q(col(i), i) = 1
	}

	val S = new Zmat(A.nrow - row.length, A.ncol)
	val T = new Zmat(A.nrow, A.ncol - col.length)
	j = 0
	val Ptranspose = P.transpose
	for (i <- 0 until P.ncol) {
		if (Ptranspose(i).forall(_ == Z(0, 0))) {
			S(j, i) = 1
			j += 1
		}
	}
	j = 0
	for (i <- 0 until Q.nrow) {
		if (Q(i).forall(_ == Z(0, 0))) {
			T(i, j) = 1
			j += 1
		}
	}

	val PAQinv = (P * A * Q).inv
	val SBTinv = (S * B * T).inv
	val PBT = P * B * T

	val M = PAQinv * (-P * B * Q + PBT * SBTinv * S * B * Q)
	val u = -PAQinv * PBT * SBTinv * S * U1

	val diffEquation = new DiffEquation(M)
	diffEquation.applyInhomogeneity(u, U2)
	diffEquation.applyStartingConditions(Array.fill(P.nrow)(0))
}

object MNA {

	def fromJSON(json: String): MNA = {

		// parse JSON
		val parsedJSON = JSON.parse(json)
		val capacitors = parsedJSON.capacitors.asInstanceOf[js.Array[js.Dynamic]]
			.map(i => new Capacitor(i.name.asInstanceOf[String], i.startNode.asInstanceOf[Int], i.endNode.asInstanceOf[Int],
				new Z(i.realValue.asInstanceOf[Double], i.imagValue.asInstanceOf[Double])))
		val inductors = parsedJSON.inductors.asInstanceOf[js.Array[js.Dynamic]]
			.map(i => new Inductor(i.name.asInstanceOf[String], i.startNode.asInstanceOf[Int], i.endNode.asInstanceOf[Int],
				new Z(i.realValue.asInstanceOf[Double], i.imagValue.asInstanceOf[Double])))
		val resistors = parsedJSON.resistors.asInstanceOf[js.Array[js.Dynamic]]
			.map(i => new Resistor(i.name.asInstanceOf[String], i.startNode.asInstanceOf[Int], i.endNode.asInstanceOf[Int],
				new Z(i.realValue.asInstanceOf[Double], i.imagValue.asInstanceOf[Double])))
		val inputs = parsedJSON.inputs.asInstanceOf[js.Array[js.Dynamic]]
			.map(i => new Input(i.name.asInstanceOf[String], i.startNode.asInstanceOf[Int], i.endNode.asInstanceOf[Int],
				Input.InhomogeneityType.withName(i.inhomogeneityType.asInstanceOf[String]),
				Input.InputType.withName(i.inputType.asInstanceOf[String]),
				i.coefficients.asInstanceOf[js.Array[js.Dynamic]]
					.map(i => Z(i.realValue.asInstanceOf[Double], i.imagValue.asInstanceOf[Double])).toArray,
				i.exponents.asInstanceOf[js.Array[js.Dynamic]]
					.map(i => Z(i.realValue.asInstanceOf[Double], i.imagValue.asInstanceOf[Double])).toArray))
		val grounds = parsedJSON.grounds.asInstanceOf[js.Array[js.Dynamic]]
			.map(i => i.node.asInstanceOf[Int])
		require(inputs.forall(i => i.exponents.length == i.coefficients.length), "Input matrix dimensions must agree.")
		require(inputs.forall(i => i.inhomogeneityType == inputs.head.inhomogeneityType), "Only single type of inhomogeneity supported.")

		new MNA(new NetList(capacitors, inductors, resistors, inputs, grounds))
	}

	case class Capacitor(name: String, startNode: Int, endNode: Int, value: Z)

	case class Inductor(name: String, startNode: Int, endNode: Int, value: Z)

	case class Resistor(name: String, startNode: Int, endNode: Int, value: Z)

	case class Input(name: String, startNode: Int, endNode: Int, inhomogeneityType: Input.InhomogeneityType.Value,
					 inputType: Input.InputType.Value, coefficients: Array[Z], exponents: Array[Z])

	object Input {

		object InhomogeneityType extends Enumeration {

			val exponential, polynomial, constant, zero = Value
		}

		object InputType extends Enumeration {

			val currentSource, voltageSource = Value
		}

	}

	/** Holding object for all the linear components of a system.
	  *
	  * @param capacitors js.Array of capacitors
	  * @param inductors  js.Array of inductors
	  * @param resistors  js.Array of resistors
	  * @param inputs     js.Array of inputs
	  * @param grounds    js.Array of Ints that are the NodeIDs of nodes with ground connections
	  */
	case class NetList(var capacitors: js.Array[Capacitor], var inductors: js.Array[Inductor],
					   var resistors: js.Array[Resistor], var inputs: js.Array[Input],
					   var grounds: js.Array[Int]) extends Serializable

}

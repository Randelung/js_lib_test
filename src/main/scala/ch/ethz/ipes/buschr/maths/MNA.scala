package ch.ethz.ipes.buschr.maths

import JampackNew.{Z, Zdiagmat, Zmat}
import ch.ethz.ipes.buschr.maths.MNA.NetList

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.JSON

/** Parse Netlist into an MNA DAE system.
  *
  * @param netlist Netlist to parse into a DAE system
  * @author Randolph Busch
  */
class MNA(private val netlist: MNA.NetList) {

	private var _netlist = netlist.copy

	// enumerate nodes. nodes in the NetList don't necessarily correspond to IDs given here, since they could be anything
	// as long as they are consistent
	private var nodes = ListBuffer[Int]()
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

	// add additional resistor for every input (non-ideal inputs)
	private var j = 0

	// this part adds resistors for each input to create non-ideal sources.
	/*_netlist.inputs.foreach(i => {
		i.inputType match {
			case MNA.Input.InputType.voltageSource =>
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
			case MNA.Input.InputType.currentSource =>
				var s = "r_" + i.name
				while ((_netlist.resistors.map(_.name) ++ _netlist.inductors.map(_.name) ++ _netlist.inputs.map(_.name) ++
					_netlist.capacitors.map(_.name)).contains(s)) {
					s += "1"
				}
				_netlist.resistors += new MNA.Resistor(s, i.startNode, i.endNode, Z(1e9, 0))
			case _ =>
				throw new Exception("Not a valid InputType.")
		}
	})*/

	// solution vector is stacked values of all node potentials and all element currents. sources are elements.
	private var vectorLength = nodes.length + 2 * _netlist.capacitors.length + _netlist.resistors.length + _netlist.inductors.length +
		_netlist.inputs.length

	// enumerate input exponents to optimize input matrix sizes
	private var inputExponents = ListBuffer[Z]()
	_netlist.inputs.foreach(i =>
		if (i.inhomogeneityType == MNA.Input.InhomogeneityType.exponential) {
			i.exponents.foreach(k => {
				if (!inputExponents.contains(k)) {
					inputExponents += k
				}
			})
		}
	)

	// fill A, B and U for Ay' + By = U
	private var A = new Zmat(vectorLength, vectorLength)
	private var B = new Zmat(vectorLength, vectorLength)
	private var U1 = new Zmat(vectorLength, if (inputExponents.nonEmpty) inputExponents.length else _netlist.inputs.map(_.coefficients.length).reduce(math.max))
	private var U2 = if (inputExponents.nonEmpty) new Zmat(inputExponents.map(Array(_)).toArray) else null

	// add single zero potential. if multiple grounds present, zero resistance equalizes currents.
	if (_netlist.grounds.isEmpty) {
		B(0, 0) = 1
	}
	else {
		B(nodes.indexOf(_netlist.grounds(0)), nodes.indexOf(_netlist.grounds(0))) = 1
		for (i <- 1 until _netlist.grounds.length) {
			var s = "r_0_1"
			while ((_netlist.resistors.map(_.name) ++ _netlist.inductors.map(_.name) ++ _netlist.inputs.map(_.name) ++
				_netlist.capacitors.map(_.name)).contains(s)) {
				s += "1"
			}
			_netlist.resistors += MNA.Resistor(s, _netlist.grounds(i), _netlist.grounds(0), 0)
		}
	}

	// KCL consists of 1 and -1 only, current and voltage relations follow in later lines.
	for (i <- nodes.indices) {
		for (j <- _netlist.capacitors.indices) {
			if (_netlist.capacitors(j).startNode == nodes(i)) {
				B(i, nodes.length + _netlist.capacitors.length + j) = -1
			}
			else if (_netlist.capacitors(j).endNode == nodes(i)) {
				B(i, nodes.length + _netlist.capacitors.length + j) = 1
			}
		}
		for (j <- _netlist.inductors.indices) {
			if (_netlist.inductors(j).startNode == nodes(i)) {
				B(i, nodes.length + 2 * _netlist.capacitors.length + j) = -1
			}
			else if (_netlist.inductors(j).endNode == nodes(i)) {
				B(i, nodes.length + 2 * _netlist.capacitors.length + j) = 1
			}
		}
		for (j <- _netlist.resistors.indices) {
			if (_netlist.resistors(j).startNode == nodes(i)) {
				B(i, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + j) = -1
			}
			else if (_netlist.resistors(j).endNode == nodes(i)) {
				B(i, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + j) = 1
			}
		}
		for (j <- _netlist.inputs.indices) {
			if (_netlist.inputs(j).startNode == nodes(i)) {
				// input current is from end to start
				B(i, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.length + j) = 1
			}
			else if (_netlist.inputs(j).endNode == nodes(i)) {
				B(i, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.length + j) = -1
			}
		}
	}

	// KVL for capacitors: potential(startNode) - potential(endNode) = u_C = 1/iwC * i_C
	// therefore: iwC * u_C = i_C, or -iwC * u_C + i_C = 0
	// and finally: iwC * (potential(endNode) - potential(startNode) + i_C = 0 (resp. input if present)
	// however, since only one variable can be used in the differentiation, potential(endNode) - potential(startNode) is
	// put into a new variable that can then be differentiated, using u_C + potential(endNode) - potential(startNode) = 0
	for (i <- _netlist.capacitors.indices) {
		val matrixIndexExtraVar = nodes.length + i
		val matrixIndexKVL = nodes.length + _netlist.capacitors.length + i
		B(matrixIndexExtraVar, nodes.indexOf(_netlist.capacitors(i).startNode)) = -1
		B(matrixIndexExtraVar, nodes.indexOf(_netlist.capacitors(i).endNode)) = 1
		B(matrixIndexExtraVar, matrixIndexExtraVar) = 1
		B(matrixIndexKVL, matrixIndexKVL) = 1
		A(matrixIndexKVL, matrixIndexExtraVar) = -_netlist.capacitors(i).value
	}

	// KVL for inductors: potential(startNode) - potential(endNode) = u_L = jwL * i_L
	// therefore: potential(endNode) - potential(startNode) + jwL * i_L = 0 (resp. input if present)
	for (i <- _netlist.inductors.indices) {
		val matrixIndex = nodes.length + 2 * _netlist.capacitors.length + i
		B(matrixIndex, nodes.indexOf(_netlist.inductors(i).startNode)) = -1
		B(matrixIndex, nodes.indexOf(_netlist.inductors(i).endNode)) = 1
		A(matrixIndex, matrixIndex) = _netlist.inductors(i).value
	}

	// KVL for resistors: potential(startNode) - potential(endNode) = u_R = R * i_R
	// therefore: potential(endNode) - potential(startNode) + R * i_R = 0 (resp. input if present)
	for (i <- _netlist.resistors.indices) {
		val matrixIndex = nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + i
		B(matrixIndex, nodes.indexOf(_netlist.resistors(i).startNode)) = -1
		B(matrixIndex, nodes.indexOf(_netlist.resistors(i).endNode)) = 1
		B(matrixIndex, matrixIndex) = _netlist.resistors(i).value
	}

	for (i <- _netlist.inputs.indices) {
		_netlist.inputs(i).inputType match {
			case MNA.Input.InputType.currentSource =>
				// remove current at startNode, add at endNode
				for (j <- _netlist.inputs(i).coefficients.indices) {
					U1(_netlist.inputs(i).startNode, inputExponents.indexOf(_netlist.inputs(i).exponents(j))) = -_netlist.inputs(i).coefficients(j)
					U1(_netlist.inputs(i).endNode, inputExponents.indexOf(_netlist.inputs(i).exponents(j))) = _netlist.inputs(i).coefficients(j)
				}
			case MNA.Input.InputType.voltageSource =>
				// KVL for inputs: potential(startNode) - potential(endNode) = u_I (which is input)
				val matrixIndex = nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.length + i
				B(matrixIndex, nodes.indexOf(_netlist.inputs(i).startNode)) = 1
				B(matrixIndex, nodes.indexOf(_netlist.inputs(i).endNode)) = -1
				if (_netlist.inputs(i).inhomogeneityType == MNA.Input.InhomogeneityType.exponential) {
					for (j <- _netlist.inputs(i).coefficients.indices) {
						U1(matrixIndex, inputExponents.indexOf(_netlist.inputs(i).exponents(j))) = _netlist.inputs(i).coefficients(j)
					}
				}
				else {
					for (j <- _netlist.inputs(i).coefficients.indices) {
						U1(matrixIndex, j) = _netlist.inputs(i).coefficients(j)
					}
				}
			case _ =>
				throw new Exception("Not a valid InputType.")
		}
	}

	// implementation of MATLAB code: DiffEquationCreator
	// rows and columns of differential variables
	private var col = ListBuffer.empty[Int]
	private var row = ListBuffer.empty[Int]
	for (i <- A.getZ.indices) {
		if (!A(i).forall(_ == Z(0, 0))) {
			row += i
		}
	}
	private var Atranspose = A.transpose
	for (i <- A.transpose.getZ.indices) {
		if (!Atranspose(i).forall(_ == Z(0, 0))) {
			col += i
		}
	}

	private var P = new Zmat(row.length, A.ncol)
	private var Q = new Zmat(A.nrow, col.length)
	for (i <- row.indices) {
		P(i, row(i)) = 1
	}
	for (i <- col.indices) {
		Q(col(i), i) = 1
	}

	private var S = new Zmat(A.nrow - row.length, A.ncol)
	private var T = new Zmat(A.nrow, A.ncol - col.length)
	j = 0
	private var Ptranspose = P.transpose
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
	private var PAQinv = (P * A * Q).inv
	private var SBTinv = (S * B * T).inv
	private var PBT = P * B * T
	private var SBQ = S * B * Q

	private var M = PAQinv * (-P * B * Q + PBT * SBTinv * SBQ)
	private var u = -PAQinv * PBT * SBTinv * S * U1

	private var diffEquation = new DiffEquation(M)
	diffEquation.applyInhomogeneity(u, U2)

	private var QmTSBTinvSBQ = Q - T * SBTinv * SBQ

	var appliedStartingConditions = false

	def applyStartingConditions(startingConditions: Array[Double]): Unit = {

		if (startingConditions == null) {
			diffEquation.applyStartingConditions(Array.fill(P.nrow)(0))
		}
		else {
			diffEquation.applyStartingConditions(startingConditions)
		}
		appliedStartingConditions = true
	}

	def getStateVector(t: Double): Array[Double] = {
		require(appliedStartingConditions, "Need starting conditions first.")

		if (_netlist.inputs.head.inhomogeneityType == MNA.Input.InhomogeneityType.exponential) {
			((QmTSBTinvSBQ * diffEquation.solutionVector(t)) +
				(T * SBTinv * S * U1 * new Zmat(U2.getZ.map(_.map(i => (i * t).exp))))).re.map(_.head)
		}
		else {
			val array = Array.tabulate(U1.ncol)(i => Array(math.pow(t, i)))
			val array2 = Array.fill(U1.ncol, 1)(0d)
			(QmTSBTinvSBQ * diffEquation.solutionVector(t) +
				T * SBTinv * S * U1 * new Zmat(array, array2)).re.map(_.head)
		}
	}

	private def solution(t: Double, index: Int): Double = {
		require(index >= 0 && index < A.nrow, "Index must be positive integer in the size bounds.")
		require(appliedStartingConditions, "Need starting conditions first.")
		// x = Q * x_diff + T * x_linear
		// x_linear = -inv(S B T) S B Q x_diff + inv(S B T) S b
		if (_netlist.inputs.head.inhomogeneityType == MNA.Input.InhomogeneityType.exponential) {
			(QmTSBTinvSBQ.get(index, index, 0, Q.ncol - 1) * diffEquation.solutionVector(t)) (0, 0).re +
				(T.get(index, index, 0, S.nrow - 1) * SBTinv * S * U1 * new Zmat(U2.getZ.map(_.map(i => (i * t).exp)))) (0, 0).re
		}
		else {
			val array = Array.tabulate(U1.ncol)(i => Array(math.pow(t, i)))
			val array2 = Array.fill(U1.ncol, 1)(0d)
			(QmTSBTinvSBQ.get(index, index, 0, Q.ncol - 1) * diffEquation.solutionVector(t)) (0, 0).re +
				(T.get(index, index, 0, S.nrow - 1) * SBTinv * S * U1 * new Zmat(array, array2)) (0, 0).re
		}
	}

	private def derivedSolution(t: Double, index: Int): Double = {
		require(index >= 0 && index < A.nrow, "Index must be positive integer in the size bounds.")
		require(appliedStartingConditions, "Need starting conditions first.")

		if (_netlist.inputs.head.inhomogeneityType == MNA.Input.InhomogeneityType.exponential) {
			(QmTSBTinvSBQ.get(index, index, 0, Q.ncol - 1) * diffEquation.derivedSolutionVector(t)) (0, 0).re +
				(T.get(index, index, 0, S.nrow - 1) * SBTinv * S * U1 * new Zdiagmat(U2.getZ.map(_.head.re), U2.getZ.map(_.head.im)) *
					new Zmat(U2.getZ.map(_.map(i => (i * t).exp)))) (0, 0).re
		}
		else {
			val array = Array.tabulate(U1.ncol)(i => Array(math.pow(t, i)))
			val array2 = Array.fill(U1.ncol, 1)(0d)
			val diffMatrix = new Zmat(U1.ncol, U1.ncol)
			for (i <- 1 until U1.ncol) {
				diffMatrix(i, i - 1) = i
			}
			(QmTSBTinvSBQ.get(index, index, 0, Q.ncol - 1) * diffEquation.derivedSolutionVector(t)) (0, 0).re +
				(T.get(index, index, 0, S.nrow - 1) * SBTinv * S * U1 * diffMatrix * new Zmat(array, array2)) (0, 0).re
		}
	}

	override def clone(): MNA = {
		val copy = new MNA(new NetList(js.Array(), js.Array(), js.Array(), js.Array(), js.Array(), js.Array()))
		copy._netlist = _netlist.copy
		copy.nodes = nodes.clone()
		copy.j = j
		copy.vectorLength = vectorLength
		copy.inputExponents = inputExponents.clone()
		copy.A = A.clone()
		copy.B = B.clone()
		copy.U1 = U1.clone()
		copy.U2 = U2.clone()
		copy.col = col.clone()
		copy.row = row.clone()
		copy.Atranspose = Atranspose.clone()
		copy.P = P.clone()
		copy.Q = Q.clone()
		copy.S = S.clone()
		copy.T = T.clone()
		copy.Ptranspose = Ptranspose.clone()
		copy.PAQinv = PAQinv.clone()
		copy.SBTinv = SBTinv.clone()
		copy.PBT = PBT.clone()
		copy.SBQ = SBQ.clone()
		copy.M = M.clone()
		copy.u = u.clone()
		copy.diffEquation = diffEquation.clone()
		copy.QmTSBTinvSBQ = QmTSBTinvSBQ.clone()
		copy
	}

	def elementVoltage(element: MNA.Capacitor, t: Double): Double = {
		require(_netlist.capacitors.contains(element), "Element not in netlist.")
		solution(t, nodes.indexOf(element.startNode)) - solution(t, nodes.indexOf(element.endNode))
	}

	def elementVoltage(element: MNA.Resistor, t: Double): Double = {
		require(_netlist.resistors.contains(element), "Element not in netlist.")
		solution(t, nodes.indexOf(element.startNode)) - solution(t, nodes.indexOf(element.endNode))
	}

	def elementVoltage(element: MNA.Inductor, t: Double): Double = {
		require(_netlist.inductors.contains(element), "Element not in netlist.")
		solution(t, nodes.indexOf(element.startNode)) - solution(t, nodes.indexOf(element.endNode))
	}

	def elementVoltage(element: MNA.Input, t: Double): Double = {
		require(_netlist.inputs.contains(element), "Element not in netlist.")
		solution(t, nodes.indexOf(element.startNode)) - solution(t, nodes.indexOf(element.endNode))
	}

	def elementCurrent(element: MNA.Capacitor, t: Double): Double = {
		require(_netlist.capacitors.contains(element), "Element not in netlist.")
		solution(t, nodes.length + _netlist.capacitors.length + _netlist.capacitors.indexOf(element))
	}

	def elementCurrent(element: MNA.Resistor, t: Double): Double = {
		require(_netlist.resistors.contains(element), "Element not in netlist.")
		solution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.indexOf(element))
	}

	def elementCurrent(element: MNA.Inductor, t: Double): Double = {
		require(_netlist.inductors.contains(element), "Element not in netlist.")
		solution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.indexOf(element))
	}

	def elementCurrent(element: MNA.Input, t: Double): Double = {
		require(_netlist.inputs.contains(element), "Element not in netlist.")
		solution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.length + _netlist.inputs.indexOf(element))
	}

	def derivedElementVoltage(element: MNA.Capacitor, t: Double): Double = {
		require(_netlist.capacitors.contains(element), "Element not in netlist.")
		derivedSolution(t, nodes.indexOf(element.startNode)) - derivedSolution(t, nodes.indexOf(element.endNode))
	}

	def derivedElementVoltage(element: MNA.Resistor, t: Double): Double = {
		require(_netlist.resistors.contains(element), "Element not in netlist.")
		derivedSolution(t, nodes.indexOf(element.startNode)) - derivedSolution(t, nodes.indexOf(element.endNode))
	}

	def derivedElementVoltage(element: MNA.Inductor, t: Double): Double = {
		require(_netlist.inductors.contains(element), "Element not in netlist.")
		derivedSolution(t, nodes.indexOf(element.startNode)) - derivedSolution(t, nodes.indexOf(element.endNode))
	}

	def derivedElementVoltage(element: MNA.Input, t: Double): Double = {
		require(_netlist.inputs.contains(element), "Element not in netlist.")
		derivedSolution(t, nodes.indexOf(element.startNode)) - derivedSolution(t, nodes.indexOf(element.endNode))
	}

	def derivedElementCurrent(element: MNA.Capacitor, t: Double): Double = {
		require(_netlist.capacitors.contains(element), "Element not in netlist.")
		derivedSolution(t, nodes.length + _netlist.capacitors.length + _netlist.capacitors.indexOf(element))
	}

	def derivedElementCurrent(element: MNA.Resistor, t: Double): Double = {
		require(_netlist.resistors.contains(element), "Element not in netlist.")
		derivedSolution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.indexOf(element))
	}

	def derivedElementCurrent(element: MNA.Inductor, t: Double): Double = {
		require(_netlist.inductors.contains(element), "Element not in netlist.")
		derivedSolution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.indexOf(element))
	}

	def derivedElementCurrent(element: MNA.Input, t: Double): Double = {
		require(_netlist.inputs.contains(element), "Element not in netlist.")
		derivedSolution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.length + _netlist.inputs.indexOf(element))
	}
}

object MNA {

	def fromJSON(json: String) = {

		val netlist = NetList.fromJSON(json)
		(netlist, new MNA(netlist))
	}

	case class Capacitor(name: String, startNode: Int, endNode: Int, value: Z) {

		def copy() = Capacitor(name, startNode, endNode, value.copy())
	}

	case class Diode(name: String, startNode: Int, endNode: Int)

	case class Inductor(name: String, startNode: Int, endNode: Int, value: Z) {

		def copy() = Inductor(name, startNode, endNode, value.copy())
	}

	case class Resistor(name: String, startNode: Int, endNode: Int, value: Z) {

		def copy() = Resistor(name, startNode, endNode, value.copy())
	}

	case class Input(name: String, startNode: Int, endNode: Int, inhomogeneityType: Input.InhomogeneityType.Value,
					 inputType: Input.InputType.Value, coefficients: Array[Z], exponents: Array[Z]) {

		def copy() = Input(name, startNode, endNode, inhomogeneityType, inputType,
			if (coefficients == null) null else coefficients.clone(),
			if (exponents == null) null else exponents.clone())

		override def toString =
			s"Input($name, $startNode, $endNode, $inhomogeneityType, $inputType, " +
				s"${if (coefficients != null) coefficients.deep else coefficients}, ${if (exponents != null) exponents.deep else exponents})"
	}

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
	  * @param diodes     js.Array of diodes
	  * @param inputs     js.Array of inputs
	  * @param grounds    js.Array of Ints that are the NodeIDs of nodes with ground connections
	  */
	case class NetList(var capacitors: js.Array[Capacitor], var inductors: js.Array[Inductor],
					   var resistors: js.Array[Resistor], var diodes: js.Array[Diode],
					   var inputs: js.Array[Input], var grounds: js.Array[Int]) {

		def apply(name: String) = {

			if (capacitors.map(_.name).contains(name)) {
				capacitors(capacitors.map(_.name).indexOf(name))
			}
			else if (inductors.map(_.name).contains(name)) {
				inductors(inductors.map(_.name).indexOf(name))
			}
			else if (resistors.map(_.name).contains(name)) {
				resistors(resistors.map(_.name).indexOf(name))
			}
			else if (inputs.map(_.name).contains(name)) {
				inputs(inputs.map(_.name).indexOf(name))
			}
		}

		def copy: NetList = {
			val copy = NetList(js.Array[Capacitor](), js.Array[Inductor](), js.Array[Resistor](), js.Array[Diode](),
				js.Array[Input](), js.Array[Int]())
			for (i <- 0 until capacitors.length) {
				copy.capacitors += capacitors(i).copy()
			}
			for (i <- 0 until inductors.length) {
				copy.inductors += inductors(i).copy()
			}
			for (i <- 0 until resistors.length) {
				copy.resistors += resistors(i).copy()
			}
			for (i <- 0 until diodes.length) {
				copy.diodes += diodes(i).copy()
			}
			for (i <- 0 until grounds.length) {
				copy.grounds += grounds(i)
			}
			for (i <- 0 until inputs.length) {
				copy.inputs += inputs(i).copy()
			}
			copy
		}
	}

	object NetList {

		def fromJSON(json: String): NetList = {

			// parse JSON
			val parsedJSON = JSON.parse(json)
			val capacitors = if (js.isUndefined(parsedJSON.capacitors)) {
				js.Array[Capacitor]()
			}
			else {
				parsedJSON.capacitors.asInstanceOf[js.Array[js.Dynamic]]
					.map(i => Capacitor(i.name.asInstanceOf[String], i.startNode.asInstanceOf[Int], i.endNode.asInstanceOf[Int],
						new Z(i.realValue.asInstanceOf[Double], i.imagValue.asInstanceOf[Double])))
			}

			val inductors = if (js.isUndefined(parsedJSON.inductors)) {
				js.Array[Inductor]()
			}
			else {
				parsedJSON.inductors.asInstanceOf[js.Array[js.Dynamic]]
					.map(i => Inductor(i.name.asInstanceOf[String], i.startNode.asInstanceOf[Int], i.endNode.asInstanceOf[Int],
						new Z(i.realValue.asInstanceOf[Double], i.imagValue.asInstanceOf[Double])))
			}

			val resistors = if (js.isUndefined(parsedJSON.resistors)) {
				js.Array[Resistor]()
			}
			else {
				parsedJSON.resistors.asInstanceOf[js.Array[js.Dynamic]]
					.map(i => Resistor(i.name.asInstanceOf[String], i.startNode.asInstanceOf[Int], i.endNode.asInstanceOf[Int],
						new Z(i.realValue.asInstanceOf[Double], i.imagValue.asInstanceOf[Double])))
			}

			val inputs = if (js.isUndefined(parsedJSON.inputs)) {
				js.Array[Input]()
			}
			else {
				parsedJSON.inputs.asInstanceOf[js.Array[js.Dynamic]]
					.map(i => new Input(i.name.asInstanceOf[String], i.startNode.asInstanceOf[Int], i.endNode.asInstanceOf[Int],
						Input.InhomogeneityType.withName(i.inhomogeneityType.asInstanceOf[String]),
						Input.InputType.withName(i.inputType.asInstanceOf[String]),
						i.coefficients.asInstanceOf[js.Array[js.Dynamic]]
							.map(j => Z(j.realValue.asInstanceOf[Double], j.imagValue.asInstanceOf[Double])).toArray,
						if (i.exponents.asInstanceOf[js.Array[js.Dynamic]].nonEmpty) {
							i.exponents.asInstanceOf[js.Array[js.Dynamic]]
								.map(j => Z(j.realValue.asInstanceOf[Double], j.imagValue.asInstanceOf[Double])).toArray
						}
						else null))
			}

			val diodes = if (js.isUndefined(parsedJSON.diodes)) {
				js.Array[Diode]()
			}
			else {
				parsedJSON.diodes.asInstanceOf[js.Array[js.Dynamic]]
					.map(i => Diode(i.name.asInstanceOf[String], i.startNode.asInstanceOf[Int], i.endNode.asInstanceOf[Int]))
			}

			val grounds = if (js.isUndefined(parsedJSON.grounds)) {
				js.Array[Int]()
			}
			else {
				parsedJSON.grounds.asInstanceOf[js.Array[js.Dynamic]]
					.map(i => i.node.asInstanceOf[Int])
			}
			require(inputs.forall(i => if (i.exponents != null) i.exponents.length == i.coefficients.length else true), "Input matrix dimensions must agree.")
			require(inputs.forall(i => i.inhomogeneityType == inputs.head.inhomogeneityType), "Only single type of inhomogeneity supported.")

			new NetList(capacitors, inductors, resistors, diodes, inputs, grounds)
		}
	}

}

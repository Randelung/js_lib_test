package ch.ethz.ipes.buschr.maths

import JampackNew.{Eye, Z, Zdiagmat, Zmat}

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.JSON

/** Parse Netlist into an MNA DAE system.
  *
  * @param netlist Netlist to parse into a DAE system
  * @author Randolph Busch
  */
//noinspection ConvertNullInitializerToUnderscore
class MNA(val netlist: MNA.NetList) {

	private val _netlist = netlist.copy

	private var P: Zmat = null
	private var Ptranspose: Zmat = null
	private var Q: Zmat = null
	private var S: Zmat = null
	private var T: Zmat = null
	private var PAQinv: Zmat = null
	private var SBTinv: Zmat = null
	private var PBT: Zmat = null
	private var SBQ: Zmat = null
	private var M: Zmat = null
	private var u: Zmat = null
	private var diffEquation: DiffEquation = null
	private var QmTSBTinvSBQ: Zmat = null

	// enumerate nodes. nodes in the NetList don't necessarily correspond to IDs given here, since their name could be
	// anything as long as they are consistent
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
	// additionally, all capacitors get an extra variable to account for the derivative of the node difference.
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

	if (col.nonEmpty && col.length < A.nrow) {
		// netlist is DAE system

		P = new Zmat(row.length, A.ncol)
		Q = new Zmat(A.nrow, col.length)
		for (i <- row.indices) {
			P(i, row(i)) = 1
		}
		for (i <- col.indices) {
			Q(col(i), i) = 1
		}

		S = new Zmat(A.nrow - row.length, A.ncol)
		T = new Zmat(A.nrow, A.ncol - col.length)
		j = 0
		Ptranspose = P.transpose
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
		PAQinv = (P * A * Q).inv
		SBTinv = (S * B * T).inv
		PBT = P * B * T
		SBQ = S * B * Q

		M = PAQinv * (-P * B * Q + PBT * SBTinv * SBQ)
		u = -PAQinv * PBT * SBTinv * S * U1

		diffEquation = new DiffEquation(M)
		diffEquation.applyInhomogeneity(u, U2)

		QmTSBTinvSBQ = Q - T * SBTinv * SBQ
	}
	else if (col.length == A.nrow) {
		// netlist is pure ODE; never actually happens since there's at least two nodes that result in KCL equations,
		// which are linear.

		P = Eye.o(A.nrow)
		Q = Eye.o(A.nrow)
	}
	else {
		// netlist is purely linear
		S = Eye.o(A.nrow)
		T = Eye.o(A.nrow)
	}

	def appliedDataPoint = if (diffEquation == null) true else diffEquation.appliedDataPoint

	def applyDataPoint(t: Double, dataVector: Array[Double]): Unit = {
		if (diffEquation == null) {
			return
		}

		if (dataVector == null) {
			diffEquation.applyDataPoint(t, Array.fill(P.nrow)(0d))
		}
		else {
			diffEquation.applyDataPoint(t, dataVector)
		}
	}

	def applyStartingConditions(startingConditions: Array[Double]): Unit = {
		if (diffEquation == null) {
			return
		}

		if (startingConditions == null) {
			diffEquation.applyStartingConditions(Array.fill(P.nrow)(0))
		}
		else {
			diffEquation.applyStartingConditions(startingConditions)
		}
	}

	def getStateVector(t: Double): Array[Double] = {
		require(appliedDataPoint, "Need starting conditions first.")

		if (diffEquation != null) {
			diffEquation.solutionVector(t).getZ.map(_.head.re)
		}
		else {
			null
		}
	}

	private def solution(t: Double, index: Int): Double = {
		require(index >= 0 && index < A.nrow, "Index must be positive integer in the size bounds.")
		require(appliedDataPoint, "Need starting conditions first.")

		if (col.nonEmpty && col.length < A.nrow) {
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
		else if (col.length == A.nrow) {
			// no linear parts, S and T are empty
			// x = Q * x_diff
			(Q.get(index, index, 0, Q.ncol - 1) * diffEquation.solutionVector(t)) (0, 0).re
		}
		else {
			// no diff parts, P and Q are empty.
			// x = inv(B) * b
			if (_netlist.inputs.head.inhomogeneityType == MNA.Input.InhomogeneityType.exponential) {
				(B.inv.get(index, index, 0, B.nrow - 1) * U1 * new Zmat(U2.getZ.map(_.map(i => (i * t).exp)))) (0, 0).re
			}
			else {
				val array = Array.tabulate(U1.ncol)(i => Array(math.pow(t, i)))
				val array2 = Array.fill(U1.ncol, 1)(0d)
				(B.inv.get(index, index, 0, B.nrow - 1) * U1 * new Zmat(array, array2)) (0, 0).re
			}
		}
	}

	private def derivedSolution(t: Double, index: Int): Double = {
		require(index >= 0 && index < A.nrow, "Index must be positive integer in the size bounds.")
		require(appliedDataPoint, "Need starting conditions first.")

		if (col.nonEmpty && col.length < A.nrow) {
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
		else if (col.length == A.nrow) {
			// no linear parts, S and T are empty
			(Q.get(index, index, 0, Q.ncol - 1) * diffEquation.derivedSolutionVector(t)) (0, 0).re
		}
		else {
			if (_netlist.inputs.head.inhomogeneityType == MNA.Input.InhomogeneityType.exponential) {
				(B.inv.get(index, index, 0, B.nrow - 1) * U1 * new Zdiagmat(U2.getZ.map(_.head.re), U2.getZ.map(_.head.im)) * new Zmat(U2.getZ.map(_.map(i => (i * t).exp)))) (0, 0).re
			}
			else {
				val array = Array.tabulate(U1.ncol)(i => Array(math.pow(t, i)))
				val array2 = Array.fill(U1.ncol, 1)(0d)
				val diffArray = new Zmat(Array.ofDim[Double](U1.ncol, U1.ncol), array2)
				for (i <- 1 until diffArray.nrow) {
					diffArray(i, i - 1) = i
				}
				(B.inv.get(index, index, 0, B.nrow - 1) * U1 * diffArray * new Zmat(array, array2)) (0, 0).re
			}
		}
	}

	override def clone(): MNA = {
		val copy = new MNA(_netlist)
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
		copy.P = if (P == null) null else P.clone()
		copy.Q = if (Q == null) null else Q.clone()
		copy.S = if (S == null) null else S.clone()
		copy.T = if (T == null) null else T.clone()
		copy.Ptranspose = if (Ptranspose == null) null else Ptranspose.clone()
		copy.PAQinv = if (PAQinv == null) null else PAQinv.clone()
		copy.SBTinv = if (SBTinv == null) null else SBTinv.clone()
		copy.PBT = if (PBT == null) null else PBT.clone()
		copy.SBQ = if (SBQ == null) null else SBQ.clone()
		copy.M = if (M == null) null else M.clone()
		copy.u = if (u == null) null else u.clone()
		copy.diffEquation = if (diffEquation == null) null else diffEquation.clone()
		copy.QmTSBTinvSBQ = if (QmTSBTinvSBQ == null) null else QmTSBTinvSBQ.clone()
		copy
	}

	def elementVoltage(element: MNA.Element, t: Double): Double = {
		element match {
			case _: MNA.Capacitor =>
				require(_netlist.capacitors.contains(element), "Element not in netlist.")
			case _: MNA.Inductor =>
				require(_netlist.inductors.contains(element), "Element not in netlist.")
			case _: MNA.Resistor =>
				require(_netlist.resistors.contains(element), "Element not in netlist.")
			case _: MNA.Input =>
				require(_netlist.inputs.contains(element), "Element not in netlist.")
			case _ =>
				throw new IllegalArgumentException("Unknown element type.")
		}
		solution(t, nodes.indexOf(element.startNode)) - solution(t, nodes.indexOf(element.endNode))
	}

	def elementCurrent(element: MNA.Element, t: Double): Double = {
		element match {
			case _: MNA.Capacitor =>
				require(_netlist.capacitors.contains(element), "Element not in netlist.")
				solution(t, nodes.length + _netlist.capacitors.length + _netlist.capacitors.indexOf(element))
			case _: MNA.Resistor =>
				require(_netlist.resistors.contains(element), "Element not in netlist.")
				solution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.indexOf(element))
			case _: MNA.Inductor =>
				require(_netlist.inductors.contains(element), "Element not in netlist.")
				solution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.indexOf(element))
			case _: MNA.Input =>
				require(_netlist.inputs.contains(element), "Element not in netlist.")
				solution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.length + _netlist.inputs.indexOf(element))
			case _ =>
				throw new IllegalArgumentException("Unknown element type.")
		}
	}

	def derivedElementVoltage(element: MNA.Element, t: Double): Double = {
		element match {
			case _: MNA.Capacitor =>
				require(_netlist.capacitors.contains(element), "Element not in netlist.")
			case _: MNA.Inductor =>
				require(_netlist.inductors.contains(element), "Element not in netlist.")
			case _: MNA.Resistor =>
				require(_netlist.resistors.contains(element), "Element not in netlist.")
			case _: MNA.Input =>
				require(_netlist.inputs.contains(element), "Element not in netlist.")
			case _ =>
				throw new IllegalArgumentException("Unknown element type.")
		}
		derivedSolution(t, nodes.indexOf(element.startNode)) - derivedSolution(t, nodes.indexOf(element.endNode))
	}

	def derivedElementCurrent(element: MNA.Element, t: Double): Double = {
		element match {
			case _: MNA.Capacitor =>
				require(_netlist.capacitors.contains(element), "Element not in netlist.")
				derivedSolution(t, nodes.length + _netlist.capacitors.length + _netlist.capacitors.indexOf(element))
			case _: MNA.Resistor =>
				require(_netlist.resistors.contains(element), "Element not in netlist.")
				derivedSolution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.indexOf(element))
			case _: MNA.Inductor =>
				require(_netlist.inductors.contains(element), "Element not in netlist.")
				derivedSolution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.indexOf(element))
			case _: MNA.Input =>
				require(_netlist.inputs.contains(element), "Element not in netlist.")
				derivedSolution(t, nodes.length + 2 * _netlist.capacitors.length + _netlist.inductors.length + _netlist.resistors.length + _netlist.inputs.indexOf(element))
			case _ =>
				throw new IllegalArgumentException("Unknown element type.")
		}
	}
}

object MNA {

	def fromJSON(json: String) = {

		val netlist = NetList.fromJSON(json)
		(netlist, new MNA(netlist))
	}

	case class Capacitor(name: String, startNode: Int, endNode: Int, value: Z) extends Element {

		def copy() = Capacitor(name, startNode, endNode, value.copy())
	}

	case class Diode(name: String, startNode: Int, endNode: Int) extends Element

	case class Inductor(name: String, startNode: Int, endNode: Int, value: Z) extends Element {

		def copy() = Inductor(name, startNode, endNode, value.copy())
	}

	case class Resistor(name: String, startNode: Int, endNode: Int, value: Z) extends Element {

		def copy() = Resistor(name, startNode, endNode, value.copy())
	}

	case class Input(name: String, startNode: Int, endNode: Int, inhomogeneityType: Input.InhomogeneityType.Value,
					 inputType: Input.InputType.Value, coefficients: Array[Z], exponents: Array[Z]) extends Element {

		def copy() = Input(name, startNode, endNode, inhomogeneityType, inputType,
			if (coefficients == null) null else coefficients.clone(),
			if (exponents == null) null else exponents.clone())

		override def toString =
			s"Input($name, $startNode, $endNode, $inhomogeneityType, $inputType, " +
				s"${if (coefficients != null) coefficients.deep else coefficients}, ${if (exponents != null) exponents.deep else exponents})"
	}

	trait Element {

		val name: String
		val startNode: Int
		val endNode: Int
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

		def apply(name: String): Element = {

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
			else throw new IllegalArgumentException("Unknown name.")
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

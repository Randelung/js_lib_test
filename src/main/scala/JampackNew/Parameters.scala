package JampackNew

object Parameters {

	var BaseIndex: Int = 0

	var BaseIndexNotChangeable: Boolean = _

	def getBaseIndex: Int = BaseIndex

	def setBaseIndex(bx: Int) {
		if (BaseIndexNotChangeable) {
			throw new JampackException("Illegal attempt to change base index")
		}
		BaseIndex = bx
		BaseIndexNotChangeable = true
	}

	def adjustBaseIndex(A: Zmat) {
		BaseIndexNotChangeable = true
		A.basex = BaseIndex
		A.loadProperties()
	}

	def adjustBaseIndex(A: Zdiagmat) {
		BaseIndexNotChangeable = true
		A.basex = BaseIndex
		A.loadProperties()
	}

	var History: Boolean = true

	def setHistory() {
		History = true
	}

	def unsetHistory() {
		History = false
	}

	var OutputFieldWidth: Int = 12

	var OutputFracPlaces: Int = 3

	var PageWidth: Int = 240

	def setOutputParams(width: Int, frac: Int, pagewidth: Int) {
		OutputFieldWidth = if (width > 0) width else OutputFieldWidth
		OutputFracPlaces = if (frac > 0) width else OutputFracPlaces
		PageWidth = if (pagewidth > 0) pagewidth else PageWidth
	}
}

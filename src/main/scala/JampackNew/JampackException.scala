package JampackNew

class JampackException(s: String) extends Exception(s) {
	def this() {
		this(null)
	}
}
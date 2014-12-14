import parse.jsonParser

object performance {
	def main(args: Array[String]): Unit = {
		var a = 0
		var startTime : Long = 0
		var endTime : Long = 0
		var runTime : Long = 0
		var totalTime: Long = 0

		for (a <- 1 to 10002){
			startTime = java.lang.System.currentTimeMillis()
			var result = jsonParser.loads("""{"id": 1,"name": "A green door","price": 12.50,"tags": ["home", "green"] }""")
			endTime = java.lang.System.currentTimeMillis()
			if (a > 2){
				runTime = endTime-startTime
				totalTime += runTime
			}
		}
		println(totalTime/10000)
  	}
}
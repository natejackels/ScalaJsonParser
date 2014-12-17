import scala.util.parsing.json._

object performance {
	def main(args: Array[String]): Unit = {
		var a = 0
		var startTime : Double = 0
		var endTime : Double = 0
		var runTime : Double = 0
		var totalTime: Double = 0
		var result2 = ""

		// parses JSON in string form
		var result = jsonParser.loads("""{"id": 1,"name": "A green door","price": 12.50,"tags": ["home", "green"] }""")
		println(result)

		// run a amount of times for performance testing, in this case testing the generator
		for (a <- 1 to 25){
			startTime = java.lang.System.currentTimeMillis()
			result2 = jsonGenerator.dumps(result)
			endTime = java.lang.System.currentTimeMillis()
			if (a >2){
				runTime = endTime-startTime
				totalTime += runTime
			}
		}
		println(totalTime/23 + " Seconds for tests")
		println(result2)
  	}
}
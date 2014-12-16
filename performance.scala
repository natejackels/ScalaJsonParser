import parser.jsonParser
import generator.jsonGenerator
object performance {
	def main(args: Array[String]): Unit = {
		var a = 0
		var startTime : Double = 0
		var endTime : Double = 0
		var runTime : Double = 0
		var totalTime: Double = 0
		var result2 = ""
		for (a <- 1 to 10002){
			startTime = java.lang.System.currentTimeMillis()
			var result = jsonParser.loads("""{"id": 1,"name": "A green door","price": 12.50,"tags": ["home", "green"] }""")
			//result2 = jsonGenerator.loads(result)
			endTime = java.lang.System.currentTimeMillis()
			if (a > 2){
				runTime = endTime-startTime
				totalTime += runTime
			}
		}
		//println (result2)
		println(totalTime/1000 + " Seconds for tests")
  	}
}
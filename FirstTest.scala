
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import java.util.HashMap
import java.util.Arrays

//Main Test file for the jsonParser
//Contains a simply json string that is used in different ways to test the different functions of the parser
//Author:Brady Mahar


//Commands to Compile and Run:
//Compile: scalac -cp scalatest_2.11-2.2.1.jar jsonParser.scala FirstTest.scala
//Run: scala -cp scalatest_2.11-2.2.1.jar org.scalatest.run FirstTest
class FirstTest extends FunSuite with BeforeAndAfter{
	var loadResults = jsonParser.loads("""{"id": 1,"name": "A green door","price": 12.50,"tags": ["home", "green"] }""").asInstanceOf[HashMap[String, Any]]
	var id = 1
	var price = 12.5
	var firstTag = "home"
	var secondTag = "green"
	var name = "A green door"
	var valuesArray = Arrays.copyOf(loadResults.values.toArray, 4)
	var testPrice = valuesArray.head
	var testName = valuesArray.apply(1)
	var testId = valuesArray.apply(2)
	var testFirstTag = valuesArray.apply(3).asInstanceOf[Array[Object]].apply(0)
	var testSecondTag = valuesArray.apply(3).asInstanceOf[Array[Object]].apply(1)


	test("loads function called using small test"){
		assert(price.toString == testPrice.toString)
		assert(name.toString == testName.toString)
		assert(id.toString == testId.toString)
		assert(firstTag.toString == testFirstTag.toString)
		assert(secondTag.toString == testSecondTag.toString)
	}

	var loadDictResults = jsonParser.loads_dict(""""id": 1,"name": "A green door","price": 12.50,"tags": ["home", "green"] }""").asInstanceOf[Tuple2[HashMap[String, Any], Integer]].productElement(0).asInstanceOf[HashMap[String, Any]]
	valuesArray = Arrays.copyOf(loadDictResults.values.toArray, 4)

	testFirstTag = valuesArray.apply(3).asInstanceOf[Array[Object]].apply(0)
	testSecondTag = valuesArray.apply(3).asInstanceOf[Array[Object]].apply(1)
	
	test("loads_dict function called using small test"){
		assert(price.toString == valuesArray.head.toString)
		assert(name.toString == valuesArray.apply(1).toString)
		assert(firstTag.toString == testFirstTag.toString)
		assert(secondTag.toString == testSecondTag.toString)
	}


	var loadListResults = jsonParser.loads_list(""""home", "green"] }""").asInstanceOf[Tuple2[Array[Object], Integer]].productElement(0).asInstanceOf[Array[Object]]

	test("loads_list function called using small test"){
		assert(firstTag.toString == loadListResults.apply(0).toString)
		assert(secondTag.toString == loadListResults.apply(1).toString)
	}

	//Multiple tests for different secitons of the json
	var loadNumResults1 = jsonParser.loads_number("""1,"name": "A green door","price": 12.50,"tags": ["home", "green"] }""").asInstanceOf[Tuple2[Any, Integer]]
	var loadNumResults2 = jsonParser.loads_number("""12.50,"tags": ["home", "green"] }""").asInstanceOf[Tuple2[Any, Integer]]

	test("loads_number function called using small test"){
		assert(id.toString == loadNumResults1.productElement(0).toString)
		assert(price.toString == loadNumResults2.productElement(0).toString)
	}


	var loadStringResults1 = jsonParser.loads_string("""id": 1,"name": "A green door","price": 12.50,"tags": ["home", "green"] }""").asInstanceOf[Tuple2[String, Integer]]
	var loadStringResults2 = jsonParser.loads_string("""A green door","price": 12.50,"tags": ["home", "green"] }""").asInstanceOf[Tuple2[String, Integer]]

	test("loads_string function called using small test"){
		assert("id" == loadStringResults1.productElement(0).toString)
		assert(name.toString == loadStringResults2.productElement(0).toString)
	}

	var removeWhitespaceResults = jsonParser.remove_whitespace("""{"id": 1,"name": "A green door","price": 12.50,"tags": ["home", "green"] }""").asInstanceOf[String]
	var finalResults = """{"id": 1,"name": "A green door","price": 12.50,"tags": ["home", "green"] }"""

	test("remove_whitespace function called with using small test"){
		assert(finalResults == removeWhitespaceResults)
	}













}
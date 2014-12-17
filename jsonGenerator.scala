import scala.actors.Actor._
import java.util.ArrayList
import java.util.HashMap
import java.util.Scanner

object jsonGenerator {
	def loads(in:Array[Any]):String = {
	  var finalVal = new Array[String](in.length)
	  var continue = true
	  var count = 0
	  val listActor = actor { // result compilation
		  while(continue){
		    receive {
		      case msg =>
		        val (x:Integer, y:String) = msg
		        finalVal(x) = y
		        count += 1
		        if(count == in.length){
		          continue = false
		        }
		    }
		  }
	  }
	  
	  val i = 0
	  in.foreach { // actor calls
	    case x:Array[Any]=>
	      listActor ! (in.indexOf(x), loads(x))
	    case x:HashMap[Any,Any]=>
	      listActor ! (in.indexOf(x), loads(x))
	    case x:Number=>
	      listActor ! (in.indexOf(x), loads(x))
	    case x:String=>
	      listActor ! (in.indexOf(x), "\"" + x + "\"")
	    case x:jsonable=>
	      listActor ! (in.indexOf(x), loads(x))
	    case x: Any =>
	      continue = false
	      throw new Exception("Class not accepted")
	  }

	  while(continue){} // wait for actor to finish
	  var result = "["
	  finalVal.foreach { // turn into a string
	    case x=>
	    result += x
	    if((finalVal.indexOf(x)+1) < finalVal.length){ result += ", "}
	  }
	  result += "]"
	  return result
	}


	def loads(in:HashMap[Any,Any]):String = {
	  if(in.size() == 0){
	    return "{}"
	  }
	  var finalKey = new Array[String](in.size())
	  var finalVal = new Array[String](in.size())
	  var Keys = true
	  var Values = true
	  var kcount = 0
	  var vcount = 0
	  val keyActor = actor {
	    while(Keys){
	      receive {
	        case msg =>
	          val(x:Integer, y:String) = msg
	          finalKey(x) = y
	          kcount += 1
	          if(kcount == in.size()){
	            Keys = false
	          }
	      }
	    }
	  }
	  val valActor = actor {
	    while(Values){
	      receive {
	        case msg =>
	          val(x:Integer, y:String) = msg
	          finalVal(x) = y
	          vcount += 1
	          if(vcount == in.size()){
	            Values = false
	          }
	      }
	    }
	  } 
	  val kset = in.keySet().toArray()
	  kset.foreach {
	    case x:String =>
	      keyActor ! (kset.indexOf(x), x)
	    case x:jsonable =>
	      keyActor ! (kset.indexOf(x), loads(x))
	    case x:Number =>
	      keyActor ! (kset.indexOf(x), loads(x))
	    case x:Any =>
	      Keys = false
	      throw new Exception("Must have a string type for Dictionary Key")
	  }
	  val vset = in.values().toArray()
	  vset.foreach{
	    case x:Array[Any]=>
	      valActor ! (vset.indexOf(x), loads(x))
	    case x:HashMap[Any,Any]=>{
	      val xresult = loads(x)
	      valActor ! (vset.indexOf(x), xresult)
	      }
	    case x:Number=>
	      valActor ! (vset.indexOf(x), loads(x))
	    case x:String=>
	      valActor ! (vset.indexOf(x), "\"" + x + "\"")
	    case x:jsonable=>
	      valActor ! (vset.indexOf(x), x.jsonify())
	    case x: Any =>
	      Values = false
	      throw new Exception("Class not accepted")
	  }
	  while((Keys==true) || (Values==true)){}
	  var result = "{"
	  var i = 0
	  while(i < in.size()){
	    //while(finalKey(i) == null){}
	    result += "\"" + finalKey(i) + "\":"
	    //while(finalVal(i) == null){}
	    result += finalVal(i)
	    if ((i+1) < in.size()){
	      result += ", "
	    }
	    i+=1
	  }
	  result += "}"
	  return result
	}


	def loads(in:Number):String = { // DONE
	  return in.toString()
	}


	def loads(in:jsonable):String = { // DONE
	  return in.jsonify
	}
}
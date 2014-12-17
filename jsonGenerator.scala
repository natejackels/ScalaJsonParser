import scala.actors.Actor._
import java.util.ArrayList
import java.util.HashMap
import java.util.Scanner

object jsonGenerator {
	def dumps_array(in:Array[Any]):String = {
	  //println("in dumps")
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
		          //println("last actor")
		          continue = false
		        }
		    }
		  }
	  }
	  //println("out of actor")
	  val i = 0
	  in.foreach { // actor calls
	    case x:Array[Any]=>
	      listActor ! (in.indexOf(x), dumps_array(x))
	    case x:HashMap[Any,Any]=>
	      listActor ! (in.indexOf(x), dumps_map(x))
	    case x:Number=>
	      listActor ! (in.indexOf(x), dumps_number(x))
	    case x:String=>
	      listActor ! (in.indexOf(x), "\"" + x + "\"")
	    case x:jsonable=>
	      listActor ! (in.indexOf(x), dumps_json(x))
	    case x =>
	      if(x == false){
	        listActor ! (in.indexOf(x), "false")
	      } else if (x == true){
	        listActor ! (in.indexOf(x), "true")
	      } else if (x == null){
	        listActor ! (in.indexOf(x), "null")
	      } else {
	    	continue= false
	    	throw new Exception("Class not accepted")
	      }
	  }
	  //println("out of in for each")
	  while(continue){for(xyz<-0 to 1){}} // wait for actor to finish
	  //println("Passsed continue")
	  var result = "["
	  finalVal.foreach { // turn into a string
	    case x=>
	    result += x
	    if((finalVal.indexOf(x)+1) < finalVal.length){ result += ", "}
	  }
	  result += "]"
	  return result
	}

	
	def dumps_map(in:HashMap[Any,Any]):String = {
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
	      keyActor ! (kset.indexOf(x), dumps_json(x))
	    case x:Number =>
	      keyActor ! (kset.indexOf(x), dumps_number(x))
	    case x =>
	      if(x == false){
	        keyActor ! (kset.indexOf(x), "false")
	      } else if (x == true){
	        keyActor ! (kset.indexOf(x), "true")
	      } else if (x == null){
	        keyActor ! (kset.indexOf(x), "null")
	      } else {
	    	Keys = false
	    	throw new Exception("Must have a string type for Dictionary Key")
	      }
	  }
	  val vset = in.values().toArray()
	  vset.foreach{
	    case x:Array[Any]=>
	      valActor ! (vset.indexOf(x), dumps_array(x))
	    case x:HashMap[Any,Any]=>
	      valActor ! (vset.indexOf(x), dumps_map(x))
	    case x:Number=>
	      valActor ! (vset.indexOf(x), dumps_number(x))
	    case x:String=>
	      valActor ! (vset.indexOf(x), "\"" + x + "\"")
	    case x:jsonable=>
	      valActor ! (vset.indexOf(x), dumps_json(x))
	    case x =>
	      if(x == false){
	        valActor ! (vset.indexOf(x), "\"false\"")
	      } else if (x == true){
	        valActor ! (vset.indexOf(x), "\"true\"")
	      } else if (x == null){
	        valActor ! (vset.indexOf(x), "\"null\"")
	      } else {
	    	Values = false
	    	throw new Exception("Class not accepted")
	      }
	  }
	  while((Keys==true) || (Values==true)){for(xyz<-0 to 1){}}
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


	def dumps_number(in:Number):String = { // DONE
	  return in.toString()
	}


	def dumps_json(in:jsonable):String = { // DONE
	  return in.jsonify
	}
	def dumps(in:Any):String = {
	  if(in == false){
	    return "false"
	  } else if (in == true){
	    return "true"
	  } else if (in == null){
	    return "null"
	  } else if( in.isInstanceOf[jsonable] ){
	  	return dumps_json(in.asInstanceOf[jsonable])
	  } else if(in.isInstanceOf[Array[Any]]){
	  	return dumps_array(in.asInstanceOf[Array[Any]])
	  } else if(in.isInstanceOf[HashMap[Any,Any]]){
	  	return dumps_map(in.asInstanceOf[HashMap[Any, Any]])
	  } else if(in.isInstanceOf[Number]){
	  	return dumps_number(in.asInstanceOf[Number])
	  } else {
		throw new Exception("Type not accepted")
	  }
	}
}
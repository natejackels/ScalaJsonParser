import scala.actors.Actor._
import java.util.ArrayList
import java.util.Scanner

object jsonParser {
  def loads(in:String):Any = {
    var input = in;
    val i0 = input.charAt(0)
    var continue = true // Use this to break in all threads if exception is thrown XXX
    if(i0 == '{'){
      return loads_dict(input)
    } else if (i0 == '['){
      return loads_list(input)
    } else if (i0 == '"'){
      val endOfString = input.indexOf("\"", 1)
    } else if (i0 == 'n'){
      if(input.substring(0, 3).equals("null")){
        if(remove_whitespace(input.substring(4)).length() == 0){
          return null
        } else {
          throw new Exception("Parse-Exception: Extra Data")
        }
      }
    } else if (i0 == 't'){
      if (input.substring(0, 3).equals("true")){
        if(remove_whitespace(input.substring(4)).length() == 0){
          return true
        } else {
          throw new Exception("Parse-Exception: Extra Data")
        }
      }
    } else if (i0 == 'f'){
      if (input.substring(0, 4).equals("false")){
        if (remove_whitespace(input.substring(5)).length() == 0){
          return false
        } else {
          throw new Exception("Parse-Exception: Extra Data")
        }
      }
    } else if ("0123456789".contains(i0)){
      
    } else {
      throw new Exception("Parse-Exception: No JSON Found")
    }
  }
  
  def loads_dict(input:String):(Map[String,Any],Integer) = {
    return null
  }
  
  def loads_list(input:String):(Array[Object],Integer) = {
    var result = new ArrayList[Object]
    var index = 0
    if(input(0) == '['){index=1}
    var next = input(index)
    var loop = true
    var nextIsComma = false
    while(loop){
      println(next + ":" + nextIsComma + ":" + index)
      if(nextIsComma){
        val l = result.toArray()
        print("Result[")
        for(i<-0 to l.length-1){
          print(l(i) + ",")
        }
        println("]")
        while((input(index) == ' ') ||
          (input(index) == '\t') || 
          (input(index) == '\n') ||
          (input(index) == '\r')){
        	index+=1
        } // skip over whitespace
        if(input(index) != ']'){
          if(input(index) != ','){
            throw new Exception("Couldn't find list serperator comma")
          }
          index+=1
          nextIsComma = false
        } else {
          loop = false
        }
      } else if(next == ']'){ // DONE
        loop = false // end of list: Exit
      } else if (next == '"'){ // DONE
        // string within a list
        index+=1
        val begin = index
        while(input(index)!='"'){index+=1}
        loop = true
        var x = input.substring(begin, index)
        result.add(x)
        index+=1
        nextIsComma = true
      } else if (next == '{'){
        //dict within a list
        var(x:Object,y:Integer) = loads_dict(input.substring(index))
        result.add(x)
        index += y
        nextIsComma = true
      } else if (next == '['){ // DONE
        //list within a list
        val(x:Array[Object], y:Integer) = loads_list(input.substring(index))
        result.add(x)
        index+=y
        nextIsComma = true
      } else if ("1234567890.-".contains(next)){ // DONE
        // number
        val (x:Object, y) = loads_number(input.substring(index))
        result.add(x)
        println("Added " + x)
        index += y
        nextIsComma = true
      } else if (" \t\n\r".contains(next)){ // DONE
        // Pass on whitespace
        index+=1
      } else {
        throw new Exception("Parse-Exception: Extra Data")
      }
      next = input(index)
    }
    return (result.toArray(),index)
  }
  
  def loads_number(input:String):(Any,Integer) = { // DONE, THROWS
    var index = 0
    if(input(0) == '-'){index=1}
    // assume that negative sign is found on entry
    while("0123456789.".contains(input(index))){index+=1} // find end of number
    val in = new Scanner(input.substring(0, index))
    var result:Any = null
    if(in.hasNextInt()){
      result = in.nextInt()
    } else if (in.hasNextDouble()){
      result = in.nextDouble()
    } else if (in.hasNextBigInteger()){
      result = in.nextBigInteger()
    } else if (in.hasNextFloat()){
      result = in.nextFloat()
    } else if (in.hasNextLong()){
      result = in.nextLong()
    } else if (in.hasNextShort()){
      result = in.nextShort()
    } else {
      if(input.indexOf(".") != -1){
        if(input.substring(input.indexOf(".")).indexOf(".") != -1){
          throw new Exception("JSON Parser: Multiple Decimals in Number")
        } else {
          throw new Exception("JSON Parser: Error parsing number")
        }
      } else {
        throw new Exception("JSON Parser: Error parsing number")
      }
    }
    return (result, index)
  }
  
  def remove_whitespace(input:String):String = {
    var i = 0
    while((input.charAt(i) == ' ') ||
          (input.charAt(i) == '\t') || 
          (input.charAt(i) == '\n') ||
          (input.charAt(i) == '\r')){
      i=i+1
    }
    println(i)
    return input.substring(i)
  }
}
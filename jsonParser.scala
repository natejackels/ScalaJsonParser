import scala.actors.Actor._
import java.util.ArrayList
import java.util.HashMap
import java.util.Scanner

/**
 * Types:
 * []
 * {} // can only use strings as keys
 * "abc"
 * 1234.5
 * true
 * false
 * null
 */

object jsonParser {
  def loads(in:String):Any = {
    val input = remove_whitespace(in)
    val i0 = input(0)
    if(i0 == '{'){ // DONE
      var(result, index) = loads_dict(input.substring(1))
      index+=1
      if(index > in.length){ index = in.length }
      if(remove_whitespace(in.substring(index)).length() != 0){ throw new Exception("Parse-Exception: Extra Data")}
      return result
    } else if (i0 == '['){ // DONE
      var(result:Array[Object], index) = loads_list(input.substring(1))
      index+=1
      if(index > in.length){ index = in.length }
      if(remove_whitespace(in.substring(index)).length() != 0){ throw new Exception("Parse-Exception: Extra Data:" + input.substring(index))}
      return result
    } else if (i0 == '"'){ // TODO XXX
      val endOfString = input.indexOf("\"", 1)
    } else if (i0 == 'n'){ // DONE
      if(input.substring(0, 4).equals("null")){
        if(remove_whitespace(input.substring(4)).length() != 0){ throw new Exception("Parse-Exception: Extra Data") }
        return null
      }
    } else if (i0 == 't'){ // DONE
      if (input.substring(0, 4).equals("true")){
        if(remove_whitespace(input.substring(4)).length() != 0){ throw new Exception("Parse-Exception: Extra Data")}
        return true
      }
    } else if (i0 == 'f'){ // DONE
      if (input.substring(0, 5).equals("false")){
        if (remove_whitespace(input.substring(5)).length() != 0){ throw new Exception("Parse-Exception: Extra Data")}
        return false
      }
    } else if ("0123456789.-".contains(i0)){ // DONE
      val(x, y) = loads_number(input.substring(0))
      if(remove_whitespace(in.substring(y)).length() != 0){throw new Exception("Parse-Exception: Extra Data")}
      return x
    }
    // else:
    throw new Exception("Parse-Exception: No JSON Found")
  }

  def loads_dict(input:String):(HashMap[String,Any],Integer) = { // TODO XXX
    var index = 0
    var result = new HashMap[String, Any]
    // While loop here
    var loop = true
    var k:String = null
    var v:Object = null
    while((index < input.length) && loop){
      if(input(index) == '}'){
        loop = false
      } else if (input(index) == '"'){
        var (k:String,i) = loads_string(input.substring(index+1))
        index += (i+1)
        while((index < input.length) &&
            ((input(index) == ' ' ) ||
             (input(index) == '\t') ||
             (input(index) == '\n') ||
             (input(index) == '\r'))){
          index += 1
        }
        if(input(index) != ':'){
          throw new Exception("Couldn't find dict colon:" + input.substring(index))
        }
        index+=1
        while((index < input.length) &&
            ((input(index) == ' ' ) ||
             (input(index) == '\t') ||
             (input(index) == '\n') ||
             (input(index) == '\r'))){
          index += 1
        }
        //Got Key, now get Val
        if(input(index) == '"'){
          // string
          var (v:String, i) = loads_string(input.substring(index+1))
          index += (i+1)
          result.put(k, v)
        } else if (input(index) == '{'){
          // dict
          index+=1
          var (v:HashMap[String,Any], i) = loads_dict(input.substring(index))
          index += (i)
          result.put(k, v)
        } else if (input(index) == '['){
          // list
          index+=1
          var (v:Array[Object], i) = loads_list(input.substring(index))
          index += (i)
          result.put(k, v)
        } else if (input(index) == 't'){
          // true?
          if(input.substring(index, index+4).equals("true")){
            index += 4
            result.put(k, true)
          } else {
            throw new Exception("Parse-Exception: Bad Dict Value")
          }
        } else if (input(index) == 'f'){
          // false?
          if(input.substring(index,index+5).equals("false")){
            index+=5
            result.put(k, false)
          } else {
            throw new Exception("Parse-Exception: Bad Dict Value")
          }
        } else if (input(index) == 'n'){
          // null?
          if(input.substring(index, index+4).equals("null")){
            index+=4
            v = null
          } else {
            throw new Exception("Parse-Exception: Bad Dict Value")
          }
          result.put(k, v)
        } else if ("-.0123456789".contains(input(index))){
          // number
          val(v:Any,i) = loads_number(input.substring(index))
          index+=i
          result.put(k, v)
        } else if(" \t\n\r".contains(input(index))){
          index+=1
        } else {
          throw new Exception("Parser-Exception: Bad Dict Value")
        }
        // Inserted, now find either '}' or ','
        while(index < input.length &&
            ((input(index) == ' ') ||
            (input(index) == '\t') ||
            (input(index) == '\n') ||
            (input(index) == '\r'))){
          index+=1
        }
        if(index < input.length){
          if(input(index) == '}'){
            loop = false
          } else if (input(index) == ','){
            index+=1
          } else {
            throw new Exception("Parser-Exception: Dict missing comma or terminating bracket")
          }
        } else {
          throw new Exception("Parser-Exception: Dict missing terminating bracket")
        }
      } else if (" \t\n\r".contains(input(index))){
        index+=1
      } else {
        throw new Exception("Parser-Exception: Bad Dict Key-Type / Extra Data")
      }      
    }
    if(input(index) == '}'){
      while((index < input.length) &&
         ((input.charAt(index) == ' ' ) ||
          (input.charAt(index) == '\t') || 
          (input.charAt(index) == '\n') ||
          (input.charAt(index) == '\r'))){
        index=index+1
      }
      return (result,index+1)
    }
    throw new Exception("Parser-Exception: Dict missing terminating bracket")
  }

  def loads_list(input:String):(Array[Object],Integer) = {
    var result = new ArrayList[Any]
    var index = 0
    var next = input(index)
    var loop = true
    var nextIsComma = false
    while(loop && (index < input.length)){
      if(nextIsComma){
        while((index < input.length) &&
         ((input(index) == ' ' ) ||
          (input(index) == '\t') || 
          (input(index) == '\n') ||
          (input(index) == '\r'))){
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
      // END if(nextIsComma)
      } else if(next == ']'){
        loop = false // end of list: Exit
      } else if (next == '"'){
        var (x, y) = loads_string(input.substring(index+1))
        result.add(x)
        index+=(y+1)
        nextIsComma = true
      } else if (next == '{'){
        //dict within a list
        index+=1
        var(x:Object,y:Integer) = loads_dict(input.substring(index))
        result.add(x)
        index += y
        nextIsComma = true
      } else if (next == '['){
        //list within a list
        index+=1
        val(x:Array[Object], y:Integer) = loads_list(input.substring(index+1))
        result.add(x)
        index+=y+1
        nextIsComma = true
      } else if ("1234567890.-".contains(next)){
        // number
        val (x:Object, y) = loads_number(input.substring(index))
        result.add(x)
        index += y
        nextIsComma = true
      } else if (" \t\n\r".contains(next)){
        // Pass on whitespace
        index+=1
      }else if (next == 'n'){
        if(input.substring(index, index+4).equals("null")){
          index +=4
          result.add(null)
          nextIsComma = true
        } else {
          throw new Exception("Parse-Exception: Extra Data")
        }
      } else if (next == 't'){
        if(input.substring(index, index+4).equals("true")){
          index+=4
          result.add(true)
        } else { throw new Exception("Parse-Exception: Extra Data")}
        nextIsComma = true
      } else if (next == 'f'){
        if (input.substring(index, index+5).equals("false")){
          index+=5
          result.add(false)
        } else { throw new Exception("Parse-Exception: Extra Data")}
        nextIsComma = true
      } else {
        throw new Exception("Parse-Exception: Extra Data")
      }
      if(index < input.length){
        next = input(index)
      }
    }
    if(index > input.length-1){
      if(input(index-1) != ']'){ throw new Exception("Parse-Exception: Open ended list")}
    } else {
      if(input(index) != ']'){ throw new Exception("Parse-Exception: Open ended list")}
    }
    return (result.toArray(),index+1)
  }

  def loads_number(input:String):(Any,Integer) = {
    var index = 0
    if(input(0) == '-'){index=1}
    // assume that negative sign is found on entry
    while((index < input.length) && ("0123456789." contains(input(index)))){index+=1} // find end of number
    val in = new Scanner(input.substring(0, index))
    var result:Any = null
    if(in.hasNextInt){
      result = in.nextInt
    } else if (in.hasNextDouble){
      result = in.nextDouble
    } else if (in.hasNextBigInteger){
      result = in.nextBigInteger
    } else if (in.hasNextFloat){
      result = in.nextFloat
    } else if (in.hasNextLong){
      result = in.nextLong
    } else if (in.hasNextShort){
      result = in.nextShort
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

  def loads_string(input:String):(String,Integer) = {
    var index = 0
    var loop = true
    while((index < input.length) && (loop)){
      if(input(index) == '\\'){
        if((index+1 < input.length) && (input(index+1) == '"')){
          index += 2
        } else {
          index += 1
        }
      } else if (input(index) == '"'){
        loop = false
      } else {
        index += 1
      }
    }
    if(input(index) != '"'){
      throw new Exception("Parse-Exception: No String Termination")
    }
    return (input.substring(0,index), index+1)
  }

  def remove_whitespace(input:String):String = {
    if(input.length == 0){ return input}
    var i = 0
    while((i < input.length) &&
         ((input.charAt(i) == ' ' ) ||
          (input.charAt(i) == '\t') || 
          (input.charAt(i) == '\n') ||
          (input.charAt(i) == '\r'))){
      i=i+1
    }
    return input.substring(i)
  }
}
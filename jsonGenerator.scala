package generator
import scala.actors.Actor._
import java.util.ArrayList
import java.util.HashMap
import java.util.Scanner

object jsonGenerator {
	def loads(in:Any):String = {
			val finalVal = "Temp holder"
			if(in.getClass.getSimpleName == "HashMap"){
			   
			  
			}
			if(in.getClass.getSimpleName == "Array"){
			  
			}
			
			//We might need to switch up our implementation. 
			//I have tried to get this working using the Any type, 
			//and so far it seems impossible for scala to handle
			//multiple types (in terms of pulling data from it)
			
			//I also tried switching up jasonParser to return 
			//more specific data types. However it seems the 
			//max you can return is 2 using Either. 
			
			//So what we might have to do, is return the results
			//directly to a specific Generator for each possible
			//return type. The only problem with this is if
			//any "Any" types are nested within the HashMap
			// or Array, etc. They wont be able to be converted
			//to their native type (at least from what i have tried
			
			//Going to head to bed for now. But will work on this
			//tomorrow
			
			//If you want to dabble, in.getClass. has some interesting
			//features that might be something that could help. 
			//I couldn't find anything specific though that would allow
			//the casting to the native type. 
			return finalVal;
	}
}
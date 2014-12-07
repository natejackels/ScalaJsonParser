class Person extends jsonable{
  var name:String = "name"
  def Person(){
    
  }
  override def jsonify():String = {
    return "\"Person[" + name + "]\""
  }
}
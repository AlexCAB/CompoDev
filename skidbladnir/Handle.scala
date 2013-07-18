package skidbladnir

trait Handle{
  val interfaces:List[String] = null
  override def toString():String = {
    val sh = this.asInstanceOf[Shadow]
    if(sh.isInstanceOf[BaseShadow]){
      "base '" + sh.name + "'"}
    else{
      if(sh.name != null){
        "static '" + sh.name + "'"}
      else{
        "dynamic " + sh.proto.getClass().getName() + "@" +System.identityHashCode(sh)}}
  } 
}

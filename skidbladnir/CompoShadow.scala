package skidbladnir

class CompoShadow(proto:Prototype) extends Shadow(proto) {
  //Fields
  var constructor:List[(Handle)=>Unit] = List()   
  var deconstructor:List[(Handle,Int)=>Unit] = List()   
  var rootIntrface:IntrDescr = null   
  //Prototype methods
  def addRootInterface(intr:Half, switching:(Handle, Handle)=>Unit) = { 
    if(compoFlags.stat != 0){throw new CompoException("You can not add inetefaces in runtime")}
    rootIntrface = new IntrDescr(this, null, null, intr, false/*plug*/, true/*mono*/, true /*root*/, null, null, switching)
  } 
}
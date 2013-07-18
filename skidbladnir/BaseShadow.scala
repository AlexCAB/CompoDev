package skidbladnir

class BaseShadow(proto:Prototype) extends Shadow(proto) {
  //Fields
  var constructor:List[()=>Unit] = List() 
  var deconstructor:List[(Int)=>Unit] = List() 
} 
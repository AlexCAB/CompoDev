package skidbladnir
import scala.collection.mutable.{Map => MutMap},
       java.lang.reflect.Field,
       java.lang.Class
              

protected class Shadow (val proto:Prototype) extends Handle with Functions{  
  //Fields
  val compoFlags = new CompoFlags
  var runtime:Runtime = null
  var name:String = null
  var compoThtead:Thread = null
  var compoThteadNotify:Boolean = true
  val compoInterfaces = MutMap[String, IntrDescr]() //Supported interfaces list 
  var interfCount = 0
  val rootConnecions =  MutMap[Shadow, IntrDescr]() //List of component which connect their the root interface, to this.
  //Runtime methods (create/destroy)
  def init() = {
    //Get field map
    val fm = MutMap[Half, (String, String)]()  // -> (name, type name)
    var c:Class[_] = proto.getClass()
    while(c != null){
      c.getDeclaredFields().foreach(e => {
        e.setAccessible(true)
        val o = e.get(proto)
        if(o.isInstanceOf[Half]){
          fm += (o.asInstanceOf[Half] -> (e.getName(), e.getType().getName()))}})
      c = c.getSuperclass()}
    //Check interfaces names
    fm.foreach(e =>{
      val (in,itn) = (e._2._1,e._2._2)
      val sp = itn.split("\\$")    
      if(sp.size < 2){throw new CompoException("Interface half can't be defined in outside object, extends Interface")}
      val ccn = sp(sp.size - 1)  
      if(!(ccn == "Am" || ccn == "Pm")){throw new CompoException("Incorrect interface half class name '" + ccn + "' mast be 'Am' or 'Pm'")}})
    //Matching interfaces
    var hil = List[String]()  
    val ci = MutMap[String, IntrDescr]()
    compoInterfaces.foreach(e =>{
      val id = e._2; val in = fm(id.halfLink)
      id.name = in._1; id.typeName = in._2.split("\\$").init.mkString("$")
      ci += (id.name -> id)
      hil :+= id.name})
    compoInterfaces.clear(); compoInterfaces ++= ci 
    //Matching root
     if(this.isInstanceOf[CompoShadow]){
       val ri = this.asInstanceOf[CompoShadow].rootIntrface
       val in = fm(ri.halfLink)
       ri.name = in._1; ri.typeName = in._2.split("\\$").init.mkString("$")
       hil :+= ri.name}
    //Check interfaces
    compoInterfaces.foreach(e =>{checkIntrface(e._2.halfLink, e._2.mono_multi, e._1)})  
    //Check root
    if(this.isInstanceOf[CompoShadow]){
      val ri = this.asInstanceOf[CompoShadow].rootIntrface
      checkIntrface(ri.halfLink, ri.mono_multi, ri.name)}
    //Add interface list   
    val fl = this.getClass().getSuperclass().getDeclaredField("interfaces")
    fl.setAccessible(true)
    fl.set(this, hil) 
  }
  def start() = {
     if(compoThtead != null && compoFlags.stat == 2){compoThtead.start()} //Start internal thread
  }
  def deinit() = {
  }
  //Runtime methods (interfaces)
  def getCompoInterface(in:String):IntrDescr = { 
   // if(compoFlags.stat == 5){throw new CompoException("Component '" + this + "' not exist, state: " +  compoFlags.stat)}
    if(compoInterfaces.contains(in)){
      compoInterfaces(in)}
    else{
      if(proto.isInstanceOf[Compo]){
        this.asInstanceOf[CompoShadow].rootIntrface}
      else{
        null}}
  }  
  //Prototype methods
  def addInterface(jack_plug:Boolean, mono_multi:Boolean, intr:Half, connection:(Handle)=>Unit, disconnection:(Handle,Int)=>Unit, switching:(Handle, Handle)=>Unit) = { //Add to interface list
    if(compoFlags.stat != 0){throw new CompoException("You can not add inetefaces in runtime")}
    interfCount += 1
    compoInterfaces += (interfCount.toString -> new IntrDescr(this, null, null, intr, jack_plug, mono_multi, false, connection, disconnection, switching))    
  }
  //Functions
  def checkIntrface(intr:Half, mono_multi:Boolean, in:String) = {       
    val ic = getAmOrPmClass(intr.getClass())
    try{
      val af = ic.getDeclaredField("imports")
      val ftn = af.getType().getName()
      if(mono_multi){
        val s = ftn.split("\\$").last
        if(!(s == "Am" ||s == "Pm")){throw new Exception("Incorrect type")}}
      else{
        val s = ftn.split("\\.").last
        if(s != "Map"){throw new Exception("Incorrect type")}}}
    catch{
      case e:Exception => {
        throw new CompoException("Incorrect field 'imports' in intrface '" + in + "' of " + this + " exeption: " + e)}} 
  }
}


class CompoFlags {
 // var exist = false //Component exist
  var operation = 0 //Connection/disconnection
  var stat = 0     //0-init, 1-construction, 2-work, 3-prepare 4-destruction, 5-die
}

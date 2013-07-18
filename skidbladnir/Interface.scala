package skidbladnir
import scala.collection.mutable.{Map => MutMap},
       java.util.concurrent.locks.{Lock, ReentrantLock}


class Interface
protected trait Half
class Mono extends Half
class Multi extends Half

protected class IntrDescr(
  val compo:Shadow,  
  var name:String,  
  var typeName:String, 
  val halfLink:Half,
  val jack_plug:Boolean,   //true = jack
  val mono_multi:Boolean,  //true = mono
  val root:Boolean,
  val connection:(Handle)=>Unit,
  val disconnection:(Handle,Int)=>Unit,
  val switching:(Handle, Handle)=>Unit){
  val connections = MutMap[Shadow, Connection]()
  val lock:Lock = new ReentrantLock
  }


protected class Connection (val descr:IntrDescr, var state:Boolean)
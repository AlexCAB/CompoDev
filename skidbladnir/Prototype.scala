package skidbladnir
import scala.collection.mutable.{Map => MutMap},
       java.lang.reflect.Field
        

protected trait Prototype {
  //Links
  private val shadow:Shadow = if(this.isInstanceOf[Base]){new BaseShadow(this)}else{new CompoShadow(this)}  
  //Fields
  private val compo = this
  //Interfaces definition 
  protected final def jack[T <: Mono](intr:T, connection:(Handle)=>Unit = (c:Handle)=>Unit, disconnection:(Handle,Int)=>Unit = (c:Handle,i:Int)=>Unit, switching:(Handle, Handle)=>Unit = (dc:Handle, cc:Handle)=>Unit):T = {
    shadow.addInterface(true, true, intr, connection, disconnection, switching)
    intr
  }
  protected final def plug[T <: Mono](intr:T, connection:(Handle)=>Unit = (c:Handle)=>Unit, disconnection:(Handle,Int)=>Unit = (c:Handle,i:Int)=>Unit, switching:(Handle, Handle)=>Unit = (dc:Handle, cc:Handle)=>Unit):T = {
   shadow.addInterface(false, true, intr, connection, disconnection, switching)
    intr
  }
  protected final def multijack[T <: Multi](intr:T, connection:(Handle)=>Unit = (c:Handle)=>Unit, disconnection:(Handle,Int)=>Unit = (c:Handle,i:Int)=>Unit):T = {
   shadow.addInterface(true, false, intr, connection, disconnection, null)
    intr
  }
  protected final def multiplug[T <: Multi](intr:T, connection:(Handle)=>Unit = (c:Handle)=>Unit, disconnection:(Handle,Int)=>Unit = (c:Handle,i:Int)=>Unit):T = {
    shadow.addInterface(false, false, intr, connection, disconnection, null)
    intr
  }
  //Multithreading
  protected final def main(f:()=>Unit):()=>Unit = {
    if(shadow.compoThtead != null){throw new CompoException("Error, two main or loop in one component")}
    shadow.compoThtead = new Thread{override def run = {
       try{compo.synchronized{f()}}catch{case e:Exception => {        
         println("Exception in main of component '" + shadow + "': ")
         e.printStackTrace()      
         shadow.runtime.destroy(shadow,4)}}
    }}
    f
  }
  protected final def loop(f:()=>Boolean):()=>Boolean = {
    if(shadow.compoThtead != null){throw new CompoException("Error, two main or loop in one component")}
    shadow.compoThtead = new Thread{override def run = {
      try{  
        var wf = true
        while (wf && shadow.compoFlags.stat == 2){
          compo.synchronized{wf = f()}}}
      catch{
      case e:Exception => {
        println("Exception in loop of component '" + shadow + "': ")
        e.printStackTrace()
        shadow.runtime.destroy(shadow,4)}}
    }}
    f
  }
  protected final def waitloop(f:()=>Boolean):()=>Boolean = {
    if(shadow.compoThtead != null){throw new CompoException("Error, two main or loop in one component")}
    shadow.compoThtead = new Thread{override def run = {
      try{  
        var wf = true
        while (wf && shadow.compoFlags.stat == 2){
          shadow.compoThtead.synchronized{wait()} 
          compo.synchronized{wf = f()}}}
      catch{
      case e:Exception => {
        println("Exception in waitloop of component '" + shadow + "': ")
        e.printStackTrace()
        shadow.runtime.destroy(shadow,4)}}
    }}
    f
  }
  protected final def sleep(t:Long) = {
    if(shadow.compoThtead == null){throw new CompoException("Error on sleep(), no main or loop in this component")}
    Thread.sleep(t)
  }
  protected final def waiting = {
    if(shadow.compoThtead == null){throw new CompoException("Error on waiting, no main or loop in this component")}
    shadow.compoThteadNotify = false
    compo.synchronized{wait()} 
  }
  protected final def waiting(t:Long) = {
    if(shadow.compoThtead == null){throw new CompoException("Error on waiting(), no main or loop in this component")}
    shadow.compoThteadNotify = false
    compo.synchronized{wait(t)}    
    if(! shadow.compoThteadNotify){throw new CompoException("waiting timeout")}
  }
  protected final def run = {
    if(shadow.compoThtead == null){throw new CompoException("Error on run, no main or loop in this component")}
    shadow.compoThteadNotify = true
    compo.synchronized{compo.notify()} 
  }
  protected final def iteration = {
    if(shadow.compoThtead == null){throw new CompoException("Error on run, no main or loop in this component")}
    shadow.compoThtead.synchronized{shadow.compoThtead.notify()} 
  }
  //New dynamic component
  implicit def C2NC (c:Compo):NC = {new NC(c)}
  protected final class NC(c:Compo) {
    def joined(pin:String):Handle = { //Connect to this
      shadow.runtime.newDynamicCompo(c, pin, shadow)
    }   
    def connected(pin:String):CF = {
      new CF(c, pin)
    }
  } 
  protected final class CF(c:Compo, pin:String) {    
    def from(pcn:String):Handle = {
      shadow.runtime.newDynamicCompo(c, pin, shadow.runtime.getShadowForName(pcn))
    }
    def from(pch:Handle):Handle = {
      shadow.runtime.newDynamicCompo(c, pin, pch.asInstanceOf[Shadow])
    }  
  }
  //Destruction
  protected final def selfdestruction = {
    shadow.runtime.destroy(shadow,1)
  }  
  protected final def distroy(cn:String) = {
    shadow.runtime.destroy(shadow.runtime.getShadowForName(cn),3)
  }  
  protected final def distroy(ch:Handle) = {
    shadow.runtime.destroy(ch.asInstanceOf[Shadow],3)
  }  
  //Handles
  protected final def handle:Handle = {
    shadow
  }
  protected final def handle(cn:String):Handle = {
    shadow.runtime.getShadowForName(cn)
  }
  //Interfaces connections/disconnections 
  implicit def S2JD (jin:String):IN = {new IN(jin)} 
  protected final class IN(val jin:String) {
    def join (n:String):JO = { 
      if(shadow.runtime.compoList.static.contains(n)){       
        shadow.runtime.connect(jin, shadow, jin, shadow.runtime.getShadowForName(n))
        null}
      else{
        new JO(jin, n)}
    } 
    def join (pch:Handle) = { 
       shadow.runtime.connect(jin, shadow, jin, pch.asInstanceOf[Shadow])
    } 
//    def change (n:String):CO = { 
//      if(shadow.runtime.compoList.static.contains(n)){
//        shadow.runtime.switch(jin, shadow, jin, shadow.runtime.getShadowForName(n)) 
//        null}
//      else{
//        new CO(jin, n)}
//    } 
//    def change (pch:Handle) = { 
//       shadow.runtime.switch(jin, shadow, jin, pch.asInstanceOf[Shadow])
//    } 
    def disjoin = { 
       shadow.runtime.disconnect(jin, shadow, null, 1)
    } 
    def disjoin (cn:String) = { 
       shadow.runtime.disconnect(jin, shadow.runtime.getShadowForName(cn), null, 1)
    } 
    def disjoin (ch:Handle) = { 
       shadow.runtime.disconnect(jin, ch.asInstanceOf[Shadow], null, 1)
    } 
    def from (cn:String):HIN = { 
      new HIN(jin, shadow.runtime.getShadowForName(cn))
    } 
    def from (ch:Handle):HIN = { 
      new HIN(jin, ch.asInstanceOf[Shadow])
    } 
  }
  protected final class JO(jin:String, pin:String) {
    def from (cn:String) = { 
      shadow.runtime.connect(jin, shadow, pin, shadow.runtime.getShadowForName(cn))
    } 
    def from (ch:Handle) = { 
      shadow.runtime.connect(jin, shadow, pin, ch.asInstanceOf[Shadow])
    } 
  }
//  protected final class CO(jin:String, pin:String) {
//    def from (cn:String) = { 
//      shadow.runtime.switch(jin, shadow, pin, shadow.runtime.getShadowForName(cn))
//    } 
//    def from (ch:Handle) = { 
//      shadow.runtime.switch(jin, shadow, pin, ch.asInstanceOf[Shadow])
//    }      
//  }
  protected final class HIN(jin:String, jch:Shadow) {
    def connect (n:String):COH = { 
      if(shadow.runtime.compoList.static.contains(n)){
        shadow.runtime.connect(jin, jch, jin, shadow.runtime.getShadowForName(n))
        null}
      else{
        new COH(jin, jch, n)}
    } 
    def connect (pch:Handle) = { 
       shadow.runtime.connect(jin, jch, jin, pch.asInstanceOf[Shadow])
    } 
//    def switch (n:String):SOH = { 
//      if(shadow.runtime.compoList.static.contains(n)){
//        shadow.runtime.switch(jin, jch, jin, shadow.runtime.getShadowForName(n))
//        null}
//      else{
//        new SOH(jin, jch, n)}
//    } 
//    def switch (pch:Handle) = { 
//       shadow.runtime.switch(jin, jch, jin, pch.asInstanceOf[Shadow])
//    } 
    def disconnect  = { 
       shadow.runtime.disconnect(jin, jch, null, 1)
    } 
    def disconnect (pcn:String) = { 
       shadow.runtime.disconnect(jin, jch, shadow.runtime.getShadowForName(pcn), 1)
    } 
    def disconnect (pch:Handle) = { 
       shadow.runtime.disconnect(jin, jch, pch.asInstanceOf[Shadow], 1)
    } 
  }
  protected final class COH(jin:String, jch:Shadow, pin:String) {
    def from (cn:String) = { 
      shadow.runtime.connect(jin, jch, pin, shadow.runtime.getShadowForName(cn))
    } 
    def from (ch:Handle) = { 
      shadow.runtime.connect(jin, jch, pin, ch.asInstanceOf[Shadow])
    }      
  }
//  protected final class SOH(jin:String, jch:Shadow, pin:String) {
//    def from (cn:String) = { 
//      shadow.runtime.switch(jin, jch, pin, shadow.runtime.getShadowForName(cn))
//    } 
//    def from (ch:Handle) = { 
//      shadow.runtime.switch(jin, jch, pin, ch.asInstanceOf[Shadow])
//    }      
//  }
}


 

  
  
  
 

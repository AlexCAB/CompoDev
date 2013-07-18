package skidbladnir
import scala.collection.mutable.{Map => MutMap}
  

trait Assembly {
  //Fields 
  private val runtime = new Runtime
  //Components construction
  implicit def C2NB (b:Base):NB = {new NB(b)}
  protected final class NB(val b:Base) {
    def named(n:String) = {
      runtime.newStaticBase(b, n)
    } 
  }
  implicit def C2NC (c:Compo):NC = {new NC(c)}
  protected final class NC(val p:Compo) {
    def connected (pin:(String)) = { 
      new CF(p, pin)
    }
  }
  protected final class CF(c:Compo, pin:(String)) {    
    def from(pcn:String) = {
      new NCC(c, pin, pcn)
    } 
  }  
  protected final class NCC(c:Compo, pin:String, pcn:String) {    
    def named(n:String) = {
      runtime.newStaticCompo(c, n, pin, pcn)
    } 
  }  
  //Components connection
  implicit def S2F (jin:String):FC = {new FC(jin)}
  protected final class FC(val jin:String) {
    def from(jcn:String) = {
      new CC(jin, jcn)
    }
  }
  protected final class CC(jin:String, jcn:String) {
    def connect(n:String):CIC = {
      if(runtime.compoList.static.contains(n)){
        if(! runtime.workMutex.construction){throw new CompoException("You can not connect components after start")}
        runtime.staticConnect(jin, runtime.getShadowForName(jcn), jin, runtime.getShadowForName(n))
        null}
      else{
        new CIC(jin, jcn, n)}
    }
  }
  protected final class CIC(jin:String, jcn:String, pin:String) {  
    def from(pcn:String) = {
      if(! runtime.workMutex.construction){throw new CompoException("You can not connect components after start")}
      runtime.staticConnect(jin, runtime.getShadowForName(jcn), pin, runtime.getShadowForName(pcn))
    } 
  }
  //Dev tools
  protected def visualization = {
    runtime.visualization = new Visualization(runtime)
  }
  protected def console = {
    //Get current pack name
    val cp = getClass().getName().split("\\.").init.mkString(".") 
    //Create console and run assembly
    runtime.console = new Console(runtime)
    runtime.go() //Run assembly
    runtime.console.work(cp) 
  }
  //Start mains / end
  protected def go() = {
    if(runtime.compoList.base.isEmpty){throw new CompoException("No not one component")}
    runtime.go() //Run assembly
  }
  protected def gowait() = {
    if(runtime.compoList.base.isEmpty){throw new CompoException("No not one component")}
    runtime.go() //Run assembly
    runtime.waitMutex.synchronized{  //Wait for end work
      if(runtime.waitMutex.wr){
        runtime.waitMutex.wt = true 
        runtime.waitMutex.wait()}}
  }
  protected def end() = {
    runtime.end()  
    System.exit(0)  
  }
  protected def breakdown() = {
    runtime.end() 
  }
}   
  
  
  
  
  
  
  
  
  

 
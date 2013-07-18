package e_timepiece
import skidbladnir.{Assembly,Base,Compo,Handle,Interface,Mono,Multi}, 
       java.text.SimpleDateFormat,
       applet.AbstractApplet

       
object Test {def main(args: Array[String]): Unit = {val a = new TestAssembly}}


object ITime extends Interface {                        
  class Am extends Multi {val imports = Map[Handle, Pm]()    
    var time:Long = 0}                                  
  class Pm extends Mono {val imports:Am = null
    def tick() = {}}                                   
}
 
class TestAssembly extends Assembly { 
  visualization
  new Main named "main"  
  new Show named "show" 
  new Timepiece named "timepiece"   
  "iTime" from "main" connect "timepiece"              
  "iTime" from "show" connect "timepiece" 
  gowait         
  end
}

class TestApplet extends AbstractApplet { 
  def construct() = {
    new Main named "main"  
    new Show named "show" 
    new Timepiece named "timepiece"   
    "iTime" from "main" connect "timepiece"              
    "iTime" from "show" connect "timepiece" 
  }
}


class Main extends Base { 
  //Interfaces
  protected val iTime = plug(new ITime.Pm{
    override def tick() = {
      println("system: " + imports.time)}}) 
  //Main
  main(()=>{
    sleep(15000)
    selfdestruction 
  })
}

class Show extends Base { 
  //Interfaces
  protected val iTime = plug(new ITime.Pm{
    override def tick() = {
       println("time: " + new SimpleDateFormat("yyyy.MM.dd 'at' HH:mm:ss z").format(imports.time))}}) 
}

class Timepiece extends Base { 
  //Interfaces
  protected val iTime = multijack(new ITime.Am)  
  //Loop
  loop(()=>{ 
    iTime.time = System.currentTimeMillis()     
    iTime.imports.foreach(e => {e._2.tick()})    
    sleep(500)
    true
  })
   
}

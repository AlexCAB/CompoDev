package e_timer
import skidbladnir.{Assembly,Base,Compo,Handle,Interface,Mono,Multi},
       applet.AbstractApplet


object Test {def main(args: Array[String]): Unit = {val a = new TestAssembly}}


object ITimer extends Interface {                      
  class Am extends Mono {val imports:Pm = null}                                  
  class Pm extends Mono {val imports:Am = null
    def tick(tn:Int) = {}}                                       
}

class TestAssembly extends Assembly { 
  visualization
  new Main named "main"  
  new Timer named "timer"  
  "iTime" from "main" connect "timer"             
  gowait           
  end
}

class TestApplet extends AbstractApplet { 
  def construct() = {
    new Main named "main"  
    new Timer named "timer"  
    "iTime" from "main" connect "timer"             
  }
}


class Main extends Base { 
  //Interfaces
  protected val iTime = jack(new ITimer.Pm{      
    override def tick(tn:Int) = {                
      if(tn == 0){println("BOOM"); selfdestruction}else{println("tick No " + tn)}}}) 
}

class Timer extends Base { 
  //Var
  private var tn = 9
  //Interfaces
  protected val iTime = plug(new ITimer.Am)  
  //Loop
  loop(()=>{                                 
    sleep(1000)
    iTime.imports.tick(tn)                   
    tn -= 1
    true                                     
  }) 
}
 
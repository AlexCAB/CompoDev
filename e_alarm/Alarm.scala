package e_alarm

import skidbladnir.{Assembly,Base,Compo,Handle,Interface,Mono,Multi},
       e_frame.{BorderFrame,IFrame,IWidget},
       e_timepiece.{Timepiece,ITime},
       e_watch.{Clock,IClock},
       javax.swing.{JButton},
       java.awt.event.{ActionListener,ActionEvent},
       java.awt.{Dimension},
       java.io.File,
       java.net.URL,
       javax.sound.sampled.{AudioSystem,DataLine,Clip},
       applet.AbstractApplet,
       java.applet.Applet
  
object Test {def main(args: Array[String]): Unit = {val a = new TestAssembly}}


object IButton extends Interface {                         
  class Am extends Mono {val imports:Pm = null
    def setSize(width:Int, height:Int) = {}
    def setText(text:String) = {}}                                
  class Pm extends Mono {val imports:Am = null}                                   
}

object IAlarm extends Interface {                         
  class Am extends Mono {val imports:Pm = null
    def setTime(time:Long):Boolean = {false}}                                
  class Pm extends Mono {val imports:Am = null}                                   
}
 
object IEvent extends Interface {                         
  class Am extends Multi {val imports = Map[Handle, Pm]()}                                
  class Pm extends Mono {val imports:Am = null
    def event() = {}}                                   
}
       
   
class TestAssembly extends Assembly { 
  visualization
  new Main named "main"  
  new Timepiece named "timepiece"   
  new BorderFrame connected "iFrame" from "main" named "frame"   
  new Clock connected "iCenter" from "frame" named "clock"
  "iClock" from "clock" connect "main"
  "iTime" from "timepiece" connect "clock"
  new Button connected "iSouth" from "frame" named "stop"
  "iButton" from "stop" connect "main"
  new Alarm connected "iAlarm" from "main" named "alarm" 
  "iTime" from "timepiece" connect "alarm"
  new Zoomer connected "iZoom" from "alarm" named "zoomer" 
  "iEvent" from "stop" connect "iStop" from "zoomer"
  gowait()                                              
  end                                                            
}

class TestApplet extends AbstractApplet { 
  def construct() ={  
    new Main named "main"  
    new Timepiece named "timepiece"   
    new BorderFrame connected "iFrame" from "main" named "frame"   
    new Clock connected "iCenter" from "frame" named "clock"
    "iClock" from "clock" connect "main"
    "iTime" from "timepiece" connect "clock"
    new Button connected "iSouth" from "frame" named "stop"
    "iButton" from "stop" connect "main"
    new Alarm connected "iAlarm" from "main" named "alarm" 
    "iTime" from "timepiece" connect "alarm"     
    new AppletZoomer connected "iZoom" from "alarm" named "zoomer" 
    "iEvent" from "stop" connect "iStop" from "zoomer"      
  }
}

class Main extends Base { 
  //Interfaces
  protected val iFrame = jack(new IFrame.Pm{   
    override def closing () = {selfdestruction}})
  protected val iClock = jack(new IClock.Pm)  
  protected val iButton = jack(new IButton.Pm) 
  protected val iAlarm = jack(new IAlarm.Pm)  
  //Main  
  main(()=>{
    iClock.imports.setSize(100)
    iFrame.imports.setTitle("Alarm")
    iButton.imports.setSize(10, 25)
    iButton.imports.setText("-= S T O P =-")
    iAlarm.imports.setTime(System.currentTimeMillis() + 15000)   
    iFrame.imports.show()
  })
} 

class Button extends JButton with Compo {
  //Self-assembly
  setPreferredSize(new Dimension(5,5))
  //Interfaces
  protected val iWidget = root(new IWidget.Am{ override val widget:Compo = compo })
  protected val iButton = plug(new IButton.Am{
    override def setSize(width:Int, height:Int) = { compo.asInstanceOf[JButton].setPreferredSize(new Dimension(width, height)); iWidget.imports.pack() }
    override def setText(text:String) = { compo.asInstanceOf[JButton].setText(text) }})
  protected val iEvent = multijack(new IEvent.Am)  
  //Listeners 
  addActionListener(new ActionListener{ override def actionPerformed(e:ActionEvent){iEvent.imports.foreach(e => {e._2.event()})} })   
}

class Zoomer extends Compo {
  //Self-assembly
  private var p:URL = null; var c:Class[_] = this.getClass()  
  while(p == null && c != null){
    p = c.getResource("alarm.wav")
    if(p == null){c = c.getSuperclass()}}
  private val af =  AudioSystem.getAudioFileFormat(p).getFormat()
  private val info = new DataLine.Info(classOf[Clip], af)  
  private val line = AudioSystem.getLine(info).asInstanceOf[Clip]
  private val ais = AudioSystem.getAudioInputStream(p)
  line.open(ais)
  //Interfaces
  protected val iZoom = root(new IEvent.Pm{override def event() = {line.setFramePosition(0); line.loop(10)}})   
  protected val iStop = plug(new IEvent.Pm{override def event() = {line.stop()}})  
  //Deconstructor
  deconstructor((h,a)=>{line.close()})
}

class AppletZoomer extends Compo {                                              
  //Self-assembly
  private var p:URL = null; var c:Class[_] = this.getClass()
  while(p == null && c != null){
    p = c.getResource("alarm.wav")
    if(p == null){c = c.getSuperclass()}}
  private val line = Applet.newAudioClip(p)
  //Interfaces
  protected val iZoom = root(new IEvent.Pm{override def event() = {line.loop()}})   
  protected val iStop = plug(new IEvent.Pm{override def event() = {line.stop()}})  
  //Deconstructor
  deconstructor((h,a)=>{line.stop()})
}

class Alarm extends Compo {
  //Vars
  private var alarm = false
  private var alatmTime:Long = 0
  //Interfaces
  protected val iAlarm = root(new IAlarm.Am{override def setTime(time:Long) = {if(time > iTime.imports.time){alatmTime = time; alarm = false; true}else{false}}})  
  protected val iZoom = multijack(new IEvent.Am) 
  protected val iTime:ITime.Pm = plug(new ITime.Pm{override def tick() = {
    if(! alarm && iTime.imports.time >= alatmTime && alatmTime != 0){
      iZoom.imports.foreach(e => {e._2.event()})
      alarm = true}}})  
}









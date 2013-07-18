package e_tunalarm

import skidbladnir.{Assembly,Base,Compo,Handle,Interface,Mono,Multi},
       e_frame.{BorderFrame,IFrame,IWidget},
       e_timepiece.{Timepiece,ITime},
       e_watch.{Clock,IClock},
       e_alarm.{Button,Zoomer,Alarm,IButton,IAlarm,IEvent},
       javax.swing.{JFrame,JDialog,WindowConstants,JPanel,SpinnerListModel,JSpinner,SpinnerDateModel,SwingUtilities},
       javax.swing.event.{ChangeListener,ChangeEvent},
       java.awt.event.{MouseAdapter,MouseEvent,WindowAdapter,WindowEvent},
       java.awt.{FlowLayout,Dimension,Font,MouseInfo},
       java.util.{Calendar,Date,GregorianCalendar},
       applet.AbstractApplet
       
    
object Test {def main(args: Array[String]): Unit = {val a = new TestAssembly}}


object ITimePicker extends Interface {                         
  class Am extends Mono {val imports:Pm = null}                                
  class Pm extends Mono {val imports:Am = null
     var location = (0,0) 
     def setTime(time:Long) = {}}                                   
}

object IMouseEvent extends Interface {                         
  class Am extends Multi {val imports = Map[Handle, Pm]()}                                
  class Pm extends Mono {val imports:Am = null
    def event(e:MouseEvent) = {}}                                   
}
       
   
class TestAssembly extends Assembly { 
  visualization
  new Main named "main"  
  new Timepiece named "timepiece"   
  new BorderFrame connected "iFrame" from "main" named "frame"   
  new AClock connected "iCenter" from "frame" named "clock"
  "iClock" from "clock" connect "main"
  "iTime" from "timepiece" connect "clock"
  new Button connected "iSouth" from "frame" named "stop"
  "iButton" from "stop" connect "main"
  new Knot connected "iMouseEvent" from "clock" named "knot"    
  new Alarm connected "iAlarm" from "knot" named "alarm" 
  "iTime" from "timepiece" connect "alarm" 
  new Zoomer connected "iZoom" from "alarm" named "zoomer" 
  "iEvent" from "stop" connect "iStop" from "zoomer"
  gowait()                                              
  end                                                            
}

class TestApplet extends AbstractApplet { 
  def construct() = {
    new Main named "main"  
    new Timepiece named "timepiece"   
    new BorderFrame connected "iFrame" from "main" named "frame"   
    new AClock connected "iCenter" from "frame" named "clock"
    "iClock" from "clock" connect "main"
    "iTime" from "timepiece" connect "clock"
    new Button connected "iSouth" from "frame" named "stop"
    "iButton" from "stop" connect "main"
    new Knot connected "iMouseEvent" from "clock" named "knot"    
    new Alarm connected "iAlarm" from "knot" named "alarm" 
    "iTime" from "timepiece" connect "alarm" 
    new Zoomer connected "iZoom" from "alarm" named "zoomer" 
    "iEvent" from "stop" connect "iStop" from "zoomer"
  }
}


class Main extends Base { 
  //Interfaces
  protected val iFrame = jack(new IFrame.Pm{   
    override def closing () = {selfdestruction}})
  protected val iClock = jack(new IClock.Pm)  
  protected val iButton = jack(new IButton.Pm) 
  //Main  
  main(()=>{
    iClock.imports.setSize(100)
    iFrame.imports.setTitle("Alarm")
    iButton.imports.setSize(10, 25)
    iButton.imports.setText("-= S T O P =-")
    iFrame.imports.show()
  })
} 

class AClock extends Clock {               
  //Interfaces
  protected val iMouseEvent = multijack(new IMouseEvent.Am)  
  //Listeners 
  addMouseListener(new MouseAdapter{override def mousePressed(me:MouseEvent){ 
    iMouseEvent.imports.foreach(e =>{e._2.event(me)}) }}) 
}

class Knot extends Compo {
  //Interfaces
  protected val iClockEvent:IMouseEvent.Pm = root(new IMouseEvent.Pm{
    override def event(e:MouseEvent) = {
      if(e.getID() == MouseEvent.MOUSE_PRESSED && iTimePicker.imports == null){
        val p = MouseInfo.getPointerInfo().getLocation()
        iTimePicker.location = (p.x,p.y)                                                 
        new TimePicker joined "iTimePicker"}}})                                                             
  protected val iAlarm = jack(new IAlarm.Pm)  
  protected val iTimePicker = jack(new ITimePicker.Pm{
    override def setTime(time:Long) = { iAlarm.imports.setTime(time) }})
}

class TimePicker extends JDialog(new JFrame()) with Compo{
  //Self-assembly
  setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE)
  setResizable(false)
  setTitle("Time picker")
  private val panel = new JPanel(new FlowLayout(FlowLayout.CENTER))
  panel.setPreferredSize(new Dimension(270,39))
  private val model = new SpinnerDateModel()
  model.setValue(Calendar.getInstance().getTime())
  private val spinner = new JSpinner(model)
  spinner.setPreferredSize(new  Dimension(260, 30))
  spinner.getEditor().asInstanceOf[JSpinner.DefaultEditor].getTextField().setFont(new  Font("Verdana", Font.BOLD, 20))
  spinner.getEditor().asInstanceOf[JSpinner.DateEditor].getFormat().applyPattern("yyyy-MM-dd HH:mm:ss") 
  add(spinner)
  pack()
  //Interfaces
  protected val iTimePicker = root(new ITimePicker.Am)
  //Constructor/deconstructor                                                                          
  constructor((h)=>{
    val l = iTimePicker.imports.location                                                                
    setLocation(l._1, l._2)  
    this.getParent()
    setVisible(true)})    
  deconstructor((h,i)=>{
    setVisible(false)
    iTimePicker.imports.setTime(spinner.getValue().asInstanceOf[Date].getTime())})    
  //Listeners   
  addWindowListener(new WindowAdapter{override def windowClosing(e:WindowEvent) = { selfdestruction }})              
}


 















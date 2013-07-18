package e_watch
import skidbladnir.{Assembly,Base,Compo,Handle,Interface,Mono,Multi},
       e_frame.{BorderFrame,IFrame,IWidget},
       e_timepiece.{Timepiece,ITime},
       javax.swing.{JLabel,SwingConstants}, 
       java.awt.{Dimension,Font,Color,GraphicsEnvironment},
       java.io.File,
       java.net.URL,
       java.text.SimpleDateFormat,
       applet.AbstractApplet
       
    
object Test {def main(args: Array[String]): Unit = {val a = new TestAssembly}}


object IClock extends Interface {                         
  class Am extends Mono {val imports:Pm = null
    def setSize(s:Int) = {}}                                
  class Pm extends Mono {val imports:Am = null
    }                                   
}
       

class TestAssembly extends Assembly { 
  visualization
  new Main named "main"  
  new Timepiece named "timepiece"   
  new BorderFrame connected "iFrame" from "main" named "frame"   
  new Clock connected "iCenter" from "frame" named "clock"
  "iClock" from "clock" connect "main"
  "iTime" from "timepiece" connect "clock"
  gowait()  
  end                                                            
}

class TestApplet extends AbstractApplet { 
  def construct() = {
    new Main named "main"  
    new Timepiece named "timepiece"   
    new BorderFrame connected "iFrame" from "main" named "frame"   
    new Clock connected "iCenter" from "frame" named "clock"
    "iClock" from "clock" connect "main"
    "iTime" from "timepiece" connect "clock"
  }
}

class ManualApplet extends AbstractApplet { 
  def construct() = {
    (new Thread{override def run() = {console; breakdown()}}).start()   
  }
}


class Main extends Base { 
  //Interfaces
  protected val iFrame:IFrame.Pm = jack(new IFrame.Pm{   
    override def closing () = {selfdestruction}},
    connection = (c)=>{iFrame.imports.setTitle("Watch"); iFrame.imports.show()})   
  protected val iClock:IClock.Pm = jack(new IClock.Pm,
    connection = (c)=>{iClock.imports.setSize(100)})  
}

class Clock extends JLabel with Compo {
  //Self-assembly    
  private var f = true; var c:Class[_] = this.getClass()
  while(f && c != null){
    if(c.getResource("DigitaldreamFat.ttf") == null){c = c.getSuperclass()}else{f = false}}
  private val fo = Font.createFont(Font.TRUETYPE_FONT, c.getResourceAsStream("DigitaldreamFat.ttf")).deriveFont(Font.BOLD, 20)
  GraphicsEnvironment.getLocalGraphicsEnvironment().registerFont(fo)
  setFont(fo)
  setText("00:00")
  setPreferredSize(new Dimension(60,20))
  setVerticalAlignment(SwingConstants.CENTER)
  setHorizontalAlignment(SwingConstants.CENTER)
  setForeground(Color.WHITE)
  setBackground(Color.BLACK)
  setOpaque(true)
  private val hourFormat = new SimpleDateFormat("HH")
  private val minFormat = new SimpleDateFormat("mm")
  //Var
  private var semi = true
  //Interfaces
  protected val iWidget = root(new IWidget.Am{override val widget:Compo = compo})
  protected val iClock = plug(new IClock.Am{override def setSize(s:Int) = {     
    setPreferredSize(new Dimension((s * 5), s + (s/2)))
    setFont(fo.deriveFont(Font.BOLD, s))  
    iWidget.imports.pack()}})
  protected val iTime = plug(new ITime.Pm{override def tick() = {
    setText(hourFormat.format(imports.time) + {if(semi){semi = false; ":"}else{semi = true; " "}} + minFormat.format(imports.time))}})  
}











 
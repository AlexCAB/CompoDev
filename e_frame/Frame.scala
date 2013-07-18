package e_frame
import skidbladnir.{Assembly,Base,Compo,Handle,Interface,Mono,Multi},
       javax.swing.{JFrame,JPanel,WindowConstants,JLabel}, 
       java.awt.{BorderLayout,Component,Dimension},
       java.awt.event.{WindowAdapter,WindowEvent},
       applet.AbstractApplet
       
    
object Test {def main(args: Array[String]): Unit = {val a = new TestAssembly}}
       
       
object IFrame extends Interface {                         
  class Am extends Mono {val imports:Pm = null    
    def setTitle(s:String) = {}
    def show() = {}
    def hide() = {}}                                
  class Pm extends Mono {val imports:Am = null
    def closing () = {}}                                   
}

object IWidget extends Interface {                        
  class Am extends Mono {val imports:Pm = null    
    val widget:Compo = null}                              
  class Pm extends Multi {val imports = Map[Handle, Am]()  
    def pack () = {}}                                   
}


object IWindow extends Interface {                        
  class Am extends Multi {val imports = Map[Handle, Pm]()  
    def getSize():(Int,Int) = {(0,0)}
    def getLocation():(Int,Int) = {(0,0)}}                                   
  class Pm extends Mono {val imports:Am = null}                              
}


class TestAssembly extends Assembly { 
  visualization
  new Main named "main"  
  new BorderFrame connected "iFrame" from "main" named "frame"   
  new Hello connected "iCenter" from "frame" named "hello"
  gowait()                                              
  end                                                               
}

class TestApplet extends AbstractApplet { 
  def construct() ={
    new Main named "main"  
    new BorderFrame connected "iFrame" from "main" named "frame" 
    new Hello connected "iCenter" from "frame" named "hello"
  }
}
  

class Main extends Base { 
  //Interfaces
  protected val iFrame:IFrame.Pm = jack(new IFrame.Pm{   
    override def closing () = {selfdestruction}})
  //Main  
  main(()=>{
    iFrame.imports.setTitle("GUI Hello")
    iFrame.imports.show()
  })
}

class BorderFrame extends JFrame with Compo { 
  //Self-assembly
  setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
  private val panel = new JPanel
  panel setLayout new BorderLayout
  add(panel)
  val frame = this
  //Interfaces
  protected val iFrame = root(new IFrame.Am{
    override def setTitle(s:String) = {frame.setTitle(s)}
    override def show() = {frame.setVisible(true)}
    override def hide() = {frame.setVisible(false)}})
  protected val iNorth:IWidget.Pm = multijack(new IWidget.Pm{
    override def pack () = {frame.pack()}},
    connection = (h)=>{addWidget(iNorth.imports(h), BorderLayout.NORTH)},       
    disconnection = (h,i)=>{delWidget(iNorth.imports(h))})                      
  protected val iSouth:IWidget.Pm = multijack(new IWidget.Pm{
    override def pack () = {frame.pack()}},                                     
    connection = (h)=>{addWidget(iSouth.imports(h), BorderLayout.SOUTH)},       
    disconnection = (h,i)=>{delWidget(iSouth.imports(h))})                      
  protected val iEast:IWidget.Pm = multijack(new IWidget.Pm{
    override def pack () = {frame.pack()}},
    connection = (h)=>{addWidget(iEast.imports(h), BorderLayout.EAST)},       
    disconnection = (h,i)=>{delWidget(iEast.imports(h))})                      
  protected val iWest:IWidget.Pm = multijack(new IWidget.Pm{
    override def pack () = {frame.pack()}},
    connection = (h)=>{addWidget(iWest.imports(h), BorderLayout.WEST)},     
    disconnection = (h,i)=>{delWidget(iWest.imports(h))})                       
  protected val iCenter:IWidget.Pm = multijack(new IWidget.Pm{
    override def pack () = {frame.pack()}},
    connection = (h)=>{addWidget(iCenter.imports(h), BorderLayout.CENTER)},      
    disconnection = (h,i)=>{delWidget(iCenter.imports(h))})  
  protected val iWindow:IWindow.Am = multijack(new IWindow.Am{
    override def getSize():(Int,Int) = {val d = frame.getSize(); (d.width, d.height)}
    override def getLocation():(Int,Int) = {val p = frame.getLocation(); (p.x, p.y)}})  
  //Deconstructor
  deconstructor((h,i)=>{setVisible(false)})                                     
  //Listeners   
  addWindowListener(new WindowAdapter{ 
    override def windowClosing(e:WindowEvent) = {iFrame.imports.closing()}
  }) 
  //Functions
  private def addWidget(i:IWidget.Am, p:String) = {
    panel.add(i.widget.asInstanceOf[Component], p)
    pack()
  }
  private def delWidget(i:IWidget.Am) = {
    panel.remove(i.widget.asInstanceOf[Component])
    pack()
  }  
}

class Hello extends JLabel with Compo {
  //Self-assembly
  setText("Hello world!")
  setPreferredSize(new Dimension(200,20))
  //Interfaces
  protected val iWidget = root(new IWidget.Am{override val widget:Compo = compo})
}





























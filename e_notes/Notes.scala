package e_notes
import skidbladnir.{Assembly,Base,Compo,Handle,Interface,Mono,Multi},
       e_frame.{BorderFrame,IFrame,IWidget},
       e_timepiece.{Timepiece,ITime},
       e_alarm.{Alarm,IAlarm,IEvent},
       e_edit.{Editor,IText,OpenSaveText,IOpenSave},
       e_tunalarm.{ITimePicker,TimePicker},
       javax.swing.{JPanel,JButton,JFileChooser,JOptionPane},
       java.awt.{BorderLayout,GridLayout,MouseInfo},
       java.awt.event.{ActionListener,ActionEvent},       
       scala.util.control.Exception,
       applet.AbstractApplet
       
    
object Test {def main(args: Array[String]): Unit = {val a = new TestAssembly}}


object ISaveAsChooser extends Interface {                         
  class Am extends Mono {val imports:Pm = null 
    def choose() = {}}                                
  class Pm extends Mono {val imports:Am = null
    def save(path:String) = {}}                                   
}

object INote extends Interface {                         
  class Am extends Mono {val imports:Pm = null}            
  class Pm extends Multi {val imports = Map[Handle, Am]()
    def newNote() = {}}                                                         
}
       
object INodeBar extends Interface {                         
  class Am extends Mono {val imports:Pm = null}                 
  class Pm extends Mono {val imports:Am = null
    def add() = {}
    def save() = {}
    def remind() = {}}                                 
}


class TestAssembly extends Assembly { 
  visualization
  new Notes named "notes"  
  new Timepiece named "timepiece"   
  gowait()                                              
  end                                                            
}

class TestApplet extends AbstractApplet { 
  def construct() = {
    new Notes named "notes"  
    new Timepiece named "timepiece"     
  }
}


class Notes extends Base { 
  //Interfaces
  protected val iNotes:INote.Pm = multijack(new INote.Pm{ 
    override def newNote() = {
      new Note joined "iNotes"}},
    disconnection = (h,i)=>{
      if(iNotes.imports.size == 1){selfdestruction}})
  //Constructor  
  constructor(()=>{
    new Note joined "iNotes"})
} 

class Note extends Compo{
  //Vars
  var editor:Handle = null
  //Interfaces
  protected val iNote = root(new INote.Am)
  protected val iFrame:IFrame.Pm = jack(new IFrame.Pm{  
    override def closing () = {selfdestruction}})
  protected val iNodeBar = jack(new INodeBar.Pm{
    override def add() = {iNote.imports.newNote()}
    override def save() = {
      if(iSaveAsChooser.imports == null){
        new SaveAsChooser joined "iSaveAsChooser"   
        val opensave = new OpenSaveText joined "iOpenSave"       
        "iText" from opensave connect editor}
      iSaveAsChooser.imports.choose()}
    override def remind() = { 
      if(iAlarm.imports == null){
        val alarm = new Alarm joined "iAlarm"
        "iZoom" join alarm
        "iTime" from alarm connect "timepiece"}
      val p = MouseInfo.getPointerInfo().getLocation()
      iTimePicker.location = (p.x,p.y)
      new TimePicker joined "iTimePicker"}})
  protected val iSaveAsChooser:ISaveAsChooser.Pm = jack(new ISaveAsChooser.Pm{
    override def save(path:String) = {
      iOpenSave.imports.save(path)}})
  protected val iOpenSave:IOpenSave.Pm = jack(new IOpenSave.Pm)
  protected val iAlarm = jack(new IAlarm.Pm)
  protected val iZoom = plug(new IEvent.Pm{
    override def event() = {
      iFrame.imports.show()}})
  protected val iTimePicker = jack(new ITimePicker.Pm{
    override def setTime(time:Long) = {
      if(iAlarm.imports.setTime(time)){
        iFrame.imports.hide()}}})
  //Constructor  
  constructor((h)=>{                                                       
    val frame = new BorderFrame joined "iFrame"
    iFrame.imports.setTitle("Note")
    editor = new Editor connected "iCenter" from frame
    val notebar = new NoteBar connected "iNorth" from frame
    "iNodeBar" join notebar                                                  
    iFrame.imports.show()
  })
}

class NoteBar extends JPanel(new GridLayout(1,3)) with ActionListener with Compo {
  //Self-assembly
  val add = new JButton("Add"); add.addActionListener(this); add(add)
  val save = new JButton("Save"); save.addActionListener(this); add(save)
  val remind = new JButton("Remind"); remind.addActionListener(this); add(remind)  
  //Interfaces
  protected val iWidget = root(new IWidget.Am{override val widget:Compo = compo}) 
  protected val iNodeBar = plug(new INodeBar.Am)
  //Listeners
  def actionPerformed(e:ActionEvent) = { 
    e.getSource() match{
      case `add` => {iNodeBar.imports.add()}
      case `save` => {iNodeBar.imports.save()}
      case `remind` => {iNodeBar.imports.remind()}
      case _ => {}}
  }
}

class SaveAsChooser extends Compo{
  //Self-assembly
  val fc = new JFileChooser() 
  //Interfaces
  protected val iSaveAsChooser:ISaveAsChooser.Am = root(new ISaveAsChooser.Am{
    override def choose() = {
      try{
        if(fc.showSaveDialog(null) == 0){  
          val p = fc.getSelectedFile().getPath()
          iSaveAsChooser.imports.save(p)}}
      catch{case e:Exception =>{
        JOptionPane.showMessageDialog(null, "Can't save" + e, "Error", JOptionPane.ERROR_MESSAGE)}}}}) 
}







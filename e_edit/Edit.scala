package e_edit
import skidbladnir.{Assembly,Base,Compo,Handle,Interface,Mono,Multi},
       e_frame.{BorderFrame,IFrame,IWidget,IWindow},
       javax.swing.{JPanel,JFrame,WindowConstants,JTextPane,JScrollPane,JPopupMenu,JMenuItem,JButton,JFileChooser,JOptionPane},
       javax.swing.event.{DocumentListener,DocumentEvent},
       java.util.Scanner,
       java.awt.{Dimension,Font,BorderLayout,GridLayout},
       java.awt.event.{ActionListener,MouseAdapter,MouseEvent,ActionEvent},
       java.io.{File,PrintWriter},
       applet.AbstractApplet
       
    
object Test {def main(args: Array[String]): Unit = {val a = new TestAssembly}}


object IText extends Interface {                         
  class Am extends Mono {val imports:Pm = null
    def set(text:String) = {}
    def get():String = {""}}                                
  class Pm extends Mono {val imports:Am = null
     def changed() = {}}                                   
}

object IOpenSave extends Interface {                         
  class Am extends Mono {val imports:Pm = null
    def open(path:String) = {}
    def save(path:String) = {}}                                
  class Pm extends Mono {val imports:Am = null}                                   
}

   
class TestAssembly extends Assembly { 
  visualization
  new Main named "main"  
  new BorderFrame connected "iFrame" from "main" named "frame" 
  new Editor connected "iCenter" from "frame" named "editor"  
  new OpenSaveBar connected "iNorth" from "frame" named "bar"
  new OpenSaveText connected "iOpenSave" from "bar" named "opensave"
  "iText" from "opensave" connect "editor"
  gowait()                                              
  end                                                             
}

class TestApplet extends AbstractApplet { 
  def construct() = {
    new Main named "main"  
    new BorderFrame connected "iFrame" from "main" named "frame" 
    new Editor connected "iCenter" from "frame" named "editor"  
    new OpenSaveBar connected "iNorth" from "frame" named "bar"
    new OpenSaveText connected "iOpenSave" from "bar" named "opensave"
    "iText" from "opensave" connect "editor"
  }
}


class Main extends Base { 
  //Interfaces
  protected val iFrame = jack(new IFrame.Pm{   
    override def closing () = {selfdestruction}})
  //Main  
  main(()=>{
    iFrame.imports.setTitle("Text editor")
    iFrame.imports.show()
  })
} 


class Editor extends JPanel(new BorderLayout()) with ActionListener with Compo {
  //Self-assembly
  val pp = new JPopupMenu()
  val pCut = new JMenuItem("Cut"); pCut.addActionListener(this); pp.add(pCut)
  val pCopy = new JMenuItem("Copy"); pCopy.addActionListener(this); pp.add(pCopy)
  val pPast = new JMenuItem("Past"); pPast.addActionListener(this); pp.add(pPast)
  val tp = new JTextPane(); setLayout(new BorderLayout())
  tp.setPreferredSize(new Dimension(400,200))
  tp.setFont(new Font("Bradley Hand ITC", Font.BOLD, 20)) 
  val sp = new JScrollPane(tp)
  add(sp)
  //Interfaces
  protected val iWidget = root(new IWidget.Am{override val widget:Compo = compo})
  protected val iText = jack(new IText.Am{
    override def set(text:String) = {tp.setText(text)}
    override def get():String = {tp.getText()} })
  //Listeners 
  tp.addMouseListener(new MouseAdapter{   
    override def mousePressed(e:MouseEvent) = {maybeShowPopup(e)}
    override def mouseReleased(e:MouseEvent) = {maybeShowPopup(e)}
    def maybeShowPopup(e:MouseEvent) = {if(e.isPopupTrigger()){ pp.show(e.getComponent(),e.getX(), e.getY())}}})
  tp.getDocument().addDocumentListener(new DocumentListener{
    override def changedUpdate(e:DocumentEvent) = {try{iText.imports.changed()}catch{case e:Exception => {e.printStackTrace()}}} 
    override def insertUpdate(e:DocumentEvent) = {try{iText.imports.changed()}catch{case e:Exception => {e.printStackTrace()}}}  
    override def removeUpdate(e:DocumentEvent) = {try{iText.imports.changed()}catch{case e:Exception => {e.printStackTrace()}}}}) 
  def actionPerformed(e:ActionEvent) = {
    e.getSource() match{
      case `pCut` => {tp.cut()}
      case `pCopy` => {tp.copy()}
      case `pPast` => {tp.paste()}
      case _ => {}}
  }
}

class OpenSaveText extends Compo {
  //Interfaces
  protected val iOpenSave:IOpenSave.Am = root(new IOpenSave.Am{
    override def open(path:String) = {
      iText.imports.set(new Scanner( new File(path), "UTF-8" ).useDelimiter("\\A").next())}
    override def save(path:String) = {
      val o = new PrintWriter(path)
      try{o.println( iText.imports.get)}finally{o.close()}}})
  protected val iText = plug(new IText.Pm)
}

class OpenSaveBar extends JPanel(new GridLayout(1,3)) with ActionListener with Compo {
  //Self-assembly
  val open = new JButton("Open"); open.addActionListener(this); add(open)
  val save = new JButton("Save"); save.addActionListener(this); add(save)
  val saveAs = new JButton("Save as..."); saveAs.addActionListener(this); add(saveAs)
  val fc = new JFileChooser() 
  //Interfaces
  protected val iWidget = root(new IWidget.Am{override val widget:Compo = compo}) 
  protected val iOpenSave = jack(new IOpenSave.Pm)
  //Listeners
  def actionPerformed(e:ActionEvent) = {
    e.getSource() match{
      case `open` => {
        if(fc.showOpenDialog(this) == JFileChooser.APPROVE_OPTION){
          try{
            iOpenSave.imports.open(fc.getSelectedFile().getPath())}
          catch{case e:Exception =>{
            JOptionPane.showMessageDialog(this, "Can't open", "Error", JOptionPane.ERROR_MESSAGE)}}}}
      case `save` => {
        val f = fc.getSelectedFile()
        if(f == null){
          fc.showSaveDialog(this)
          try{
            iOpenSave.imports.save(fc.getSelectedFile().getPath())}
          catch{case e:Exception =>{
            JOptionPane.showMessageDialog(this, "Can't save", "Error", JOptionPane.ERROR_MESSAGE)}}}
        else{
          try{
            iOpenSave.imports.save(fc.getSelectedFile().getPath())}
          catch{case e:Exception =>{
            JOptionPane.showMessageDialog(this, "Can't save", "Error", JOptionPane.ERROR_MESSAGE)}}}}
      case `saveAs` => {
        fc.showSaveDialog(this)
          try{
            iOpenSave.imports.save(fc.getSelectedFile().getPath())}
          catch{case e:Exception =>{
            JOptionPane.showMessageDialog(this, "Can't save", "Error", JOptionPane.ERROR_MESSAGE)}}}
      case _ => {}}
  }
}





































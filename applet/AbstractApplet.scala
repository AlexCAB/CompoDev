package applet
import skidbladnir.{Assembly,Base,Compo,Handle,Interface,Mono,Multi},
       javax.swing.{JButton}, 
       java.awt.{BorderLayout},
       java.awt.event.{ActionListener,ActionEvent},
       java.applet.Applet

abstract class AbstractApplet extends Applet with Assembly with ActionListener { 
  //Vars
  val strstp = new JButton()
  var launch = false
  //Methods 
  override def init() = {
    super.init()
    strstp.addActionListener(this)
    strstp.setText("Launch")
    setLayout(new BorderLayout())
    add(strstp,BorderLayout.CENTER)
  }
  override def destroy(): Unit = {
    breakdown() 
  }
  //Listeners
  def actionPerformed(e:ActionEvent) = {
    if(e.getSource() == `strstp`){
      if(! launch){
        launch = true
        strstp.setText("Terminate")
        visualization
        construct()
        go()}
      else{
        strstp.setText("...")
        strstp.setEnabled(false)
        breakdown()}}
  }
  //Methods
  def construct()
}
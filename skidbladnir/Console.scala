package skidbladnir
import javax.swing.{JFrame, WindowConstants},
       bsh.util.{GUIConsoleInterface,JConsole},
       java.io.{BufferedReader,IOException},
       java.awt.Color,
       java.awt.event.{WindowAdapter, WindowEvent, KeyEvent},
       scala.util.control.Exception,
       com.sun.awt.AWTUtilities
       
       
class Console(val runtime:Runtime) extends JFrame {
  //Vals
  val jconsole = new JConsole 
  val newline = System.getProperty("line.separator");
  var currentPack = ""; val prompt = ":>"; val promptColor = Color.BLUE
  var workFlag = true
  //Initialization 
  setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)  
  setTitle("Components console")
  getContentPane().add(jconsole);
  setSize(700, 350);
  Exception.ignoring(classOf[Exception]){AWTUtilities.setWindowOpacity(this, 0.8f)}
  setVisible(true)
  //Methods
  def work(cp:String) = {
    currentPack = cp
    val input = jconsole.getIn()
    val bufInput = new BufferedReader(input)
    //Display prompt
    jconsole.print("Type 'help' for help. Current pack '" + currentPack + "'\n", Color.GRAY)
    jconsole.print(currentPack + prompt, promptColor)
    //Command loop
    val line = new StringBuilder
    while(workFlag){
      //Get line
      var cr = ' '; line.clear
      while(cr != '\n'){
        val crl = for(i <- 0 to 5)yield{input.read().toChar}
        val hcr = crl.drop(2).mkString("") 
        cr = Integer.parseInt( hcr, 16 ).toChar
        if(cr != '\n'){line += cr}}
      //Parse line  
      val comand = line.toString.replaceAll("\\s+", " ").replaceAll("^\\s+|\\s+$", "")
      if(! comand.isEmpty){
        val words = comand.split(' '); var incorect = false
        words(0) match{
          case "help" => {jconsole.print(helpText + '\n', Color.GREEN)}
          case "view" => {view()}
          case "pack" => {if(words.size == 2){currentPack = words(1)}else{incorect = true}}          
          case "new" => {
            if(words.size >= 4){words.size match{
              case 4 =>{
                words(2) match{
                  case "named" =>{newBase(words(1), words(3))}
                  case "connected" =>{newDynamic(words(1), null, words(3))}
                  case _ => {incorect = true}}}
              case 6 =>{
                words(4) match{
                  case "named" =>{newStatic(words(1), words(5), null, words(3))}
                  case "from" =>{newDynamic(words(1), words(3), words(5))}
                  case _ => {incorect = true}}}
              case 8 =>{newStatic(words(1), words(7), words(3), words(5))}
              case _ => {incorect = true}}}
            else{incorect = true}}        
          case "destroy" => {if(words.size == 2){destroy(words(1))}else{incorect = true}}  
          case "exit" => {workFlag = false}  
          case ";" => {} 
          case _ => {
            if(words.size > 1){words(1) match{
              case "from" => {
                if(words.size > 3){words(3) match{
                  case "connect" => {
                    words.size match{ 
                      case 5 =>{connect(words(0), words(2), words(0), words(4))}
                      case 7 =>{connect(words(0), words(2), words(4), words(6))}
                      case _ =>{incorect = true}}}
                  case "disconnect" => {            
                    words.size match{ 
                      case 4 =>{disconnect(words(0), words(2), null)}
                      case 5 =>{disconnect(words(0), words(2), words(4))}
                      case _ =>{incorect = true}}}
                  case "switch" => {       
                    words.size match{ 
                      case 5 =>{switch(words(0), words(2), words(0), words(4))}
                      case 7 =>{switch(words(0), words(2), words(4), words(6))}
                      case _ =>{incorect = true}}}
                    case _ => {incorect = true}}}
                  else{incorect = true}}
                case _ => {incorect = true}}}
            else{incorect = true}}}
        if(incorect){jconsole.print("Incorrect command\n", Color.RED)}}
      //Display prompt
      if(workFlag){jconsole.print(currentPack + prompt, promptColor)}}
  }
  //Listeners
  val thisFrame = this
  addWindowListener(new WindowAdapter{ 
    override def windowClosing(e:WindowEvent) = {
      workFlag = false
      jconsole.keyPressed(new KeyEvent(thisFrame, KeyEvent.KEY_PRESSED, 0, 0, '\n', '\n'))}
  }) 
  //Methods
  def end() = {
     workFlag = false
     jconsole.keyPressed(new KeyEvent(thisFrame, KeyEvent.KEY_PRESSED, 0, 0, '\n', '\n'))
     setVisible(false)
  }
  //Functions
  private def newBase(pn:String, n:String) = {
    try{
      val p = getClass.getClassLoader().loadClass(currentPack + "." + pn).newInstance().asInstanceOf[Base]
      runtime.createStaticBase(p, n)}
    catch{
      case e:Exception => {
        jconsole.print("Create failed, exception:" + e + "\n", Color.RED)}}
  }
  private def newStatic(pn:String, n:String, pin:String, pcn:String) = { //pin can be null
    try{
      val p = getClass.getClassLoader().loadClass(currentPack + "." + pn).newInstance().asInstanceOf[Compo]
      runtime.createStaticCompo(p, n, getIntrName(p, pin), pcn)}
    catch{
      case e:Exception => {
        jconsole.print("Create failed, exception:" + e + "\n", Color.RED)}}
  }
  private def newDynamic(pn:String, pin:String, pcn:String) = {//pin can be null
    try{
      val p = getClass.getClassLoader().loadClass(currentPack + "." + pn).newInstance().asInstanceOf[Compo]
      runtime.newDynamicCompo(p, getIntrName(p, pin), getShadow(pcn:String))}
    catch{
      case e:Exception => {
        jconsole.print("Create failed, exception:" + e + "\n", Color.RED)}}
  }
  private def destroy(n:String) = {
    val sh = getShadow(n)
    if(sh != null){runtime.destrCompo(sh,3)} 
  } 
  private def connect(jin:String, jcn:String, pin:String, pcn:String) = {
    try{
      val jch = getShadow(jcn); val pch = getShadow(pcn) 
      if(jch != null && pch != null){runtime.connect(jin, jch, pin, pch)}}
    catch{ case e:Exception =>{
      jconsole.print("Exception " + e + "\n", Color.RED)}} 
  }              
  private def disconnect(jin:String, jcn:String, pcn:String) = {
    try{
      val jch = getShadow(jcn); val pch = if(pcn != null){getShadow(pcn)}else{null}
      if(jch != null && (pch != null || pcn == null)){runtime.disconnect(jin, jch, pch, 1)}}
    catch{ case e:Exception =>{
      jconsole.print("Exception " + e + "\n", Color.RED)}} 
  }              
  private def switch(jin:String, jcn:String, pin:String, pcn:String) = {
//    try{
//      val jch = getShadow(jcn); val pch = getShadow(pcn) 
//      if(jch != null && pch != null){runtime.switch(jin, jch, pin, pch)}}
//    catch{ case e:Exception =>{
//      jconsole.print("Exception " + e + "\n", Color.RED)}} 
  }              
  private def getShadow(n:String):Shadow = {
    val c = if(n.charAt(0) == '@'){
      if(runtime.compoList.dynamic.contains(n)){runtime.compoList.dynamic(n)}else{null}}
    else{
      if(runtime.compoList.static.contains(n)){runtime.compoList.static(n)}else{null}}
    if(c == null){jconsole.print("Component '" + n + "' not found\n", Color.RED)}
    c
  }     
  private def view() = {
    if(runtime.visualization == null){
      runtime.visualization = new Visualization(runtime)}
    else{
      runtime.visualization.setVisible(true) 
      runtime.visualization.toFront()}
  }
  private def getIntrName(p:Compo, in:String) ={
    if(in != null){
      in}
    else{
      val fl = p.getClass().getDeclaredField("skidbladnir$Prototype$$shadow")
      fl.setAccessible(true)
      val sh = fl.get(p).asInstanceOf[CompoShadow]
      sh.rootIntrface.name}
  }  
  //Help
  private val helpText = 
    """|Commands:
       |  'help' 
       |  'view' 
       |  'pack <pack>'                                                      
       |  'new <prototype> named <name>' 
       |  'new <prototype> connected <name/@ID>'                              
       |  'new <prototype> connected <interface> from <name/@ID>' 
       |  'new <prototype> connected <name/@ID> named <name>' 
       |  'new <prototype> connected <interface> from <name/@ID> named <name>' 
       |  'destroy <name/@ID>' 
       |  '<interface> from <name/@ID> connect <name/@ID>' 
       |  '<interface> from <name/@ID> connect <interface> from <name/@ID>' 
       |  '<interface> from <name/@ID> disconnect' 
       |  '<interface> from <name/@ID> disconnect <neme/@ID>' 
       |  '<interface> from <name/@ID> switch <name/@ID>' 
       |  '<interface> from <name/@ID> switch <interface> from <name/@ID>'  
       |  'exit' 
       |Description:       
       |  view - open visualization window, 
       |  pack - set the java-class package for command 'new',
       |  new - create component: 
       |    prototype - component class,
       |    name - name for base or static component (omitted for dynamic component),
       |    interface and name/@ID - interface from component where will connect 
       |      root interface from new(if have same name as root, may be omitted),
       |  destroy - destroy component,
       |  connect - connect interface from name/@ID component to interface (if 
       |    both have same name, may be omitted) from name/@ID component,
       |  disconnect - disconnect interface from name/@ID of neme/@ID component ( if
       |    omit for multiinterface then will disconnect all),
       |  switch - switch interface from name/@ID to interface from name/@ID,
       |  exit - exit from programm
       |""".stripMargin
}
 











































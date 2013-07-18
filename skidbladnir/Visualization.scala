package skidbladnir
import javax.swing.{JFrame, WindowConstants},
       scala.collection.mutable.{Map => MutMap},
       scala.util.control.Exception,
       edu.uci.ics.jung.graph.SparseMultigraph,
       edu.uci.ics.jung.algorithms.layout.SpringLayout,     
       edu.uci.ics.jung.visualization.VisualizationViewer,
       edu.uci.ics.jung.visualization.decorators.ToStringLabeller,
       edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse,
       edu.uci.ics.jung.visualization.LayeredIcon,
       edu.uci.ics.jung.visualization.decorators.{PickableVertexPaintTransformer,PickableEdgePaintTransformer,VertexIconShapeTransformer,
         EllipseVertexShapeTransformer,DefaultVertexIconTransformer},
       edu.uci.ics.jung.visualization.decorators.EdgeShape,
       edu.uci.ics.jung.algorithms.layout.util.VisRunner,
       edu.uci.ics.jung.algorithms.util.IterativeContext,
       edu.uci.ics.jung.visualization.decorators.GradientEdgePaintTransformer,
       org.apache.commons.collections15.Transformer,
       java.util.{Map => JMap, HashMap},
       javax.swing.{ImageIcon,Icon,SwingUtilities},
       javax.imageio.ImageIO,
       java.awt.{Dimension,Color,Paint},
       java.awt.event.{WindowAdapter, WindowEvent},
       com.sun.awt.AWTUtilities,
       java.lang.reflect.InvocationTargetException
 
              
class Visualization(val runtime:Runtime) extends JFrame {
  //Variables
  val components = MutMap[Shadow, VComponent]()
  val iconsMap:JMap[Vertex,Icon] = new HashMap[Vertex,Icon]()
  var work = true
  //Load icons
  val iconsNames = List("Base.png","Static.png","Dynamic.png","Jack.png","Plug.png","MultiJack.png","MultiPlug.png","Root.png","Connect.png","RootConnect.png")
  val iconsList = for(e <- iconsNames)yield{ 
    var i:Icon = null
    try{   
      val tx = ImageIO.read(getClass().getResource(e))
       i = new LayeredIcon(new ImageIcon(tx).getImage())}
    catch{
      case e:Exception => {println("Can't load icon")}}
    i} 
  //Create graph
  val graph = new SparseMultigraph[Vertex, Edge]()
  //Create layout    
  val graphLayout = new SpringLayout(graph)
  graphLayout.setSize(new Dimension(630,330))  
  //Create viewer
  val viewer = new VisualizationViewer[Vertex, Edge](graphLayout)
  val vpf =  new PickableVertexPaintTransformer[Vertex](viewer.getPickedVertexState(), Color.white, Color.yellow);  
  viewer.getRenderContext().setVertexFillPaintTransformer(vpf);
  viewer.getRenderContext().setEdgeDrawPaintTransformer(new PickableEdgePaintTransformer[Edge](viewer.getPickedEdgeState(), Color.black, Color.cyan));
  viewer.setBackground(Color.white);
  viewer.getRenderContext().setVertexLabelTransformer(new ToStringLabeller())
  val vertexImageShapeFunction = new VertexIconShapeTransformer[Vertex](new EllipseVertexShapeTransformer[Vertex]());
  val vertexIconFunction = new DefaultVertexIconTransformer[Vertex]();
  vertexImageShapeFunction.setIconMap(iconsMap);
  vertexIconFunction.setIconMap(iconsMap);
//  viewer.getRenderContext().setEdgeShapeTransformer(new EdgeShape.Line[Vertex, Edge]())
  viewer.getRenderContext().setVertexShapeTransformer(vertexImageShapeFunction);
  viewer.getRenderContext().setVertexIconTransformer(vertexIconFunction); 
  viewer.setPreferredSize(new Dimension(700,400))
  val graphMouse = new DefaultModalGraphMouse[Vertex, Edge]() //picking Editing
  viewer.setGraphMouse(graphMouse)
  viewer.addKeyListener(graphMouse.getModeKeyListener())
  val edgeDrawPaint = new GradientPickedEdgePaintFunction[Vertex, Edge]( new PickableEdgePaintTransformer[Edge](viewer.getPickedEdgeState(),Color.BLACK, Color.CYAN), viewer)
  viewer.getRenderContext().setEdgeDrawPaintTransformer( edgeDrawPaint );
  //Clear loop
  val clearLoop:Thread = new Thread{
    override def run() = {
      while (work){
        clearLoop.synchronized{clearLoop.wait()}
        if(work){
          Thread.sleep(5000)
          val r = new Runnable() {def run() = {
            try{
              viewer.getRenderContext().getPickedVertexState().clear()
              viewer.getRenderContext().getPickedEdgeState().clear()}
           catch{
             case e:Exception => {println("Warning: error in visualization.clearLoop: " + e); e.printStackTrace()}}}}
          SwingUtilities.invokeAndWait(r)}}
    }
  }; clearLoop.start()
  //Listeners   
  addWindowListener(new WindowAdapter{ 
    override def windowClosing(e:WindowEvent) = {work = false; clearLoop.synchronized{clearLoop.notify()}}
  }) 
  //Add components
  components.synchronized{
    runtime.compoList.static.foreach(e => {addCompoVertex(e._2)})
    runtime.compoList.dynamic.foreach(e => {addCompoVertex(e._2)})
    var cl = Set[(IntrDescr, Shadow, IntrDescr, Shadow)]()
    components.foreach(e => {
      e._1.compoInterfaces.foreach(ie =>{
        ie._2.connections.foreach(ce =>{
          val c = ce._1; val id = ce._2
          cl += ((ie._2, e._1, ce._2.descr, ce._1))})})})
    for(e <- cl){if(cl.contains(e._3, e._4, e._1, e._2)){cl -= e}}
    cl.foreach(e =>{connectCompo(e._1.name, e._2, e._3.name, e._4, (e._1.root || e._1.root))})}
  //Construct window
  getContentPane().add(viewer)
  setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE)
  setTitle("Components visualization")
  pack()
  Exception.ignoring(classOf[Exception]){AWTUtilities.setWindowOpacity(this, 0.8f)}
  setVisible(true)
  //Methods
  def end() = {
    setVisible(false)
  }
  def addCompo(sh:Shadow) = {components.synchronized{ 
      addCompoVertex(sh) 
  }}
  def delCompo(sh:Shadow) = {components.synchronized{
    val r = new Runnable() {def run() = {
      try{  
        if(components.contains(sh)){
          val dc = components(sh)
          components -= sh
          dc.interfaces.foreach(e =>{
            graph.removeEdge(e._2.edge)
            graph.removeVertex(e._2)
            iconsMap.remove(e._2)})
          graph.removeVertex(dc)
          iconsMap.remove(dc)}}
      catch{
        case e:Exception => {println("Warning: error in visualization.delCompo: " + e); e.printStackTrace()}}}}
    if(SwingUtilities.isEventDispatchThread()){
      r.run()}
    else{
      SwingUtilities.invokeAndWait(r)}         
  }}
  def connectCompo(jin:String, jch:Shadow, pin:String, pch:Shadow, root:Boolean) = {components.synchronized{
    val r = new Runnable() {def run() = {
      try{
        val n = pin + "-" + jin 
        val fc = components(jch); val sc = components(pch)
        val fi = fc.interfaces(jin); val si = sc.interfaces(pin)
        val feg = new Edge(fi.edge.root); val seg = new Edge(si.edge.root) 
        //Create connection node
        val c = new VConnect(n, feg, seg, fi, fc, si, sc)      
        fi.connections += (pch -> c); si.connections += (jch -> c)      
        //Hide interfaces
        if(fi.mono_multi){graph.removeEdge(fi.edge); graph.removeVertex(fi)}
        if(si.mono_multi){graph.removeEdge(si.edge); graph.removeVertex(si)}
        //Add connection
        viewer.getRenderContext().getPickedVertexState().clear();
        viewer.getRenderContext().getPickedEdgeState().clear();
        clearLoop.synchronized{clearLoop.notify()}
        iconsMap.put(c, iconsList(if(root){9}else{8}))
        graph.addVertex(c)
        viewer.getRenderContext().getPickedVertexState().pick(c, true)
        graph.addEdge(feg, c, {if(fi.mono_multi){fc}else{fi}})
        viewer.getRenderContext().getPickedEdgeState().pick(feg, true)
        graph.addEdge(seg, c, {if(si.mono_multi){sc}else{si}})
        viewer.getRenderContext().getPickedEdgeState().pick(seg, true)}
      catch{
        case e:Exception => {println("Warning: error in visualization.connectCompo: " + e); e.printStackTrace()}}}}
    if(SwingUtilities.isEventDispatchThread()){
      r.run()}
    else{
        SwingUtilities.invokeAndWait(r)}  
  }}
  def disconnectCompo(jin:String, jch:Shadow, pch:Shadow) = {components.synchronized{
    val r = new Runnable() {def run() = {
      try{  
        val fc = components(jch)
        val fi = fc.interfaces(jin)
        val c = fi.connections(pch) 
        val (si, sc) = if(c.fCompo == fc){(c.sIntr,c.sCompo)}else{(c.fIntr,c.fCompo)}
        //Delete connection
        graph.removeEdge(c.fEdge)
        graph.removeEdge(c.sEdge)
        graph.removeVertex(c)
        fc.interfaces(fi.name).connections -= pch
        sc.interfaces(si.name).connections -= jch   
        iconsMap.remove(c)
        //Restore interfases
        viewer.getRenderContext().getPickedVertexState().clear();
        viewer.getRenderContext().getPickedEdgeState().clear();  
        clearLoop.synchronized{clearLoop.notify()}
        if(fi.mono_multi){
          graph.addVertex(fi)
          viewer.getRenderContext().getPickedVertexState().pick(fi, true)
          graph.addEdge(fi.edge, fi, fc)
          viewer.getRenderContext().getPickedEdgeState().pick(fi.edge, true)}
        if(si.mono_multi){
          graph.addVertex(si)
          viewer.getRenderContext().getPickedVertexState().pick(si, true)
          graph.addEdge(si.edge, si, sc)
          viewer.getRenderContext().getPickedEdgeState().pick(si.edge, true)}}
      catch{
        case e:Exception => {println("Warning: error in visualization.disconnectCompo: " + e); e.printStackTrace()}}}}
    if(SwingUtilities.isEventDispatchThread()){
      r.run()}
    else{
      SwingUtilities.invokeAndWait(r)}  
  }}
  //Functions
  private def addCompoVertex(sh:Shadow) = { 
  val r = new Runnable() {def run() = {
    try{  
      val t = if(sh.isInstanceOf[BaseShadow]){1}else{if(sh.name != null){2}else{3}} //Component type
      //Add to list
      val il = for(e <- sh.compoInterfaces)yield{
        val eg = new Edge(false) 
        val v = new VIntrface(e._2.jack_plug, e._2.mono_multi, e._2.name, e._2.typeName, eg)
        val i = if(e._2.jack_plug){if(e._2.mono_multi){3}else{5}}else{if(e._2.mono_multi){4}else{6}}
        iconsMap.put(v, iconsList(i))
        (e._2.name -> v)}
      val ilr = if(t != 1){//Add root
        val eg = new Edge(true) 
        val ri = sh.asInstanceOf[CompoShadow].rootIntrface
        val v = new VIntrface(false, true, ri.name, ri.typeName, eg) 
        iconsMap.put(v, iconsList(7))
        il.toMap + (ri.name -> v)}
      else{
        il.toMap} 
      val c = new VComponent(t, sh.toString(), ilr)
      iconsMap.put(c, iconsList(t - 1))
      components += (sh -> c)
      //Add do graph
      viewer.getRenderContext().getPickedVertexState().clear();
      viewer.getRenderContext().getPickedEdgeState().clear();
      clearLoop.synchronized{clearLoop.notify()}
      graph.addVertex(c)
      viewer.getRenderContext().getPickedVertexState().pick(c, true);
      ilr.foreach(e => {
        graph.addVertex(e._2)
        viewer.getRenderContext().getPickedVertexState().pick(e._2, true);
        graph.addEdge(e._2.edge, c, e._2)
        viewer.getRenderContext().getPickedEdgeState().pick(e._2.edge, true)})}
    catch{
      case e:Exception => {println("Warning: error in visualization.addCompo: " + e); e.printStackTrace()}}}}  
    if(SwingUtilities.isEventDispatchThread()){
      r.run()}
    else{
      SwingUtilities.invokeAndWait(r)}
  }
}

class Vertex 

class VComponent(val t:Int, val name:String, val interfaces:Map[String, VIntrface]) extends Vertex{ //t - type: 1-base, 2-static component, 3-dynamic component
  override def toString():String = {name}
}

class VIntrface(val jack_plug:Boolean, val mono_multi:Boolean, val name:String,  val typeName:String, val edge:Edge) extends Vertex{ 
  val connections = MutMap[Shadow, VConnect]()
  override def toString():String = {name + ":" + typeName}
}

class VConnect(val name:String, val fEdge:Edge, val sEdge:Edge, val fIntr:VIntrface, val fCompo:VComponent, val sIntr:VIntrface, val sCompo:VComponent) extends Vertex{
  override def toString():String = {name}
}
class VEmpty() extends Vertex{
  override def toString():String = {"empty"}
}

class Edge(val root:Boolean){ 
  override def toString():String = {""}
}

class GradientPickedEdgePaintFunction[V,E](defaultEdgePaintFunction:Transformer[E,Paint], vv:VisualizationViewer[V,E]) extends GradientEdgePaintTransformer[V,E](Color.WHITE, Color.BLACK, vv){
  override  def  transform(e:E):Paint = {
    if(e.asInstanceOf[Edge].root){
      return defaultEdgePaintFunction.transform(e)}
    else{
      return super.transform(e)}
  }         
  override  protected def getColor2(e:E):Color = {
    if(vv.getPickedEdgeState().isPicked(e)){Color.CYAN}else{c2}
  }
}

























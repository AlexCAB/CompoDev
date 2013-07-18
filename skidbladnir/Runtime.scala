package skidbladnir
import scala.collection.mutable.{Map => MutMap} 
      

class CompoException(val s:String) extends Exception (s)

protected class Runtime extends Functions{
  //Fields
  val compoList = new CompoList          
  var initThread:Thread = null
  val workMutex = new WorkMutex 
  val waitMutex = new WaitMutex 
  var startList = List[Shadow]()
  var visualization:Visualization = null
  var console:Console = null
  //Creating static
  def newStaticBase(p:Base, n:String) = {  
    if(! workMutex.construction){throw new CompoException("You can not add named components after start")}
    try{
      createStaticBase(p, n)}
    catch{
      case e:Exception => {
        if(! compoList.static.isEmpty){destroy(compoList.static.head._2, 5)} //Destroy assembly 
        throw e}}    
  }
  def createStaticBase(p:Base, n:String):BaseShadow = {  
    if(compoList.static.contains(n)){throw new CompoException("Double component name '" + n + "'")}
    var sh:BaseShadow = null
    try{
      //Get shadow
      sh = getShadow(p).asInstanceOf[BaseShadow]
      //Set shadow parameters and init
      sh.runtime = this; sh.name = n
      sh.init()
      //Add to lists
      compoList.base += (n -> sh)
      compoList.static += (n -> sh)
      if(visualization != null){visualization.addCompo(sh)}
      //Initialization
      sh.compoFlags.synchronized{
        sh.compoFlags.stat = 1
        sh.constructor.foreach(e =>{e()}) 
        sh.compoFlags.stat = 2}
      //Run component
      if(workMutex.construction){
        startList +:= sh}
      else{
        sh.start()}}
    catch{
      case e:Exception => {
        //Delete from lists
        if(visualization != null && sh != null){visualization.delCompo(sh)}
        if(compoList.static.contains(n)){compoList.static -= n}
        if(compoList.base.contains(n)){compoList.base -= n}
        //Deinit 
        if(sh != null){try{sh.deinit()}catch{case e:Exception => {e.printStackTrace()}}}
        throw e}}
    sh
  }
  def newStaticCompo(p:Compo, n:String, pin:String, pcn:String) = {   
    if(! workMutex.construction){throw new CompoException("You can not add named components after start")}
    try{
      createStaticCompo(p, n, pin, pcn)}  
    catch{
      case e:Exception => {
        if(! compoList.static.isEmpty){destroy(compoList.static.head._2, 5)} //Destroy assembly
        throw e}}                                  
  }
  def createStaticCompo(p:Compo, n:String, pin:String, pcn:String):CompoShadow = {  
    if(compoList.static.contains(n)){throw new CompoException("Double component name '" + n + "'")}
    var sh:CompoShadow = null
    try{
      //Get shadow
      sh = getShadow(p).asInstanceOf[CompoShadow]
      //Set shadow parameters and init
      sh.runtime = this; sh.name = n
      sh.init()
      //Add to lists 
      compoList.static += (n -> sh)
      if(visualization != null){visualization.addCompo(sh)}
      //Initialization
      connect(sh.rootIntrface.name, sh, pin, getShadowForName(pcn))
      //Run component
      if(workMutex.construction){
        startList +:= sh}
      else{
        sh.start()}}
    catch{
      case e:Exception => {
        //Delete from lists
        if(visualization != null && sh != null){visualization.delCompo(sh)}
        if(compoList.static.contains(n)){compoList.static -= n}
        //Deinit 
        if(sh != null){try{sh.deinit()}catch{case e:Exception => {e.printStackTrace()}}}
        throw e}}                              
    sh
  }
  //Creating dynamic
  def newDynamicCompo(c:Compo, pin:String, pch:Shadow):Shadow = {  
    var sh:CompoShadow = null
    try{
      //Get shadow
      sh = getShadow(c).asInstanceOf[CompoShadow]
      //Set runtime and init
      sh.runtime = this 
      sh.init()
      //Add to list 
      compoList.dynamic += (("@"+ sh.proto.toString.split('@').last) -> sh)
      if(visualization != null){visualization.addCompo(sh)}
      //Init and connect    
      connect(sh.rootIntrface.name, sh, pin, pch)        
      //Run component
      if(workMutex.construction){
        startList +:= sh}
      else{
        sh.start()}}
    catch{
      case e:Exception => {
        //Delete from lists
        val n = if(sh != null){("@"+ sh.proto.toString.split('@').last)}else{null}
        if(n != null && compoList.dynamic.contains(n)){compoList.dynamic -= n}
        if(visualization != null && sh != null){visualization.delCompo(sh)}
        //Deinit
        if(sh != null){try{sh.deinit()}catch{case e:Exception => {e.printStackTrace()}}}
        sh = null 
        throw e}}
    sh
  }
  //Static connection
  def staticConnect(jin:String, jch:Shadow, pin:String, pch:Shadow) = { 
    try{
      connect(jin, jch, pin, pch)}
    catch{
      case e:Exception => {
        if(! compoList.static.isEmpty){destroy(compoList.static.head._2, 5)} //Destroy assembly
        throw e}}                              
  }
  //Go/end
  def go() = { 
    if(! workMutex.construction){throw new CompoException("Double go")}
    initThread = Thread.currentThread()
    workMutex.synchronized{ 
      workMutex.construction = false
      startList.foreach(e =>{e.start()})
      startList = List()}
  } 
  def end() = { 
    try{if(! compoList.base.isEmpty){destroy(compoList.base.head._2, 5)}}catch{case e:Exception => {e.printStackTrace()}}   
    if(console != null){console.end}
    if(visualization != null){visualization.end}
  }   
  //Destruction
  def destroy(sh:Shadow, i:Int):Unit = {       
    //Destruction
    if(sh.name == null){  //If dynamic component
      destrCompo(sh,i)}
    else{    
      //Destroy assembly
      var bl:MutMap[String, Shadow] = null
      compoList.base.synchronized{bl = compoList.base.clone}
      bl.foreach(e => {
        destrCompo(e._2, 2)})       
      //End work
      waitMutex.synchronized{ 
        waitMutex.wr = false 
        if(waitMutex.wt){
          waitMutex.notify()}}} 
  }
  def destrCompo(dsh:Shadow, i:Int):Unit = {
    if(dsh.compoFlags.stat == 2){
      var nd = dsh.rootConnecions.filter(e =>{e._1.compoFlags.stat == 2})    
      if(nd.isEmpty){ //If no connected with root - destroy component(this list clear when root disconnected)
        dsh.compoFlags.synchronized{ 
          dsh.compoFlags.stat = 3
          if(dsh.isInstanceOf[BaseShadow]){ //Destroy base
            val bsh = dsh.asInstanceOf[BaseShadow] 
            //Call deconstructor
            bsh.compoFlags.stat = 4
            bsh.deconstructor.foreach(e =>{try{e(i)}catch{case e:Exception => {e.printStackTrace()}}})
            bsh.compoFlags.stat = 5 
            //Disconnect all interface  
            discAllIntr(bsh)
            //Deinitialization
            try{bsh.deinit()}catch{case e:Exception => {e.printStackTrace()}}
            //Delete from lists
            if(visualization != null){visualization.delCompo(bsh)}
            if(compoList.static.contains(bsh.name)){compoList.static -= bsh.name}
            compoList.base.synchronized{if(compoList.base.contains(bsh.name)){compoList.base -= bsh.name}}}
          else{ //Destroy compo
            val csh = dsh.asInstanceOf[CompoShadow] 
            //Call deconstruction and disconnect root   
            try{disconnectRoot(csh.rootIntrface, i)}catch{case e:Exception => {e.printStackTrace()}}
            //Disconnect all interface
            discAllIntr(csh)
            //Deinitialization
            try{csh.deinit()}catch{case e:Exception => {e.printStackTrace()}}
            //Delete from lists
            if(visualization != null){visualization.delCompo(csh)}
            if(csh.name != null){
              if(compoList.static.contains(csh.name)){compoList.static -= csh.name}}
            else{
              val n = "@"+ csh.proto.toString.split('@').last 
              if(compoList.dynamic.contains(n)){compoList.dynamic -= n}}}}}
    else{
      while(! nd.isEmpty){  //Destroy connected with root before
        destrCompo(nd.head._1,2)
        nd = dsh.rootConnecions.filter(e =>{e._1.compoFlags.stat == 2})}
      destrCompo(dsh, 2)}}
  }
  //Connection/switching/disconnection
  def connect(jin:String, jch:Shadow, pin:String, pch:Shadow) = { 
    //Get interfaces descriptions
    val jid = jch.getCompoInterface(jin); val pid = pch.getCompoInterface(pin)
    //Check
    if(jch == pch){throw new CompoException("Can't connect same component '" + jch + "'")}
    if(jid == null){throw new CompoException("Componetn '" + jch + "' don't have interface '" + jin + "'")}
    if(pid == null){throw new CompoException("Componetn '" + pch + "' don't have interface '" + pin + "'")}
    if(jid.typeName != pid.typeName){
      throw new CompoException("Interfase type '" + jid.typeName + "' and '" + pid.typeName + "' at '" + jin + "' of '" + jid.compo + "' and '" + pin + "' of '" + pid.compo + "' is not identical")}
    if(jid.jack_plug == pid.jack_plug){throw new CompoException("Can not connect jack-jack or plug-plug")}
    //Lock components interfaces
    jid.compo.compoFlags.synchronized{jid.compo.compoFlags.operation += 1}
    pid.compo.compoFlags.synchronized{pid.compo.compoFlags.operation += 1}   
    jid.lock.lock()
    while(! pid.lock.tryLock()){
      jid.lock.unlock()
      Thread.`yield`()
      jid.lock.lock()}   
    //Try connect
    try{
      if(jid.mono_multi && jid.connections.size != 0){throw new CompoException("Monointrface '" + jid.name + "' in componetn '" + jid.compo + "' already connect")}
      if(pid.mono_multi && pid.connections.size != 0){throw new CompoException("Monointrface '" + pid.name + "' in componetn '" + pid.compo + "' already connect")}
      if(jid.compo.compoFlags.stat == 5){throw new CompoException("Component '" + jid.compo + "' not exist, state: " +  jid.compo.compoFlags.stat)}
      if(pid.compo.compoFlags.stat == 5){throw new CompoException("Component '" + pid.compo + "' not exist, state: " +  pid.compo.compoFlags.stat)}
      if(jid.connections.contains(pid.compo)){throw new CompoException("Component '" + pid.compo + "' already connect to " +  jid.name + " from " + jid.compo)}
      if(pid.connections.contains(jid.compo)){throw new CompoException("Component '" + jid.compo + "' already connect to " +  pid.name + " from " + pid.compo)}    
      //Connect
      inilIntrfaceLinks(jid, pid)     
      try{inilIntrfaceLinks(pid, jid)}catch{case e:Exception => {try{clearIntrfaceLinks(jid, pid.compo)}catch{case e:Exception => {e.printStackTrace()}}; throw e}}    
      //Call connect methods
      try{
        if(pid.root){ //If root interface- call constructor
          pid.compo.compoFlags.synchronized{
            pid.compo.compoFlags.stat = 1
            pid.compo.asInstanceOf[CompoShadow].constructor.foreach(e =>{e(jid.compo)})
            pid.compo.compoFlags.stat = 2}}
        else{
          pid.connection(jid.compo)}}
      catch{
        case e:Exception => {
          try{clearIntrfaceLinks(pid, jid.compo)}catch{case e:Exception => {e.printStackTrace()}}
          try{clearIntrfaceLinks(jid, pid.compo)}catch{case e:Exception => {e.printStackTrace()}}
          throw e}}       
      try{
        if(jid.root){ //If root interface- call constructor
          jid.compo.compoFlags.synchronized{
            jid.compo.compoFlags.stat = 1
            jid.compo.asInstanceOf[CompoShadow].constructor.foreach(e =>{e(pid.compo)})
            jid.compo.compoFlags.stat = 2}}
        else{
          jid.connection(pid.compo)}}
      catch{
        case e:Exception => {
          if(pid.root){ //If root interface- call deconstructor    
            pid.compo.compoFlags.synchronized{
              pid.compo.compoFlags.stat = 4
              pid.compo.asInstanceOf[CompoShadow].deconstructor.foreach(e =>{try{e(jid.compo, 6)}catch{case e:Exception => {e.printStackTrace()}}})
              pid.compo.compoFlags.stat = 5
              discAllIntr(pid.compo)
              try{pid.compo.deinit()}catch{case e:Exception => {e.printStackTrace()}}}} 
          else{
            try{pid.disconnection(jid.compo, 2)}catch{case e:Exception => {e.printStackTrace()}}} 
          try{clearIntrfaceLinks(pid, jid.compo)}catch{case e:Exception => {e.printStackTrace()}}
          try{clearIntrfaceLinks(jid, pid.compo)}catch{case e:Exception => {e.printStackTrace()}}
          throw e}}
      //Add to visualization     
      if(visualization != null){visualization.connectCompo(jid.name, jid.compo, pid.name, pid.compo,(jid.root || pid.root))}}
    finally{
      jid.lock.unlock()
      pid.lock.unlock()
      jid.compo.compoFlags.synchronized{jid.compo.compoFlags.operation -= 1}
      pid.compo.compoFlags.synchronized{pid.compo.compoFlags.operation -= 1}}
  }
//  def switch(jin:String, jch:Shadow, pin:String, pch:Shadow) = {
////println(Thread.currentThread() + " switch intrfase " + jin + " from " + jch + ", to: " + pin + " from " + pch)  
//    //Get interfaces descriptions
//    val jid = jch.getCompoInterface(jin); val pid = pch.getCompoInterface(pin) 
//    //Check
//    if(jch == pch){throw new CompoException("Can't switch same component '" + jch + "'")}
//    if(jid == null){throw new CompoException("Componetn '" + jch + "' don't have interface '" + jin + "'")}
//    if(pid == null){throw new CompoException("Componetn '" + pch + "' don't have interface '" + pin + "'")}
//    if(jid.typeName != pid.typeName){
//      throw new CompoException("Interfase type '" + jid.typeName + "' and '" + pid.typeName + "' at '" + jin + "' of '" + jid.compo + "' and '" + pin + "' of '" + pid.compo + "' is not identical")}
//    if(jid.jack_plug == pid.jack_plug){throw new CompoException("Can not connect jack-jack or plug-plug")}
//    if(! jid.mono_multi){throw new CompoException("Can't switch multiinterfce '" + jid.name + "' from '" + jid.compo + "'")}
//    if(pid.root){throw new CompoException("Can not switch to root '" + pid.name + "' from '" + pid.compo + "'")}
//    if(jid.root){
//      try{ 
//        var nsh = pid.compo
//        while(! nsh.isInstanceOf[BaseShadow]){
//          nsh = nsh.asInstanceOf[CompoShadow].rootIntrface.connections.head._1}}
//      catch{
//        case e:Exception => {throw new CompoException("Can not confirm absence the loops, exception: " + e)}}}
//    //Lock components interfaces 
//    var f = true; var y = false
//    while(f){
//      jid.lock.lock()
//      try{
//        val did = if(jid.connections.size == 1){jid.connections.head._2.descr}else{null}
//        if(did != null){  
//          if(did.lock.tryLock()){
//            try{
//              if(pid.lock.tryLock()){ 
//                try{
//                  //Check
//                  if(did.root){throw new CompoException("Can not switch from root '" + did.name + "' from '" + did.compo + "'")}
//                  if(pid.mono_multi && pid.connections.size != 0){throw new CompoException("Monointrface '" + pid.name + "' in componetn '" + pid.compo + "' already connect")}
//                  if(pid.compo.compoFlags.stat == 5){throw new CompoException("Component '" + pid.compo + "' not exist, state: " +  pid.compo.compoFlags.stat)}
//                  if(pid.connections.contains(jid.compo)){throw new CompoException("Component '" + pid.compo + "' already connect to " +  jid.name + " from " + jid.compo)}
//                  //Disconnect jack
//                  if(did.connections.contains(jid.compo)){ //If still connect
//                    Exception.ignoring(classOf[Exception]){did.disconnection(jid.compo, 1)}
//                    Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(did, jid.compo)} 
//                    Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(jid, did.compo)}
//                    if(visualization != null){visualization.disconnectCompo(jid.name, jid.compo, did.compo)}}
//                  //Connect to plug
//                  inilIntrfaceLinks(jid, pid)
//                  try{inilIntrfaceLinks(pid, jid)}catch{case e:Exception => {Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(jid, pid.compo)}; throw e}}
//                  try{
//                    jid.switching(did.compo, pid.compo)}
//                  catch{
//                    case e:Exception => {
//                      Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(jid, pid.compo)}
//                      Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(pid, jid.compo)}
//                      throw e}}
//                  try{
//                    pid.connection(jid.compo)} 
//                  catch{
//                    case e:Exception => {
//                      Exception.ignoring(classOf[Exception]){jid.disconnection(pid.compo, 2)}
//                      Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(jid, pid.compo)}
//                      Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(pid, jid.compo)}
//                      throw e}}
//                  if(visualization != null){visualization.connectCompo(jid.name, jid.compo, pid.name, pid.compo,(jid.root || pid.root))}
//                  f = false}
//                finally{
//                  pid.lock.unlock()}}
//              else{
//                y = true}}
//            finally{
//              did.lock.unlock()}}
//          else{
//            y = true}} 
//        else{
//          if(pid.lock.tryLock()){ 
//            try{
//             //Check
//             if(pid.mono_multi && pid.connections.size != 0){throw new CompoException("Monointrface '" + pid.name + "' in componetn '" + pid.compo + "' already connect")}
//             if(pid.compo.compoFlags.stat == 5){throw new CompoException("Component '" + pid.compo + "' not exist, state: " +  pid.compo.compoFlags.stat)}
//             if(pid.connections.contains(jid.compo)){throw new CompoException("Component '" + pid.compo + "' already connect to " +  jid.name + " from " + jid.compo)}
//             //Connect to plug
//             inilIntrfaceLinks(jid, pid)
//             try{inilIntrfaceLinks(pid, jid)}catch{case e:Exception => {Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(jid, pid.compo)}; throw e}}
//             try{
//              jid.connection(pid.compo)}
//             catch{
//               case e:Exception => {
//                 Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(jid, pid.compo)}
//                 Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(pid, jid.compo)}
//                 throw e}}
//             try{
//               pid.connection(jid.compo)} 
//             catch{
//               case e:Exception => {
//                 Exception.ignoring(classOf[Exception]){jid.disconnection(pid.compo, 2)}
//                 Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(jid, pid.compo)}
//                 Exception.ignoring(classOf[Exception]){clearIntrfaceLinks(pid, jid.compo)}
//                 throw e}}
//             if(visualization != null){visualization.connectCompo(jid.name, jid.compo, pid.name, pid.compo,(jid.root || pid.root))}
//             f = false}
//            finally{
//              pid.lock.unlock()}}
//          else{
//            y = true}}}
//    finally{
//      jid.lock.unlock()}
//    if(y){Thread.`yield`(); y = false}}
//  }  
  def disconnect(jin:String, jch:Shadow, pch:Shadow, i:Int) = { //If parameter pch == null -> disconnect all from multi
    val jid = jch.getCompoInterface(jin)
    if(! jid.root){
      //Try to disconnect
      var f = true; var y = false
      while(f){
        jid.lock.lock()
        try{
          val pid = if(pch == null || jid.mono_multi){ //If disconnect all or jack is monointrface 
            val id = jid.connections.find(e => {! e._2.descr.root}); if(! id.isEmpty){id.get._2.descr}else{null}} //Get first not root
          else{
            if(jid.connections.contains(pch)){val id = jid.connections(pch); if(! id.descr.root){id.descr}else{null}}else{null}} //Get if not root   
          if(pid != null){ //If been disconnect other by threads or not connected or root
            if(pid.lock.tryLock()){ //If successfully lock - disconnection
              try{
                if(pid.connections.contains(jid.compo)){ //If still connect
                  try{jid.disconnection(pid.compo, i)}catch{case e:Exception => {e.printStackTrace()}}
                  try{pid.disconnection(jid.compo, i)}catch{case e:Exception => {e.printStackTrace()}}
                  try{clearIntrfaceLinks(jid, pid.compo)}catch{case e:Exception => {e.printStackTrace()}}
                  try{clearIntrfaceLinks(pid, jid.compo)}catch{case e:Exception => {e.printStackTrace()}}
                  if(visualization != null){visualization.disconnectCompo(jid.name, jid.compo, pid.compo)}} 
                if(pch != null){f = false}} //If disconect specific - exit from loop 
              finally{
                pid.lock.unlock()}}
            else{
              y = true}} 
          else{
            f = false}} //If still there is connections and disconnect all - don't exit from loop
      finally{
        jid.lock.unlock()}
      if(y){Thread.`yield`(); y = false}}}
    else{
      destroy(jid.compo, 2)} 
    //If  disconnect all from multi and found root disconnection - destroy compo
    if(pch == null){
      jid.connections.foreach(e => {if(e._2.descr.root){destroy(e._1, 2)}})}
    else{
      try{if(jid.connections(pch).descr.root){destroy(jid.connections(pch).descr.compo, 2)}}catch{case e:Exception => {e.printStackTrace()}}}
  }   
  //Additional methods
  def getShadowForName(cn:String):Shadow = {
    if(! compoList.static.contains(cn)){throw new CompoException("Component with name '" + cn + "' not found")}
    compoList.static(cn)
  }
  //Functions
  private def inilIntrfaceLinks(il:IntrDescr, sil:IntrDescr) = { //Call twice
    val cl = il.compo; val scl = sil.compo
    //Setting link
    val hl = il.halfLink
    val shl = sil.halfLink
    val hlc = getAmOrPmClass(hl.getClass())
    val af = hlc.getDeclaredField("imports")
    af.setAccessible(true)
    if(il.mono_multi){         
      af.set(hl,shl)}
    else{
      val l = af.get(hl).asInstanceOf[Map[Handle, Half]]
      val nl = l + (scl.asInstanceOf[Handle] -> shl)
      af.set(hl,nl)} 
    //Add connections list
    il.connections += (scl -> new Connection(sil, true))
    //Add to root list
    if(sil.root){cl.rootConnecions += (scl -> sil)}    
  }
  private def clearIntrfaceLinks(il:IntrDescr, scl:Shadow) = {  //Call twice
    val sil = il.connections(scl)
    //Delete of connections list
    il.connections -= scl
    //Delete of root list
    if(il.root){scl.rootConnecions -= il.compo}
    //Clear link
    val shl = il.halfLink
    val shlc = getAmOrPmClass(shl.getClass())
    val af = shlc.getDeclaredField("imports")
    af.setAccessible(true)
    if(il.mono_multi){     
      af.set(shl,null)}
    else{
      val l = af.get(shl).asInstanceOf[Map[Handle, Half]]    
      if(l.contains(scl)){
        val nl = l - scl  
        af.set(shl,nl)}}  
  }
  private def disconnectRoot(rid:IntrDescr, i:Int) = {
    var f = true; var y = false; var dc:Shadow = null
    while(f){
      rid.lock.lock()
      try{
        val pid = if(rid.connections.size == 1){rid.connections.head._2}else{null}
        if(pid != null){ //If been disconnect other by threads or not connected 
          if(pid.descr.lock.tryLock()){
            try{
              if(pid.state){  
                pid.state = false
                //Call disconnections and deconstructors
                try{
                  pid.descr.disconnection(rid.compo, 3)
                  rid.compo.compoFlags.synchronized{
                    rid.compo.compoFlags.stat = 4
                    rid.compo.asInstanceOf[CompoShadow].deconstructor.foreach(e =>{try{e(pid.descr.compo, i)}catch{case e:Exception => {e.printStackTrace()}}})   
                    rid.compo.compoFlags.stat = 5}}
                catch{case e:Exception => {e.printStackTrace()}}
                //Clear links
                try{clearIntrfaceLinks(rid, pid.descr.compo)}catch{case e:Exception => {e.printStackTrace()}}
                try{clearIntrfaceLinks(pid.descr, rid.compo)}catch{case e:Exception => {e.printStackTrace()}}
                if(visualization != null){visualization.disconnectCompo(rid.name, rid.compo, pid.descr.compo)}}
              f = false}
            finally{
              pid.descr.lock.unlock()}}
          else{
            y = true}}
        else{
          f = false}}
    finally{
      rid.lock.unlock()}
    if(y){Thread.`yield`(); y = false}}
  }
  private def discAllIntr(sh:Shadow) = {
    var f = true; var y = false; while(f){sh.compoFlags.synchronized{  //Wait for end all connections
    if(sh.compoFlags.operation == 0){
      sh.compoInterfaces.foreach(e => {try{disconnect(e._1, sh, null, 3)}catch{case e:Exception => {e.printStackTrace()}}})
        f = false}
      else{
        y = true}}
      if(y){Thread.`yield`()}}
  }
}


class WorkMutex {  
  var construction = true
}


class WaitMutex {  
  var wr = true
  var wt = false
}

class CompoList {
  val base =  MutMap[String, Shadow]()
  val static = MutMap[String, Shadow]()
  val dynamic = MutMap[String, Shadow]() 
}

























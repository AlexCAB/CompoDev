package skidbladnir


trait Functions {
  def getAmOrPmClass(cl:Class[_]):Class[_] = {
     var hc = cl
      var id = ""
      do{
        val hcn = hc.getName()
        id = hcn.split("\\$").last
        if(!(id == "Am" || id == "Pm")){
          hc = hc.getSuperclass()}
      }while(!(id == "Am" || id == "Pm")) 
    hc
  }
  def getShadow(p:Prototype):Shadow = {
    var sh:Shadow = null; var c:Class[_] = p.getClass()
    while(sh == null && c != null){
      try{
        val fl = c.getDeclaredField("skidbladnir$Prototype$$shadow") 
        fl.setAccessible(true)
        sh = fl.get(p).asInstanceOf[Shadow]}
      catch{case e:java.lang.NoSuchFieldException =>{
       c = c.getSuperclass()}}}
    sh    
  }
}
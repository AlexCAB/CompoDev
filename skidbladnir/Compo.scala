package skidbladnir

trait Compo extends Prototype{
  //Links
  private val shadow = getShadow()
  //Fields
  protected val compo = this
  //Interfaces definition 
  protected final def root[T <: Mono](intr:T, switching:(Handle, Handle)=>Unit = (dc:Handle, cc:Handle)=>Unit):T = {
    shadow.addRootInterface(intr, switching)
    intr
  }
  //Constructor/deconstructor
  protected final def constructor(f:(Handle)=>Unit):(Handle)=>Unit = {
    shadow.constructor :+= f
    f 
  }  
  protected final def deconstructor(f:(Handle,Int)=>Unit):(Handle,Int)=>Unit = {
    shadow.deconstructor :+= f
    f
  }  
  //Functions
  private def getShadow():CompoShadow = {
    var sh:CompoShadow = null; var c:Class[_] = this.getClass()
    while(sh == null && c != null){
      try{
        val fl = c.getDeclaredField("skidbladnir$Prototype$$shadow") 
        fl.setAccessible(true)
        sh = fl.get(this).asInstanceOf[CompoShadow]}
      catch{case e:java.lang.NoSuchFieldException =>{
       c = c.getSuperclass()}}}
    sh    
  }
}
package skidbladnir

trait Base extends Prototype {
  //Links
  private val shadow = getShadow()
  //Fields
  protected val compo = this
  //Constructor/deconstructor
  protected final def constructor(f:()=>Unit):()=>Unit = {
    shadow.constructor :+= f
    f 
  }  
  protected final def deconstructor(f:(Int)=>Unit):(Int)=>Unit = {
    shadow.deconstructor :+= f
    f
  } 
  //Functions
  private def getShadow():BaseShadow = {
    var sh:BaseShadow = null; var c:Class[_] = this.getClass()
    while(sh == null && c != null){
      try{
        val fl = c.getDeclaredField("skidbladnir$Prototype$$shadow") 
        fl.setAccessible(true)
        sh = fl.get(this).asInstanceOf[BaseShadow]}
      catch{case e:java.lang.NoSuchFieldException =>{
       c = c.getSuperclass()}}}
    sh    
  }
}
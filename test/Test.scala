package test
import skidbladnir.{Assembly,Base,Compo,Handle,Interface,Mono,Multi}

object Test {
  def main(args: Array[String]): Unit = {
    val a = new Asm()
  }
}
  
//Intr
object Intr extends Interface {
  class Am extends Mono {val imports:Pm = null
    var i:Int = 0
    def t() = {println("t")}}                                  
  class Pm extends Mono {val imports:Am = null
    def m() = {println("m")}}                                   
}

//MIntr
object MIntr extends Interface {
  class Am extends Mono {val imports:Pm = null
    var i:Int = 0
    def t() = {println("t")}}                                  
  class Pm extends Multi {val imports = Map[Handle, Am]()
    def m() = {println("m")}}                                   
}



//Assembly
class Asm extends Assembly { 
 visualization
  //console
  
  new Comp1 named "C1"
  new Comp4 named "C4"  
 // "I" from "C4" connect "F" from "C1"
//  new Comp2 connected "I" from "C1" named "C2"  
  
//  "J" from "C1" connect "I" from "C2"
  
//  "I" from "C1" connect "C2"
//  "mJ" from "C1" connect "mG" from "C2"
//  "mH" from "C1" connect "mG" from "C2"
  
 // gowait 
  
  
  console
  
  end
  
}

//Component 1
class Comp1 extends Base {
  protected val I = jack(new Intr.Am{
    override def t() = {println("o_t")}},
    connection = (c)=>{println("Comp1_connected_I")},  
    disconnection = (c,i)=>{println("Comp1_desconnected_I " + i)},
    switching = (c,n) => {println("Comp1_switching_I")}) 
    
  protected val J = jack(new Intr.Am{
    override def t() = {println("o_j")}},
    connection = (c)=>{println("Comp1_connected_J")},  
    disconnection = (c,i)=>{println("Comp1_desconnected_J " + i)},
    switching = (c,n) => {println("Comp1_switching_J")}) 
    
  protected val F = jack(new Intr.Am{
    override def t() = {println("o_t")}},
    connection = (c)=>{println("Comp1_connected_F")},  
    disconnection = (c,i)=>{println("Comp1_desconnected_F " + i)},
    switching = (c,n) => {println("Comp1_switching_F")})  
    
  protected val Iroot = jack(new Intr.Am{
    override def t() = {println("o_t")}},
    connection = (c)=>{println("Comp1_connected_Iroot")},  
    disconnection = (c,i)=>{println("Comp1_desconnected_Iroot " + i)},
    switching = (c,n) => {println("Comp1_switching_Iroot")})   
    
    
  protected val mJ = jack(new MIntr.Am,                         
    connection = (c)=>{println("Comp1_connected_mJ")},  
    disconnection = (c,i)=>{println("Comp1_desconnected_mJ " + i)},
    switching = (c,n) => {println("Comp1_switching_mJ")}) 

  protected val mH = jack(new MIntr.Am,                         
    connection = (c)=>{println("Comp1_connected_mH")},  
    disconnection = (c,i)=>{println("Comp1_desconnected_mH " + i)},
    switching = (c,n) => {println("Comp1_switching_mH")}) 
    
    var fc:Handle = null
    var sc:Handle = null
  constructor(()=>{println("Comp1_construct")
     fc = new Comp2 joined "I"
     sc = new Comp2 connected "IC2" from fc
  
  })
  deconstructor((a)=>{println("Comp1_deconstruct " + a)})
   
    
  main(()=>{
    
   
    
  //  "I".join("Iy","C2")
    
  
    
   // I.imports.m
    
    sleep(10000)
    
 //   "Iroot" from sc switch "J" from "C1"
//    
//    
//    "J" join "I" from sc
//     sleep(10000)
//    "J" disjoin
    
  //  sleep(10000)
  //  distroy("C1")
  //  mJ.imports.m
 //   selfdestruction 
    
  //  "I" disjoin
    
//   "Iroot" from sc switch "J" from "C1"
    
   // selfdestruction 
    
  })  
  
  
  
}

//Component 2
class Comp2 extends Compo {
  
  protected val IC2 = jack(new Intr.Am{
    override def t() = {println("o_t")}},
    connection = (c)=>{println("Comp2_connected_IC2")},  
    disconnection = (c,i)=>{println("Comp2_desconnected_IC2 " + i)},
    switching = (c,n) => {println("Comp2_switching_IC2")})   
  
  
  protected val I = plug(new Intr.Pm{
    override def m() = {println("o_m")}},
    connection = (c)=>{println("Comp2_connected_I")/*; throw new Exception("Ops")*/},  
    disconnection = (c,i)=>{println("Comp2_desconnected_I " + i)},
    switching = (c,n) => {println("Comp2_switching_I")})   

  
  protected val mG = multijack(new MIntr.Pm{
    override def m() = {println("o_m")}},
    connection = (c)=>{println("Comp2_connected_mG")/*; throw new Exception("Ops")*/},  
    disconnection = (c,i)=>{println("Comp2_desconnected_mG " + i)})  
    
    
//  constructor(()=>{println("Comp2_construct")})
//  deconstructor((a)=>{println("Comp2_deconstruct " + a)})
    
  
  protected val Iroot = root(new Intr.Pm{
    override def m() = {println("o_m")}},  
    switching = (c,n) => {println("Comp2_switching_Iroot")})  
    
    
  constructor((c)=>{println("Comp2_construct")
    
    
    new Comp3 joined "mG"
    
    
    })
  deconstructor((c,a)=>{println("Comp2_deconstruct " + a)})
  
//     main(()=>{
//       
//       Iroot.imports.t()
//       
//       "Iroot" disjoin
//       
//       distroy("C1")
//     //  I.imports.t
//       
//     })
    
}

class Comp3 extends Compo {
  
  protected val Iroot = root(new MIntr.Am{
    override def t() = {println("o_m")}},  
    switching = (c,n) => {println("Comp3_switching_Iroot")})  
    
  constructor((c)=>{println("Comp3_construct")})
  deconstructor((c,a)=>{println("Comp3_deconstruct " + a)})
  
}

class Comp4 extends Base {
  
  protected val IC2 = jack(new Intr.Am{
    override def t() = {println("o_t")}},
    connection = (c)=>{println("Comp2_connected_IC2")},  
    disconnection = (c,i)=>{println("Comp2_desconnected_IC2 " + i)},
    switching = (c,n) => {println("Comp2_switching_IC2")})   
  
  
  protected val I = plug(new Intr.Pm{
    override def m() = {println("o_m")}},
    connection = (c)=>{println("Comp2_connected_I")/*; throw new Exception("Ops")*/},  
    disconnection = (c,i)=>{println("Comp2_desconnected_I " + i)},
    switching = (c,n) => {println("Comp2_switching_I")})   

  
   constructor(()=>{println("Comp2_construct")})
  deconstructor((a)=>{println("Comp2_deconstruct " + a)})
  
    
}











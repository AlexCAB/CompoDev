package e_hello
import skidbladnir.{Assembly,Base,Compo,Handle,Interface,Mono,Multi},
       applet.AbstractApplet

object Test {def main(args: Array[String]): Unit = {val a = new SimpleAssembly}}

 
class SimpleAssembly extends Assembly { 
  visualization                       
  new Hello named "hello"  
  gowait     
  end
}

class TestApplet extends AbstractApplet { 
  def construct() = {
    new Hello named "hello"  
  }
}


class Hello extends Base { 
  main(()=>{
    println("Hi!")
    sleep(10000)
    selfdestruction   
  }) 
}

 
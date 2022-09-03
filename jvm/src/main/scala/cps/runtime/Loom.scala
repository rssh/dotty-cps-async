package cps.runtime

object Loom {

    val startVirtualThreadMethod: java.lang.reflect.Method | Null = try {
      val threadClass = classOf[java.lang.Thread]
      val method = threadClass.getMethod("startVirtualThread", classOf[Runnable])
      method
    } catch {
      case ex: NoSuchMethodException => null
    }

    def isEnabled() = {
      startVirtualThreadMethod != null
    }

    def startVirtualThread(f:()=>Unit): Unit = {
       if (startVirtualThreadMethod == null) {
         throw new RuntimeException("startVirtualThread is not available on this JVM")
       }
       startVirtualThreadMethod.invoke(
         null,
         new Runnable{
           override def run(): Unit = f()
         }
       )
    }

    

    /*
    def startVirtualThreadJDK19(f:()=>Unit): Unit = {
       Thread.startVirtualThread( new Runnable{
         override def run(): Unit = 
            f()
       } )
    }
    */

    

}
package cps.example

import cps._
import cps.testconfig.given

trait Connection[F[_]:CpsTryMonad]:

  def read():F[Array[Byte]]
  
  def send(reply: Array[Byte]): F[Unit]

  def close(): Unit



trait Handler[F[_]:CpsTryMonad]:

   trait Command:
     def isShutdown: Boolean


   trait Reply:
     def isMuted: Boolean
     def toBytes: Array[Byte]
   

   def openConnection(): F[Connection[F]] = ???
   def readCommand(cn: Connection[F]): F[Command] = ???
   def handle(command:Command): F[Reply] = ???

   def run():F[Unit] = async[F]{
     val connection = await(openConnection())
     try 
       while
         val command = await(readCommand(connection))
         val reply = await(handle(command))
         if (!reply.isMuted)
           await(connection.send(reply.toBytes))
         !command.isShutdown
       do ()
     finally
       connection.close()
   }



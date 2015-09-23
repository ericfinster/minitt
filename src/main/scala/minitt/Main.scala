/**
  * Main.scala - Main Module for MiniTT in Scala
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package minitt

import scala.io.Source._

object Main {

  def main(args: Array[String]) : Unit = {

    if (args.length != 1) {

      println("Usage: opetopictt <filename>")

    } else {

      val lines : String = 
        fromFile(args(0)).mkString

      println("MiniTT in Scala")

      import MiniTTParser._

      val res = parseAll(phrase(expr), lines)
      println(res)

    }

  }

}

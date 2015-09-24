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
      import MiniTTTypeChecker._

      parseAll(phrase(expr), lines) match {
        case Success(e, _) => {

          println("Parsing succesful, now typechecking ...")

          val res = check(RNil, Nil, e, One)
          println("Result: " ++ res.toString)

        }
        case err => println(err.toString)
      }
    }

  }

}

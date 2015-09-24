/**
  * package.scala - Globals for Scala MiniTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package object minitt {

  type Ident = String
  type Name = String
  type CaseTk = ((Int, Int), String)
  type DataTk = ((Int, Int), String)
  type Pos = ((Int, Int), String)
  type SClos = (List[(Name, Expr)], Rho)
  type   Nf  = Val
  type TVal  = Val

}

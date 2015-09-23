/**
  * Syntax.scala - Syntax for Scala MiniTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package minitt

// Expressions
sealed trait Expr
case class ELam(p: Patt, e: Expr) extends Expr
case object ESet extends Expr
case class EPi(p: Patt, e: Expr, t: Expr) extends Expr
case class ESig(p: Patt, e: Expr, t: Expr) extends Expr
case object EOne extends Expr
case object Eunit extends Expr
case class EPair(e: Expr, f: Expr) extends Expr
case class ECon(id: Ident, e: Expr) extends Expr
case class EData(d: DataTk, ss: List[Summand]) extends Expr
case class ECase(c: CaseTk, bs: List[Branch]) extends Expr
case class EFst(e: Expr) extends Expr
case class ESnd(e: Expr) extends Expr
case class EApp(e: Expr, f: Expr) extends Expr
case class EVar(id: Ident) extends Expr
case object EVoid extends Expr
case class EDec(d: Decl, e: Expr) extends Expr
case object EPN extends Expr

// Patterns
sealed trait Patt
case object Punit extends Patt
case class PVar(id: Ident) extends Patt
case class PPair(p: Patt, q: Patt) extends Patt

// Declarations
sealed trait Decl
case class Def(p: Patt, e: Expr, f: Expr) extends Decl
case class Drec(p: Patt, e: Expr, f: Expr) extends Decl

// Summands and Branches
case class Summand(id: Ident, e: Expr) 
case class Branch(id: Ident, e: Expr) 


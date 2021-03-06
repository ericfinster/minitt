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

// Values
sealed trait Val
case class Lam(c: Clos) extends Val
case class Pair(v: Val, w: Val) extends Val
case class Con(n: Name, v: Val) extends Val
case object Unt extends Val
case object Set extends Val
case class Pi(v: Val, c: Clos) extends Val
case class Sig(v: Val, c: Clos) extends Val
case object One extends Val
case class Fun(p: Pos, sc: SClos) extends Val
case class Data(p: Pos, sc: SClos) extends Val
case class Nt(n: Neut) extends Val

// Neutral terms
sealed trait Neut
case class Gen(i: Int, n: Name) extends Neut
case class App(n: Neut, nf: Nf) extends Neut
case class Fst(n: Neut) extends Neut
case class Snd(n: Neut) extends Neut
case class NtFun(p: Pos, sc: SClos, n: Neut) extends Neut

// Function closures
sealed trait Clos
case class Cl(p: Patt, e: Expr, rho: Rho) extends Clos
case class ClCmp(f: Clos, c: Name) extends Clos

// Environment
sealed trait Rho
case object RNil extends Rho
case class UpVar(rho: Rho, p: Patt, v: Val) extends Rho
case class UpDec(rho: Rho, d: Decl) extends Rho



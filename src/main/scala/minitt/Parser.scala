/**
  * Parser.scala - Scala MiniTT Parser
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package minitt

import scala.util.parsing.combinator._
import scala.util.parsing.input._

object MiniTTParser extends RegexParsers with PackratParsers {

  lazy val ident: Parser[String] = 
    """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""".r

  lazy val expr: PackratParser[Expr] = (
      expr1 ~ "," ~ expr ^^
        { case e ~ "," ~ f => EPair(e, f) }
    | expr1
  )

  lazy val expr1: PackratParser[Expr] = (
      "\\" ~ pattern1 ~ "." ~ expr1 ^^
        { case "\\" ~ p ~ "." ~ e => ELam(p, e) }
    | "Pi" ~ pattern1 ~ ":" ~ expr1 ~ "." ~ expr1 ^^
        { case "Pi" ~ p ~ ":" ~ e ~ "." ~ f => EPi(p, e, f) }
    | "Sig" ~ pattern1 ~ ":" ~ expr1 ~ "." ~ expr1 ^^
        { case "Sig" ~ p ~ ":" ~ e ~ "." ~ f => ESig(p, e, f) }
    | positioned(caseTk) ^^
        { case ctk@CaseTk(bs) => ECase(((ctk.pos.line, ctk.pos.column), "fun"), bs) }
    | positioned(dataTk) ^^
        { case dtk@DataTk(ss) => EData(((dtk.pos.line, dtk.pos.column), "Sum"), ss) }
    | decl ~ ";" ~ expr1 ^^ 
        { case d ~ ";" ~ e => EDec(d, e) }
    | expr2 ~ "->" ~ expr1 ^^ 
        { case e ~ "->" ~ f => EPi(Punit, e, f) }
    | expr2 ~ "*" ~ expr1 ^^
        { case e ~ "*" ~ f => ESig(Punit, e, f) }
    | expr2
  )

  case class CaseTk(bs: List[Branch]) extends Positional
  case class DataTk(ss: List[Summand]) extends Positional

  lazy val caseTk: PackratParser[CaseTk] = 
    "fun" ~ "(" ~ repsep(branch, "|") ~ ")" ^^ 
      { case "fun" ~ "(" ~ bs ~ ")" => CaseTk(bs) }

  lazy val dataTk: PackratParser[DataTk] = 
    "Sum" ~ "(" ~ repsep(summand, "|") ~ ")" ^^
      { case "Sum" ~ "(" ~ ss ~ ")" => DataTk(ss) }

  lazy val expr2: PackratParser[Expr] = (
      "$" ~ ident ~ expr3 ^^
        { case "$" ~ id ~ e => ECon(id, e) }
    | expr2 ~ expr3 ^^
        { case e ~ f => EApp(e, f) }
    | expr3
  )

  lazy val expr3: PackratParser[Expr] = (
      "U" ^^^ ESet
    | "1" ^^^ EOne
    | "0" ^^^ Eunit
    | expr3 ~ ".1" ^^ 
        { case e ~ ".1" => EFst(e) }
    | expr3 ~ ".2" ^^ 
        { case e ~ ".2" => ESnd(e) }
    | ident ^^ { EVar(_) }
    | "Void" ^^^ EVoid
    | "PN" ^^^ EPN
    | "(" ~ expr ~ ")" ^^
        { case "(" ~ e ~ ")" => e }
  )

  lazy val branch: PackratParser[Branch] = 
    ident ~ expr ^^ { case id ~ e => Branch(id, e) }

  lazy val summand: PackratParser[Summand] = 
    ident ~ expr ^^ { case id ~ e => Summand(id, e) }

  lazy val pattern: PackratParser[Patt] = (
      pattern1 ~ "," ~ pattern ^^
        { case p ~ "," ~ q => PPair(p, q) }
    | pattern1
  )

  lazy val pattern1: PackratParser[Patt] = (
      "_" ^^^ Punit
    | ident ^^ { PVar(_) }
    | "(" ~ pattern ~ ")" ^^ 
        { case "(" ~ p ~ ")" => p }
  )

  lazy val decl: PackratParser[Decl] = (
      "let" ~ pattern ~ ":" ~ expr ~ "=" ~ expr ^^ 
        { case "let" ~ p ~ ":" ~ e ~ "=" ~ f => Def(p, e, f) }
    | "letrec" ~ pattern ~ ":" ~ expr ~ "=" ~ expr ^^ 
        { case "letrec" ~ p ~ ":" ~ e ~ "=" ~ f => Drec(p, e, f) }
  )

  override protected val whiteSpace = """(\s|--.*)+""".r

}

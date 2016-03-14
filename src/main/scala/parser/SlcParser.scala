package parser

/**
 * Created by sagrava on 2/24/15.
 */


import parser.term._

import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}

class SlcParser  extends JavaTokenParsers with PackratParsers {

  lazy val monosaccharides : PackratParser[String] =
    ("F" | "M" | "N" | "A" | "L" | "S" | "G" | "C" | "G" | "B" | "N" | " U" | "K" | "Q" | "N")


  lazy val targetLinkage : PackratParser[String] =  "2" | "3" | "4" | "6" | "8"
  lazy val anomer        : PackratParser[String] =  "a" | "b"
  lazy val linkerArm     : PackratParser[Any]    = (wholeNumber ~ "pS-") | ("TKNaVL-" | "syLPDM-" | "LVA)TL(N-")
  lazy val parens         : PackratParser[Term]  =  (")" | "(") ^^ {
    case ")" => OpenParenTerm()
    case "(" => CloseParenTerm()
  }
  lazy val expr          : PackratParser[Term] = glycan | parens

  lazy val glycan   : PackratParser[Term] =
      opt(targetLinkage) ~ opt(anomer) ~ monosaccharides  ^^ {
        case Some(_1) ~ Some(_2) ~ _3 => {
          val a = _3
          val b = Some(_2).get
          val c = Some(_1).get
          SingleGlycanTerm(a, (b mkString ""), (c mkString ""))
        }
        case None ~ Some(_1) ~ _2     => {
          val a = _2
          val b = _1
          SingleGlycanTerm(a, (b mkString ""), "" )
        }
        case None ~ None ~ _1         => {
          val a = _1
          SingleGlycanTerm(a, "", "" )
        }
      }


  lazy val branchedGlycan : PackratParser[MultipleGlycanTerm] =
    ")" ~> rep(glycan) <~ "(" ^^ {
      case glycan  => {
        var terms: List[GlycanTerm] = List[GlycanTerm]()
        for (t: Term <- glycan) {
          t match {
            case s: SingleGlycanTerm => {
              terms = SingleGlycanTerm(s.slc, s.anomer, s.link) :: terms
            }
            case m: MultipleGlycanTerm => {
              for (g: GlycanTerm <- m.term) {
                  terms = BranchedGlycanTerm(g.slc, g.anomer, g.link) :: terms
              }

            }
          }
        }
        MultipleGlycanTerm(terms.reverse)
    }
  }

  lazy val structure     : PackratParser[List[Term]] = linkerArm ~> rep(expr)

  def parse(s: String): ParseResult[List[Term]] =
    parseAll(structure, s)


  def apply(s: String): List[Term] =
    parse(s)
    match {
      case Success(structure, _) => structure
      case e: NoSuccess => throw new Exception("syntax error: "+ e.msg + " for " + s)
    }


}

package parser

/**
 * Created by sagrava on 2/24/15.
 */


import parser.term.{Term, SingleGlycanTerm, MultipleGlycanTerm, GlycanTerm}

import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}

class IupacParser  extends JavaTokenParsers with PackratParsers {

  lazy val monosaccharides : PackratParser[String] =
    ("Fuc" | "Man" | "GlcNAc" | "GalNAc" | "Glc" | "Gal" | "Xyl" | "Neu5Ac") ^^ {

      case "Fuc"    => "F"
      case "Man"    => "M"
      case "GlcNAc" => "N"
      case "GalNAc" => "A"
      case "Gal"    => "L"
      case "Neu5Ac" => "S"
      case "Glc"    => "G"
  }

  lazy val targetLinkage : PackratParser[String] =  "2" | "3" | "4" | "6" | "8"
  lazy val sourceLinkage : PackratParser[String] =  "1" | "2"
  lazy val linkage       : PackratParser[String] = ((sourceLinkage ~ "-") ~> targetLinkage)
  lazy val anomer        : PackratParser[String] =  "a" | "b"
  lazy val linkerArm     : PackratParser[Any]    = "-Sp" ~ wholeNumber
  lazy val expr          : PackratParser[Term] = glycan | branchedGlycan

  lazy val glycan   : PackratParser[Term] =
      monosaccharides ~ opt(anomer) ~ opt(linkage)  ^^ {
        case _1 ~ Some(_2) ~ Some(_3) => {
          val a = _1
          val b = Some(_2).get
          val c = Some(_3).get
          //println(s"glycan: glycan: case 1 $a$b$c")
          SingleGlycanTerm(_1, (_2 mkString ""), (_3 mkString ""))
        }
        case _1 ~ Some(_2) ~ None     => {
          val a = _1
          val b = _2
          //println(s"glycan: glycan: case 2 $a$b")
          SingleGlycanTerm(_1, (_2 mkString ""), "" )
        }
        case _1 ~ None ~ None         => {
          val a = _1
          //println(s"glycan: glycan: case 3 $a")
          SingleGlycanTerm(_1, "", "" )
        }
      } | branchedGlycan


  lazy val branchedGlycan : PackratParser[MultipleGlycanTerm] =
    "(" ~> rep(glycan) <~ ")" ^^ {
      case glycan  => {
        var terms: List[GlycanTerm] = List[GlycanTerm]()
        for (t: Term <- glycan) {
          t match {
            case s: SingleGlycanTerm => {

              terms = SingleGlycanTerm(s.slc, s.anomer, s.link) :: terms
            }
            case m: MultipleGlycanTerm => {
              for (g: GlycanTerm <- m.term) {
                terms = SingleGlycanTerm(g.slc, g.anomer, g.link) :: terms
              }

            }
          }
        }
        println("terms: " + terms)
        MultipleGlycanTerm(terms)
    }
  }

  lazy val structure     : PackratParser[List[Term]] = rep(expr)  <~ linkerArm

  def parse(s: String): ParseResult[List[Term]] =
    parseAll(structure, s)


  def apply(s: String): List[Term] =
    parse(s)
    match {
      case Success(structure, _) => structure
      case e: NoSuccess => throw new Exception("syntax error: "+ e.msg)
    }


}

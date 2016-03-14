package parser

/**
 * Created by sagrava on 2/24/15.
 */


import parser.term._

import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}

class ReverseIupacParser  extends JavaTokenParsers with PackratParsers {

  lazy val monosaccharides : PackratParser[String] =
    ("Fuc".reverse | "GlcA".reverse | "Man".reverse
      | "GlcNAc".reverse | "GalNAc".reverse | "Glc".reverse | "Gal".reverse
      | "Xly".reverse | "Neu5Ac".reverse | "Neu5,9Ac".reverse
      | "Neu5Gc".reverse | "Glcol".reverse | "Rha".reverse | "GlcNGc".reverse
      | "MurNAc".reverse | "KDN".reverse | "GlcN[Gc]".reverse) ^^ {

      case "cuF"      => "F"   // Fuc
      case "naM"      => "M"   // Man
      case "cANclG"   => "N"   // GlcNAc
      case "cANlaG"   => "A"   // GalNAc
      case "laG"      => "L"   // Gal
      case "cA5ueN"   => "S"   // Neu5Ac
      case "clG"      => "G"   // Glc
      case "cA9,5ueN" => "S"   // Neu5,9Ac
      case "cG5ueN"   => "C"   // Neu5Gc
      case "loclG"    => "G"   // Glcol
      case "ahR"      => "B"   // Rha
      case "cGNclG"   => "N"   // GlcNGc
      case "cANruM"   => "U"   // MurNAc
      case "NDK"      => "K"   // KDN
      case "AclG"     => "Q"   // GlcA
      case "]cG[NclG" => "N"   // GlcN[Gc]
  }

  lazy val targetLinkage : PackratParser[String] =  "2" | "3" | "4" | "6" | "8"
  lazy val sourceLinkage : PackratParser[String] =  "1" | "2"
  lazy val linkage       : PackratParser[String] = (targetLinkage <~ ("-" ~ sourceLinkage))
  lazy val anomer        : PackratParser[String] =  "a" | "b"
  lazy val linkerArm     : PackratParser[Any]    = (wholeNumber ~ "pS-") | ("TKNaVL-" | "syLPDM-" | "LVA)TL(N-")
//  lazy val expr          : PackratParser[Term] = glycan | branchedGlycan
  lazy val parens         : PackratParser[Term]  =  (")" | "(") ^^ {
    case ")" => OpenParenTerm()
    case "(" => CloseParenTerm()
  }
  lazy val expr          : PackratParser[Term] = glycan | parens

  lazy val glycan   : PackratParser[Term] =
      opt(linkage) ~ opt(anomer) ~ monosaccharides  ^^ {
        case Some(_1) ~ Some(_2) ~ _3 => {
          val a = _3
          val b = Some(_2).get
          val c = Some(_1).get
          //println(s"glycan: glycan: case 1 $a$b$c")
          SingleGlycanTerm(a, (b mkString ""), (c mkString ""))
        }
        case None ~ Some(_1) ~ _2     => {
          val a = _2
          val b = _1
          //println(s"glycan: glycan: case 2 $a$b")
          SingleGlycanTerm(a, (b mkString ""), "" )
        }
        case None ~ None ~ _1         => {
          val a = _1
          //println(s"glycan: glycan: case 3 $a")
          SingleGlycanTerm(a, "", "" )
        }
      }
  //| branchedGlycan


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
//                terms = SingleGlycanTerm(g.slc, g.anomer, g.link) :: terms
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

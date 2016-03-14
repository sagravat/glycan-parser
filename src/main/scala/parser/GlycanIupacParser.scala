package parser

import parser.node._
import parser.term._

import scala.collection.mutable.{Stack}
import util.GlycanInverter

/**
 * Created by sagrava on 2/24/15.
 */
object GlycanIupacParser extends ReverseIupacParser {

  def parseToTree(code: String, name: String = "?"): Glycan = {

    var tree: GlycanNode = null
    var i = 1
    val nodeStack: Stack[GlycanNode] = Stack()
    var inBranch = false

    for (glycanTerm <- this(code.reverse))
      glycanTerm match {

        case SingleGlycanTerm(slc, anomer, link)  => {

          if (inBranch)
            nodeStack.pop()

          inBranch = false

          if (tree == null) {
            tree = GlycanNode(i, slc, anomer, link)()
            nodeStack.push(tree)
          }
          else {
            val lastNode = nodeStack.pop()
            val node = tree.find(lastNode)
            node match {

              case Some(n) => {
                val newNode = GlycanNode(i, slc, anomer, link)(n.id)
                n.add(newNode)
                nodeStack.push(newNode)
              }
              case None => println("node not found")

            }
          }
          i += 1
        }

        case OpenParenTerm() => {
          val lastNode = nodeStack.top
          nodeStack.push(lastNode)

        }

        case CloseParenTerm() => {

          nodeStack.pop()
        }
        case MultipleGlycanTerm(terms)  => {
          inBranch = true
          for (term <- terms) {
            val glycanTerm: GlycanTerm = term
            //println(glycanTerm)
            val lastNode = nodeStack.pop()
            if (lastNode.nodeType == 0)
              nodeStack.push(lastNode)
            val node = tree.find(lastNode)
            node match {

              case Some(n) => {
                val newNode: GlycanNode = GlycanNode(i, glycanTerm.slc, glycanTerm.anomer, glycanTerm.link)(n.id, 1)
                n.add(newNode)
                nodeStack.push(newNode)
              }
              case None  => ???
            }
            i += 1

          }
        }
      }

    Glycan(tree, name)
  }

  def parseCode(code: String): List[GlycanNode] = {

    var tree: GlycanNode = null
    var i = 1
    val nodeStack: Stack[GlycanNode] = Stack()
    var inBranch = false

    for (glycanTerm <- this(code.reverse))
      glycanTerm match {

        case SingleGlycanTerm(slc, anomer, link)  => {

          if (inBranch)
            nodeStack.pop()

          inBranch = false

          if (tree == null) {
            tree = GlycanNode(i, slc, anomer, link)()
            nodeStack.push(tree)
          }
          else {
            val lastNode = nodeStack.pop()
            val node = tree.find(lastNode)
            node match {

              case Some(n) => {
                val newNode = GlycanNode(i, slc, anomer, link)(n.id)
                n.add(newNode)
                nodeStack.push(newNode)
              }
              case None => println("node not found")

            }
          }
          i += 1
        }

        case OpenParenTerm() => {
          val lastNode = nodeStack.top
          nodeStack.push(lastNode)

        }

        case CloseParenTerm() => {

          nodeStack.pop()
        }
        case MultipleGlycanTerm(terms)  => {
          inBranch = true
          for (glycanTerm <- terms) glycanTerm match {
            case SingleGlycanTerm(_,_,_) => {
              val lastNode = nodeStack.pop()
              if (lastNode.nodeType == 0)
                nodeStack.push(lastNode)
              val node = tree.find(lastNode).get
              val newNode: GlycanNode = GlycanNode(i, glycanTerm.slc, glycanTerm.anomer, glycanTerm.link)(node.id, 1)
              node.add(newNode)
              nodeStack.push(newNode)


            }
            case BranchedGlycanTerm(_,_,_) => {
              val lastNode = nodeStack.pop()
              if (lastNode.nodeType == 0 || lastNode.nodeType == 1)
                nodeStack.push(lastNode)
              val node = tree.find(lastNode).get
              val newNode: GlycanNode = GlycanNode(i, glycanTerm.slc, glycanTerm.anomer, glycanTerm.link)(node.id, 1)
              node.add(newNode)
              nodeStack.push(newNode)

            }
            i += 1

          }
        }
      }

    //println("children: " + children)
    //var validSubsets: List[List[GlycanNode]] =  Nil
    var validSubsets: List[GlycanNode] =  Nil
    validSubsets = glycanNodeSubsets(tree, 3)
//      glycanNodeSubsets(tree, 3) :::
//      glycanNodeSubsets(tree, 4) :::
//      glycanNodeSubsets(tree, 5)
//      glycanNodeSubsets(tree, 6)


    validSubsets
  }

  def glycanNodeSubsets(tree: GlycanNode, subsetSize: Int): List[GlycanNode] = {
    var validSubsets: List[GlycanNode] =  Nil
    val children = tree.childList
    var k = 0
    for (nodes <- children.combinations(subsetSize)) {
//      println(k + ", " + nodes)

      k = k + 1
      var isCorrect = true
      var i = 0
      var subtree: GlycanNode = null
      //println(subsetSize + ", " + nodes.size + ", " + nodes)
      for (glycanNode <- nodes) {

        glycanNode match {

          //case g: GlycanNode(slc,anomer,link,nodeType,children) => {
          case g: GlycanNode => {
            val parentId = glycanNode.parentId
            if (parentId != 0 && i > 0 && isCorrect) {
              if (nodes.filter(_.id == parentId).size == 0) {
                isCorrect = false
              }
              else {
                val node =
                  GlycanNode(g.id, g.slc,
                    g.anomer,
                    g.link,
                    List())(parentId, if (g.children.size == 0)  3 else 0 )
                subtree.findParent(node) match {
                  case Some(GlycanNode(_,_,_,_,_)) => {
                    subtree.findParent(node).get.add(node)

                  }
                  case None => {
                    //println("stop!")
                  }
                }
              }
            }
            else if (i == 0 && g.children.size == 0) {
              isCorrect = false
            }
            else {
              subtree = new GlycanNode(g.id,
                g.slc,
                g.anomer,
                g.link,
                List())(parentId, g.nodeType)
            }
            i = i + 1

          }
        }
      }
      if (isCorrect) validSubsets = subtree :: validSubsets
    }
    validSubsets
  }

  def main(args: Array[String]) {

    val g: Glycan =
      GlycanIupacParser.parseToTree("Neu5Aca2-6Galb1-4GlcNAcb1-2-N(LT)AVL", "1")
    println(GlycanInverter.invert( g.glycanNode.toSlc() ))

  }



}

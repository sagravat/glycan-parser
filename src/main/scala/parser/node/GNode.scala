package parser.node

import scala.collection.mutable.Stack

/**
 * Created by sagrava on 3/15/15.
 */
trait GNode {

  def id: Int
  def slc: String
  def anomer: String
  def link : String
  def parentId: Int
  def nodeType: Int
  var children: List[GNode]

  def add(glycanNode: GNode): Unit = {
    children = children :+ glycanNode
  }

  def find(glycanNode: GNode): Option[GNode] = {

    iterator.find(_.id == glycanNode.id)

  }

  def findParent(glycanNode: GNode): Option[GNode] = {

    iterator.find(_.id == glycanNode.parentId)

  }

  def childList: List[GNode] = {

    this :: children.flatMap(_.childList)

  }
  def iterator: Iterator[GNode] = {

    Iterator(this) ++ children.flatMap(_.iterator)

  }

  def prettyPrint(): String = {


    var formattedString = ""
    def printTree(index: Int, g: GNode): Unit = {

      for (i <- 0 until index) {
        formattedString = formattedString + " "
      }
      formattedString = formattedString + s"${g.slc}${g.anomer}${g.link}\n"
      for (child <- g.children) {
        printTree(index + 2, child)
      }

    }

    printTree(0, this)

    formattedString
  }

  def toSlc(): String = {

    val result: StringBuilder = new StringBuilder()
    def buildTree(g: GNode, bracket: Stack[String]): Unit = {

      result.append(s"${g.slc}${g.anomer}${g.link}")

      val num_braces = g.children.size - 1
      if (num_braces >= 1) {
        for (i <- 0 to num_braces - 1) {
          bracket.push(")")
          result.append("(")
        }
      }
      for (child <- g.children) {
        buildTree(child, bracket)
      }

      if (bracket.size > 0 && g.children.size == 0) {
        bracket.pop()
        result.append(")")
      }

    }

    buildTree(this, Stack[String]())
    result.toString()
  }

  def toReducingSlc(): String = {

    val result: StringBuilder = new StringBuilder()
    def buildTree(g: GNode, bracket: Stack[String]): Unit = {

      result.append(s"${g.slc}${g.anomer}${g.link}")

      val num_braces = g.children.size - 1
      if (num_braces >= 1) {
        for (i <- 0 to num_braces - 1) {
          bracket.push(")")
          result.append("(")
        }
      }
      for (child <- g.children) {
        buildTree(child, bracket)
      }

      if (bracket.size > 0 && g.children.size == 0) {
        bracket.pop()
        result.append(")")
      }

    }

    buildTree(this, Stack[String]())
    result.toString()
  }


  override def toString(): String = s"$slc$anomer$link"


//  override def equals(o: Any) = o match {
//    case that: GNode => that.toSlc() == this.toSlc()
//    case _ => false
//  }
//  override def hashCode = toSlc().toUpperCase.hashCode

}

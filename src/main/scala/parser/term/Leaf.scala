package parser.term

/**
 * Created by Sanjay Agravat on 5/29/15.
 */
case object Leaf extends Tree[Nothing] {
  def value: Nothing = fail("empty tree")
  def children: Nothing = fail("empty tree")
  def isEmpty: Boolean = true
}

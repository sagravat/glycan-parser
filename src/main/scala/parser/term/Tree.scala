package parser.term

/**
 * Created by Sanjay Agravat on 5/29/15.
 */
object Tree {

  def empty[T]: Tree[T] = Leaf
  def make[T](x: T, children: List[Tree[T]] = List(Leaf)): Tree[T] =
    Branch[T](x, children)

  /*
  def apply[T](xs: T*): Tree[T] = {
    var r: Tree[T] = Tree.empty
    for (x <- xs) r = r.add(x)
    r
  }
  */
}

abstract class Tree[+T] {

  def value: T
  def children: List[Tree[T]]
  def isEmpty: Boolean

  def iterator: Iterator[T] = {

    if (!this.isEmpty)
      Iterator(this.value) ++ children.flatMap(_.iterator)
    else
      Iterator(this.value)


  }



  def treeIterator: List[Tree[T]] = {


    if (!this.isEmpty)
      this :: children.flatMap(_.treeIterator)
    else
      List(this)
  }



  def find[B >: T](t: B): Option[B] = {

    iterator.find(_ == t)

  }


  def size: Int = {

    children.foldLeft(1)( _ + _.size )

  }

  def add[B >: T ](t: B): Tree[B]= {

    if (isEmpty) Tree.make(t)
    else Tree.make(this.value, Tree.make(t) :: children)

  }

  def addTree[B >: T ](t: Tree[B]): Tree[B]= {

    if (isEmpty) t
    else Tree.make(this.value, t :: children)

  }
  def prettyPrint(): Unit = {

    def traverse(tree: Tree[T], indent: Int):  Unit = {

      println(" " * indent + tree.value)
      for (child <- tree.children) {

        traverse(child, indent + 2)
      }
    }

    traverse(this, 0)
  }

  /**
   * Fails with given message 'm'.
   */
  def fail(m: String) = throw new NoSuchElementException(m)

}
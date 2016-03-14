package parser.term

/**
 * Created by Sanjay Agravat on 5/29/15.
 */
case class Branch[T](value: T,
                     children: List[Tree[T]]
                      ) extends Tree[T] {

  def isEmpty: Boolean = false
}

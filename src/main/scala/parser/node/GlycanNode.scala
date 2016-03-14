package parser.node

/**
 * Created by Sanjay Agravat on 5/29/15.
 */
case class GlycanNode( id: Int,
                       slc: String,
                       anomer: String,
                       link: String,
                       var children: List[GNode] = List())
                        (
                          val parentId: Int = 0,
                          val nodeType: Int = 0) extends GNode {

  def copyNode(): GlycanNode = {



    def copyIt(subTree: GlycanNode): GlycanNode = {
      val newTree = subTree.copy(0, subTree.slc, subTree.anomer, subTree.link, List())(0,0)
      for (child <- subTree.children) {
        val gNode = GlycanNode(child.id, child.slc, child.anomer, child.link, child.children)()
        newTree.add(gNode.copyNode())
        copyIt(gNode)
      }

      newTree
    }

    copyIt(this)
  }
}

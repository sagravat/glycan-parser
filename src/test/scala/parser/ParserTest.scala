package parser

import util.GlycanInverter
import org.junit.Test
import junit.framework.TestCase

class ParserTest extends TestCase {

  @Test
  def testLinearStructure(): Unit = {
    val g: Glycan =
      GlycanIupacParser.parseToTree("Neu5Aca2-6Galb1-4GlcNAcb1-2-N(LT)AVL", "1")
    val slc = GlycanInverter.invert( g.glycanNode.toSlc() )
    assert( slc.equals("Sa6Lb4Nb2"))
  }


}

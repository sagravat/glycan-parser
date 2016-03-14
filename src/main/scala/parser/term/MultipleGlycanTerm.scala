package parser.term

/**
 * Created by Sanjay Agravat on 5/29/15.
 */
case class MultipleGlycanTerm(term: List[GlycanTerm])
                             extends Term with ListGlycanTerm

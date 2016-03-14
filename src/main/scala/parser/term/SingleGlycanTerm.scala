package parser.term

/**
 * Created by Sanjay Agravat on 5/29/15.
 */
case class SingleGlycanTerm(slc: String,
                                 anomer: String,
                                 link: String) extends Term with GlycanTerm

package search

object Models {
  type DocId = Int
  type Token = String
  type PostingList = List[DocId]
  case class Term(term: Token, docFreq: Int = 0)
  type Index = List[(Term, PostingList)]
}





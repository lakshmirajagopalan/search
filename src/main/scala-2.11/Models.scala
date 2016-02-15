package search

object Models {
  type DocId = Int
  type Token = String
  type Term = Token
  type Position = Int
  type PostingList = List[Posting]
  type PositionList = List[Position]

  case class Posting(docId: DocId, docFreq: Int = 0, positionList: PositionList)
  type Index = List[(Term, PostingList)]
}





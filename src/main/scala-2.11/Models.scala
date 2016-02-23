package search

object Models {
  type DocId = Int
  type Token = String
  type Term = Token

  type Position = Int
  type PositionList = List[Position]

  case class Posting(docId: DocId, docFreq: Int = 0, positionList: PositionList)
  type PostingList = List[Posting]

  type Index = List[(Term, PostingList)]

  case class ProximityPosition(left: Position, second: Position)
  case class PostingIntersection(docId: DocId, positions: List[ProximityPosition])
  type PostingIntersectionResult = List[PostingIntersection]
}





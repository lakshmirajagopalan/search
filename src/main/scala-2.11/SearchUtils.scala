package search
import search.Models._
import scala.math.abs

object SearchUtils {
  def intersect(first: List[DocId], second: List[DocId]): List[DocId] =
    (first, second) match {
      case (List(), _) | (_, List()) => Nil
      case (x::xs, y::ys) if x == y => x::intersect(xs, ys)
      case (x::xs, y::ys) if x < y => intersect(xs, y::ys)
      case (x::xs, y::ys) if x > y => intersect(x::xs, ys)
    }

  def positionalIntersect(first: PostingList, second: PostingList, k: Int): PostingIntersectionResult = {
    (first, second) match {
      case (_, Nil) | (Nil, _)  => Nil
      case (x::xs, y::ys) if x.docId == y.docId =>
        var p1 = x.positionList
        var p2 = y.positionList
        var l:List[Position] = List.empty
        var answer: List[ProximityPosition] = List.empty

        while(p1 != Nil) {
          while (p2 != Nil && (p2.head-p1.head < k)) {
            if (abs(p1.head - p2.head) <= k)
              l = l ++ List(p2.head)
            p2 = p2.tail
          }

          while (l != Nil && abs(l.head - p1.head) > k)
            l = l.tail

          answer = answer ++ l.map(pp2 =>ProximityPosition(p1.head, pp2))
          p1 = p1.tail
        }
        if (answer.nonEmpty) PostingIntersection(x.docId, answer) :: positionalIntersect(xs, ys, k)
        else positionalIntersect(xs, ys, k)
      case (x::xs, y::ys) if x.docId > y.docId => positionalIntersect(xs, y::ys, k)
      case (x::xs, y::ys) if x.docId < y.docId => positionalIntersect(x::xs, ys, k)
    }
  }

  def tokenize(str: String): List[Token] = filterStopWords(str).split(" ").toList.map(_.toLowerCase)

  def filterIndex(index: Index, searchTerms: List[Token]): List[PostingList] =
    (index, searchTerms) match {
      case (_, Nil) | (Nil, _) => List.empty
      case (x :: xs, y :: ys) if x._1 == y => x._2 :: filterIndex(xs, ys)
      case (x :: xs, y :: ys) if x._1 < y => filterIndex(xs, y :: ys)
      case (x :: xs, y :: ys) => filterIndex(xs, y :: ys)
    }

  val stopWords = List("\\.", ";", "\\'", ":", "the")

  def filterStopWords(document: String) = stopWords.foldLeft(document)((transformed, stopWord) => transformed.replaceAll(stopWord, "")).replaceAll(" +", " ")

  def stem(token: Token) = {
    var stemmedToken = token
    if (stemmedToken.endsWith("sses")) stemmedToken=stemmedToken.dropRight(2)
    else if(stemmedToken.endsWith("ies")) stemmedToken=stemmedToken.dropRight(2)
    else if(stemmedToken.endsWith("ss")) stemmedToken=stemmedToken
    else if(stemmedToken.endsWith("s")) stemmedToken=stemmedToken.dropRight(2)
    stemmedToken
  }
}

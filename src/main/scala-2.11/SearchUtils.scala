package search
import search.Models._

object SearchUtils {
  def intersect(first: List[DocId], second: List[DocId]): List[DocId] =
    (first, second) match {
      case (List(), _) | (_, List()) => Nil
      case (x::xs, y::ys) if x == y => x::intersect(xs, ys)
      case (x::xs, y::ys) if x < y => intersect(xs, y::ys)
      case (x::xs, y::ys) if x > y => intersect(x::xs, ys)
    }

  def tokenize(str: String): List[Token] = str.split(" ").toList.map(_.toLowerCase).sorted

  def filterIndex(index: Index, searchTerms: List[Token]): List[PostingList] =
    (index, searchTerms) match {
      case (_, Nil) | (Nil, _) => List.empty
      case (x :: xs, y :: ys) if x._1.term == y.toString => x._2 :: filterIndex(xs, ys)
      case (x :: xs, y :: ys) if x._1.term < y => filterIndex(xs, y :: ys)
      case (x :: xs, y :: ys) => filterIndex(xs, y :: ys)
    }

}
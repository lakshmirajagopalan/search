package search
import Models._

class Search(index: Index) {
  def search(query: String) = {
    process(SearchUtils.tokenize(query))
  }

  private def process(searchTerms: List[Token]): List[DocId] = {
    val matchingPostings = SearchUtils.filterIndex(index, searchTerms)
    matchingPostings.tail.foldLeft(matchingPostings.head)({case(acc, postingList) => SearchUtils.intersect(acc, postingList)})
  }
}

object Search extends App {
  val index = List(
    Term("ambitious", 1) -> List(2),
    Term("be", 1) -> List(2),
    Term("brutus", 2) -> List(1, 2),
    Term("capitol", 1) -> List(1),
    Term("caesar", 2) -> List(1, 2),
    Term("julius", 1) -> List(1),
    Term("killed", 1) -> List(2),
    Term("noble", 1) -> List(2),
    Term("was", 2) -> List(1, 2),
    Term("with", 1) -> List(2)
  )
  val query = "noble caesar was killed"

  val result = new Search(index).search(query)
  println(result)
}

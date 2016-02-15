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
  val tokens = List(
    (1, "I did enact Julius Caesar: I was killed i' the Capitol; Brutus killed me"),
    (2, "So let it be with Caesar. The noble Brutus hath told you Caesar was ambitious")
  )
  val index = new Indexer().index(tokens)
  println(index)
  val query = "noble caesar was killed"

  val result = new Search(index).search(query)
  println(result)
}

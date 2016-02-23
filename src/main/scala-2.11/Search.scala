package search
import Models._

class Search(index: Index) {
  def search(query: String) = {
    process(SearchUtils.tokenize(query).sorted)
  }

  private def process(searchTerms: List[Token]): List[DocId] = {
    val matchingPostings: List[PostingList] = SearchUtils.filterIndex(index, searchTerms)

    matchingPostings.reduceLeft((first: PostingList, second: PostingList) => {
      val res = SearchUtils.positionalIntersect(first, second, 5)
        .map(result => Posting(result.docId, positionList = result.positions.map(_.second)))
      res
    }).map(_.docId)
  }
}

object Search extends App {
  val tokens = List(
    (1, "I did enact Julius Caesar: I was kill i' the Capitol; Brutus killed me." +
      "So let it be with Caesar. The noble Brutus hath told you Caesar was ambitious."),
    (2, "Caesar, all hail! good morrow, worthy Caesar: I come to fetch you to the senate-house." +
      "Fear him not, Caesar; he's not dangerous; He is a noble Roman and well given." +
      "The games are done and Caesar is returning. And then, I grant, we put a sting in him, " +
      "And kill him in the shell.")
  )
  val index = new Indexer().index(tokens)
  println(index)
  val query = "caesar kill"

  val result = new Search(index).search(query)
  println(result)
}

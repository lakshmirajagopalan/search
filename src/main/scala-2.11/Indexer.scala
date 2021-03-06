package search

import search.Models._
import search.SearchUtils._

class Indexer() {
  def reduce(list: List[(Token, DocId, Position)]) = {
    list.groupBy(_._2).map{docTerm: (DocId, List[(Token, DocId, Position)]) => {
        val positionList = docTerm._2.map(_._3).sorted
        Posting(docTerm._1, positionList.length, positionList)}}
  }

  def shuffle(tokens: List[(Token, DocId, Position)]): List[(Token, List[(Token, DocId, Position)])] = tokens.groupBy(_._1).toList.sortBy(_._1)

  def map(collection: List[(DocId, String)]): List[(Token, DocId, Position)] = collection.flatMap({ case (docId, document) => tokenize(document).zipWithIndex.map({ case (token, position) => (token, docId, position) }) })

  def index(collection: List[(DocId, String)]): Index =
    shuffle(map(collection)).map((y: (Token, List[(Token, DocId, Position)])) => y._1 -> reduce(y._2).toList)

}



package testvite

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import com.raquo.laminar.api.L.{*, given}

import org.scalajs.dom

case class ViewObject(entity: Entity) extends Component {
  def body: HtmlElement = div(
    h1("View Object with id: ", entity.id),
    ViewValue(entity.value),
    ViewRelations(entity.relations),
  )
}

case class ViewValue(value: Value) extends Component {
  def body: HtmlElement = p(value)
}

case class ViewRelations(relations: Relations) extends Component {
  def body: HtmlElement = div(relations.toList.map(r => ViewRelation(r)))
}

case class ViewRelation(relation: Relation) extends Component {
  def body: HtmlElement = div(
    p(s"${relation._1} -> ${relation._2}"),
  )
}

case class Search(query: String, db: Database) extends Component {
  def body: HtmlElement = div(
    SearchQuery(query),
    SearchResults(db.search(query))
  )
}

case class SearchQuery(query: String) extends Component {
  def body: HtmlElement = div(
    div(
      marginTop("1em"),
      display.flex,
      flexDirection.row,
      h4(margin("0em"), "Search by"),
      input(
        autoFocus(true),
        marginLeft("0.5em"),
        typ := "text",
        value := query,
        onInput.mapToValue --> { query => Router.router.pushState(Page.Search(query)) }
      )
    ),
  )
}

case class SearchResults(results: List[Entity]) extends Component {
  def body: HtmlElement =
    div(
      marginTop("1em"),
      display.flex,
      flexDirection.column,
      results.map(e => a(
        onClick --> { _ => Router.router.pushState(Page.ViewObject(e.id))},
        s"${e.id} ${e.value}"
      )))
}
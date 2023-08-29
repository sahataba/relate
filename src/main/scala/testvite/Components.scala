package testvite

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import com.raquo.laminar.api.L.{*, given}

import org.scalajs.dom

case class ViewObject(entity: Entity, db: Database) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h1("View Object with id: ", entity.id.toString()),
    ViewValue(entity.value),
    ViewRelations(entity.relations, db),
    ViewReferences(entity.references, db),
  )
}

case class ViewValue(value: Value) extends Component {
  def body: HtmlElement = p(
    roundedBorder,
    value
  )
}

case class ViewRelations(relations: Relations, db: Database) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h3("Relations"),
    relations.toList.map(r => ViewRelation(r, db))
  )
}

case class ViewReferences(references: References, db: Database) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h3("References"),
    references.toList.map(r => ViewRelation(r, db))
  )
}

def relationSentence(relation: Relation, db: Database): String = {
  val from = relation.from match {
    case id: ValueId => db.get(id).map(e => e.value).getOrElse("not found")
    case id: RelationId => relationSentence(db.getRelation(id).get, db)//todo .get
  }
  val to = relation.to match {
    case id: ValueId => db.get(id).map(e => e.value).getOrElse("not found")
    case id: RelationId => relationSentence(db.getRelation(id).get, db)
  }
  s"${from} ${relation.kind} ${to}"
}

case class ViewRelation(relation: Relation, db: Database) extends Component {
  def body: HtmlElement = p(relationSentence(relation, db))
}

case class Search(query: String, db: Database) extends Component {
  def body: HtmlElement = div(
    SearchQuery(query),
    SearchResults(db.search(query))
  )
}

case class SearchQuery(query: String) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
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
      roundedBorder,
      marginTop("1em"),
      display.flex,
      flexDirection.column,
      results.map(e => a(
        onClick --> { _ => e.id match {
          case id: RelationId => 
          case id: ValueId => Router.router.pushState(Page.ViewObject(id))
        }},
        s"${e.id} ${e.value}"
      )))
}

def app(): HtmlElement = {
    val db = Database.dummy
    div(
      NavBar(),
      div(
        height("100vh"),
        div(
          marginLeft("2em"),
          display.flex,
          flexDirection.row,
          justifyContent.center,
          child <-- Router.router.currentPageSignal.map {
            case Page.HomePage => div(h1("Relate"))
            case Page.ViewObject(id) => div(
              db.get(id) match {
                case Some(e) => ViewObject(e, db)
                case None => div(h1("Not found"))
              })
            case Page.Search(query) => Search(query, db)
          }
        )
      )
    )
  }
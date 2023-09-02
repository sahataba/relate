package testvite

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import com.raquo.laminar.api.L.{*, given}

import org.scalajs.dom

case class ViewObject(entity: Entity, db: Database, removeRelation: (id: RelationId) => Unit) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h1("View Object with id: ", entity.id.toString()),
    ViewValue(entity.value),
    ViewRelations(entity.relations, db, removeRelation),
    ViewReferences(entity.references, db, removeRelation),
    AddRelations(db, entity.id),
  )
}

case class ViewValue(value: Value) extends Component {
  def body: HtmlElement = p(
    roundedBorder,
    value
  )
}

case class ViewRelations(relations: Relations, db: Database, removeRelation: (id: RelationId) => Unit) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h3("Relations"),
    relations.toList.map(r => ViewRelation(r, db, removeRelation))
  )
}

case class ViewReferences(references: References, db: Database, removeRelation: (id: RelationId) => Unit) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h3("References"),
    references.toList.map(r => ViewRelation(r, db, removeRelation))
  )
}

def relationSentence(relation: Relation, db: Database): HtmlElement = {
  val from = relation.from match {
    case id: ValueId => a(
      aLink,
      db.get(id).map(e => e.value).getOrElse("not found"),
      onClick --> { _ => Router.router.pushState(Page.ViewObject(id))},
    )
    case id: RelationId => relationSentence(db.getRelation(id).get, db)//todo .get
  }
  val to = relation.to match {
    case id: ValueId => a(
      aLink,
      db.get(id).map(e => e.value).getOrElse("not found"),
      onClick --> { _ => Router.router.pushState(Page.ViewObject(id))},
    )
    case id: RelationId => relationSentence(db.getRelation(id).get, db)
  }
  div(
    display.flex,
    flexDirection.row,
    from,
    span(marginLeft("1em"), marginRight("1em"),  s"${relation.kind}"),
    to
  )
}

case class ViewRelation(relation: Relation, db: Database, removeRelation: (id: RelationId) => Unit) extends Component {
  def body: HtmlElement =
    div(
      display.flex,
      flexDirection.row,
      relationSentence(relation, db),
      button(
        "X",
        onClick --> { _ => removeRelation(relation.id) }
      )
    )
}

case class AddRelations(db: Database, from: Id) extends Component {
  val relationsVar = Var(List(
    EditRelation(
      id = db.newRelationId(),
      from = from,
      kind = "has a",
      to = None)))

  def newRelation(from: Option[Id])(): Unit = {
    if (from.isDefined) {
      relationsVar.update(relations => relations :+ EditRelation(
        id = db.newRelationId(),
        from = from.get,
        kind = "has a",
        to = None))
    }
  }
  def body: HtmlElement = div(
    h3("New relations"),
    div(
      display.flex,
      flexDirection.row,
      child <-- relationsVar.signal.map(relations =>
        div(relations.map(r => AddRelation(db, r.from, newRelation(r.to))))
      )
    )
  )
}

case class AddRelation(db: Database, from: Id, newRelation: () => Unit) extends Component {
  val kindVar = Var("has a")
  val toVar = Var("")
  def body: HtmlElement = div(
    display.flex,
    flexDirection.row,
    span(from.toString()),
    input(
      typ := "text",
      value <-- kindVar.signal,
      onInput.mapToValue --> { kind => kindVar.update(_ => kind) }
    ),
    select(
      value <-- toVar.signal,
      db.search("").map(e => option(s"${e.id.toString()} ${e.value}")),
    ),
    button(
      "Add",
      onClick --> { _ => newRelation() }
    )
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
    table(
      roundedBorder,
      tbody(
        marginTop("1em"),
        results.map(e =>
          tr(
            td(
              aLink,
              onClick --> { _ => e.id match {
                case id: RelationId => 
                case id: ValueId => Router.router.pushState(Page.ViewObject(id))
              }},
              s"${e.id}"
            ),
            td(e.value),
          )
        )
      )
    )
}

def app(): HtmlElement = {
    val dbVar = Var(Database.dummy)
    def removeRelation(id: RelationId): Unit = {
      dbVar.update(_.remove(id))
    }
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
              child <-- dbVar.signal.map(db => db.get(id) match {
                case Some(e) => ViewObject(e, db, removeRelation)
                case None => div(h1("Not found"))
              })
            )
            case Page.Search(query) => Search(query, dbVar.now())
          }
        )
      )
    )
  }
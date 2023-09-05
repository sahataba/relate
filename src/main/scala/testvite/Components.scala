package testvite

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import com.raquo.laminar.api.L.{*, given}

import org.scalajs.dom

case class ViewObject(entity: Entity, db: Var[Database], removeRelation: (id: RelationId) => Unit) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h1("View Object with id: ", idToString(entity.id)),
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

case class ViewRelations(relations: Relations, db: Var[Database], removeRelation: (id: RelationId) => Unit) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h3("Relations"),
    relations.toList.map(r => ViewRelation(r, db, removeRelation))
  )
}

case class ViewReferences(references: References, db: Var[Database], removeRelation: (id: RelationId) => Unit) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h3("References"),
    references.toList.map(r => ViewRelation(r, db, removeRelation))
  )
}

def relationSentence(relation: Relation, dbVar: Var[Database]): HtmlElement = {
  val db = dbVar.now()
  val from = relation.from match {
    case id: ValueId => a(
      aLink,
      db.get(id).map(e => e.value).getOrElse("not found"),
      onClick --> { _ => Router.router.pushState(Page.ViewObject(id))},
    )
    case id: RelationId => relationSentence(db.getRelation(id).get, dbVar)//todo .get
  }
  val to = relation.to match {
    case id: ValueId => a(
      aLink,
      db.get(id).map(e => e.value).getOrElse("not found"),
      onClick --> { _ => Router.router.pushState(Page.ViewObject(id))},
    )
    case id: RelationId => relationSentence(db.getRelation(id).get, dbVar)
  }
  div(
    display.flex,
    flexDirection.row,
    idToString(relation.id),
    from,
    span(marginLeft("1em"), marginRight("1em"),  s"${relation.kind}"),
    to
  )
}

case class ViewRelation(relation: Relation, db: Var[Database], removeRelation: (id: RelationId) => Unit) extends Component {
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

case class AddRelations(dbVar: Var[Database], from: Id) extends Component {
  val db = dbVar.now()
  val relationsVar = Var(List(
    EditRelation(
      id = db.newRelationId(),
      from = from,
      kind = "has a",
      to = None)))

  def newRelation(from: Id): Unit = {
    relationsVar.update(relations => {
      val nId = RelationId(Math.max(db.newRelationId().value, relations.map(_.id.value).max + 1))
      val (previous, last) = relations.splitAt(relations.length - 1)
      val updatedRelations = previous :+ last.head.copy(to = Some(nId))
      updatedRelations :+ EditRelation(
      id = nId,
      from = from,
      kind = "has a",
      to = None)
    })
  }
  def saveRelations(): Unit = {
    dbVar.update(db => {
      val edited = relationsVar.now()
      val (previous, partial) = edited.partition(_.to.isDefined)
      val (defined, last) = previous.splitAt(previous.length - 1)
      db.saveRelations((defined ::: last.map(l => l.copy(to = Some(partial.head.from)))).map(r => Relation(r.id, r.from, r.to.get, r.kind)))
    })
  }
  def body: HtmlElement = div(
    div(display.flex, h3(margin("0em"), "New relations"), button("Save", onClick --> { _ => { saveRelations() }})),
    div(
      display.flex,
      flexDirection.row,
      child <-- relationsVar.signal.map(relations =>
        div(relations.map(r => AddRelation(db, r, newRelation)))
      )
    )
  )
}

case class AddRelation(db: Database, relation: EditRelation, newRelation: (from: Id) => Unit) extends Component {
  val kindVar = Var("has a")
  val toVar: Var[Option[Id]] = Var(relation.to)
  def body: HtmlElement = div(
    display.flex,
    flexDirection.row,
    span(idToString(relation.id)),
    span(idToString(relation.from)),
    input(
      typ := "text",
      value <-- kindVar.signal,
      onInput.mapToValue --> { kind => kindVar.update(_ => kind) }
    ),
    select(
      value <-- toVar.signal.map(_.map(idToString).getOrElse("")),
      db.search("").map(e => option(value := idToString(e.id), s"${idToString(e.id)} ${e.value}")),
      onChange.mapToValue --> {v => newRelation(stringToId(v))},
    ),
    button(
      "Add",
      onClick --> { _ => {
        println(toVar.now())
        toVar.now() match {
          case Some(to) => newRelation(to)
          case None => 
        }
      }}
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
              s"${idToString(e.id)}"
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
            case Page.HomePage => div(
              h1("Relate"),
              Graph(),
            )
            case Page.ViewObject(id) => div(
              child <-- dbVar.signal.map(db => db.get(id) match {
                case Some(e) => ViewObject(e, dbVar, removeRelation)
                case None => div(h1("Not found"))
              })
            )
            case Page.Search(query) => Search(query, dbVar.now())
          }
        )
      )
    )
  }
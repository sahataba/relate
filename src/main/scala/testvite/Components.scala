package testvite

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import com.raquo.laminar.api.L.{*, given}
import zio.json._
import org.scalajs.dom

import Page as MyPage
import be.doeraene.webcomponents.ui5.*

case class ViewObject(entity: Entity, db: Var[Database], removeRelation: (id: Relation) => Unit) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h1("View: ", idToString(entity.id)),
    viewId(entity.id, db.now()),
    ViewRelations(entity.relations, db, removeRelation),
    ViewReferences(entity.references, db, removeRelation),
    entity.id match {
      case id: URI => AddRelations(db, id)
      case value: ValueId => div()
    },
  )
}

case class ViewValue(value: Value) extends Component {
  def body: HtmlElement = p(
    roundedBorder,
    value
  )
}

case class ViewRelations(relations: Relations, db: Var[Database], removeRelation: (id: Relation) => Unit) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h3("Relations"),
    relations.toList.map(r => ViewRelation(r, db, removeRelation))
  )
}

case class ViewReferences(references: References, db: Var[Database], removeRelation: (id: Relation) => Unit) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h3("References"),
    references.toList.map(r => ViewRelation(r, db, removeRelation))
  )
}

def viewId(id: Id, db: Database): HtmlElement = id match {
  case id: ValueId => a(
      aLink,
      db.get(id).map(e => idToString(e.id)).getOrElse("not found"),
      onClick --> { _ => Router.router.pushState(MyPage.View(id))},
    )
  case id: URI => a(id.toString())
}

def relationSentence(relation: Relation, dbVar: Var[Database]): HtmlElement = {
  val db = dbVar.now()
  div(
    display.flex,
    flexDirection.row,
    viewId(relation.subject, db),
    span(marginLeft("1em"), marginRight("1em"),  s"${relation.predicate}"),
    viewId(relation.`object`, db),
  )
}

case class ViewRelation(relation: Relation, db: Var[Database], removeRelation: (id: Relation) => Unit) extends Component {
  def body: HtmlElement =
    div(
      display.flex,
      flexDirection.row,
      relationSentence(relation, db),
      button(
        "X",
        onClick --> { _ => removeRelation(relation) }
      )
    )
}

case class AddRelations(dbVar: Var[Database], from: URI) extends Component {
  val db = dbVar.now()
  val relationsVar = Var(List(
    EditRelation(
      subject = Some(from),
      predicate = None,
      `object` = None)))

  def newRelation(from: URI): Unit = {
    relationsVar.update(relations => {
      val (previous, last) = relations.splitAt(relations.length - 1)
      val updatedRelations = previous :+ last.head//.head.copy(`object` = Some(nId))
      updatedRelations :+ EditRelation(
      subject = Some(from),
      predicate = None,
      `object` = None)
    })
  }
  def saveRelations(): Unit = {
    dbVar.update(db => {
      val edited = relationsVar.now()
      val (previous, partial) = edited.partition(_.`object`.isDefined)
      val (defined, last) = previous.splitAt(previous.length - 1)
      db.saveRelations(edited.map(r => Relation(r.subject.get, r.`object`.get, r.predicate.get)))//todo
    })
  }
  def body: HtmlElement = div(
    div(display.flex, h3(margin("0em"), "New relations"), button("Save", onClick --> { _ => { saveRelations() }})),
    div(
      display.flex,
      flexDirection.row,
      child <-- relationsVar.signal.map(relations =>
        div(relations.map(r => AddRelation(dbVar, r, newRelation)))
      )
    )
  )
}

case class AddRelation(dbVar: Var[Database], relation: EditRelation, newRelation: (from: URI) => Unit) extends Component {
  val relationVar = Var(relation)
  val newValueVar: Var[Option[Value]] = Var(None)
  val toVar: Var[Option[Id]] = Var(relation.`object`)
  val db = dbVar.now()
  val allOptions =
    db.search("").map(e => option(value := idToString(e.id), s"${idToString(e.id)}")).concat(
      db.getRelations().map(r => option(value := r.toString(), r.toString())))
  def body: HtmlElement = div(
    display.flex,
    flexDirection.row,
    Input(
      _.readonly := true,
      _.placeholder := "Subject",
      _.showClearIcon := true,
      value <-- relationVar.signal.map(_.subject.map(idToString).getOrElse("")),
      onInput.mapToValue --> { value => relationVar.update(_.copy(subject = if (value.isEmpty()) None else Some(stringToRelationId(value)))) }
    ),
    Input(
      _.placeholder := "Predicate",
      _.showClearIcon := true,
      value <-- relationVar.signal.map(_.predicate.map(idToString).getOrElse("")),
      onInput.mapToValue --> { value => relationVar.update(_.copy(predicate = if (value.isEmpty()) None else Some(stringToRelationId(value)))) }
    ),
    div(
      display.flex,
      flexDirection.column,
      Input(
        _.placeholder := "Object",
        _.showClearIcon := true,
        _.value <-- newValueVar.signal.map(_.getOrElse("")),
        onInput.mapToValue --> { newValue => newValueVar.update(_ => if(newValue.isEmpty()) None else Some(newValue)) }
      ),
      select(
        value <-- toVar.signal.map(_.map(idToString).getOrElse("")),
        allOptions,
        onChange.mapToValue --> {v => newRelation(stringToRelationId(v))},
      ),
    ),
    button(
      "Add",
      onClick --> { _ => {
        val newValue = newValueVar.now()
        newValue match {
          case Some(value) => {
            val (valueId, newDb) = dbVar.now().newValue(value)
            dbVar.update(_ => newDb)
          }
          case None => {
            println(toVar.now())
            val relation = relationVar.now()
            if (relation.subject.isDefined) {
              newRelation(relation.subject.get)
            }
          }
        }
      }}
    )
  )
}

case class Search(query: String, db: Database) extends Component {
  def body: HtmlElement = div(
    SearchQuery(query),
    SearchResults(db.search(query), db)
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
        onInput.mapToValue --> { query => Router.router.pushState(MyPage.Search(query)) }
      )
    ),
  )
}

case class SearchResults(results: List[Entity], db: Database) extends Component {
  def body: HtmlElement =
    table(
      roundedBorder,
      tbody(
        marginTop("1em"),
        results.map(e =>
          tr(
            td(viewId(e.id, db)),
          )
        )
      )
    )
}

def app(): HtmlElement = {
    val localData = dom.window.localStorage.getItem("db")
    val initialData = if (localData == null) data else localData
    val initial = initialData.fromJson[Database].getOrElse(Database.empty)
    val dbVar = Var(initial)
    val o = dbVar.signal.map(db => {
      dom.window.localStorage.setItem("db", db.toJson)
    }).map(s => span(""))
    def removeRelation(id: Relation): Unit = {
      dbVar.update(_.remove(id))
    }
    div(
      NavBar(),
      div(
        height("100vh"),
        span(child <-- o),//todo
        div(
          marginLeft("2em"),
          display.flex,
          flexDirection.row,
          justifyContent.center,
          child <-- Router.router.currentPageSignal.map {
            case MyPage.HomePage => div(
              h1("Relate"),
              Graph(),
            )
            case MyPage.View(id) => div(
              child <-- dbVar.signal.map(db => db.get(id) match {
                case Some(e) => ViewObject(e, dbVar, removeRelation)
                case None => div(h1("Not found"))
              })
            )
            case MyPage.Search(query) => Search(query, dbVar.now())
            case MyPage.ViewDatabase => pre(
              child <-- dbVar.signal.map(db => code(display.block, width("0"), js.JSON.stringify(js.JSON.parse(db.toJson), null, 2)))
            )
          }
        )
      )
    )
  }
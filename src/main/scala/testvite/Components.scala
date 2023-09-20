package testvite

import scala.scalajs.js

import com.raquo.laminar.api.L.{*}
import zio.json._
import org.scalajs.dom

import Page as MyPage
import be.doeraene.webcomponents.ui5.*

case class ViewObject(
    entity: Entity,
    db: Var[Database],
) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h1("View: ", idToString(entity.id)),
    ViewRelations(entity.relations, db),
    ViewReferences(entity.references, db),
    entity.id match {
      case id: URI      => AddRelations(db, id)
      case value: Value => div("todo")
    }
  )
}

case class ViewValue(value: Value) extends Component {
  def body: HtmlElement = p(
    roundedBorder,
    value.value
  )
}

case class ViewRelations(
    relations: Relations,
    db: Var[Database],
) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h3("Relations"),
    relations.toList.map(r => ViewRelation(r, db, "relation"))
  )
}

case class ViewReferences(
    references: References,
    db: Var[Database],
) extends Component {
  def body: HtmlElement = div(
    roundedBorder,
    h3("References"),
    references.toList.map(r => ViewRelation(r, db, "reference"))
  )
}

def viewId(id: Id, hide: Boolean = false): HtmlElement = id match {
  case id: Value =>
    a(
      marginLeft("1em"),
      aLink,
      if (hide) "ID" else id.value,
      onClick --> { _ => Router.router.pushState(MyPage.View(id)) }
    )
  case id: URI =>
    a(
      marginLeft("1em"),
      aLink,
      if (hide) "ID" else id.value,
      onClick --> { _ => Router.router.pushState(MyPage.View(id)) }
    )
}

def relationSentence(relation: Relation, dbVar: Var[Database], viewKind: ViewKind): HtmlElement = {
  val db = dbVar.now()
  div(
    display.flex,
    flexDirection.row,
    viewId(relation.id, hide = true),
    if (viewKind == "relation") div() else viewId(relation.subject),
    viewId(relation.predicate),
    if (viewKind == "reference") div() else viewId(relation.`object`)
  )
}

type ViewKind = "relation" | "reference" | "none"
case class ViewRelation(
    relation: Relation,
    db: Var[Database],
    viewKind: ViewKind = "none"
) extends Component {
  def body: HtmlElement =
    div(
      display.flex,
      flexDirection.row,
      relationSentence(relation, db, viewKind),
      button(
        "X",
        onClick --> { _ => db.update(_.remove(relation.id)) }
      )
    )
}

case class AddRelations(dbVar: Var[Database], from: URI) extends Component {
  val db = dbVar.now()
  val relationsVar = Var(
    List(EditRelation(id = URI.newId(), subject = None, predicate = None, `object` = None))
  )

  def newRelation(): Unit = {
    relationsVar.update(relations => {
      relations :+ EditRelation(
        id = URI.newId(),
        subject = None,
        predicate = None,
        `object` = None
      )
    })
  }
  def body: HtmlElement = div(
    div(
      display.flex,
      h3(margin("0em"), "New relations"),
    ),
    div(
      display.flex,
      flexDirection.row,
      child <-- relationsVar.signal.map(relations =>
        div(relations.map(r => AddRelation(dbVar, r, newRelation)))
      )
    )
  )
}

def selOption(p : URI | Value) =
  p match {
    case URI(v) => Input.suggestion(
      _.text := v
    )
    case Value(v) => Input.suggestion(
      _.text := v
    )
  }

case class AddRelation(
    dbVar: Var[Database],
    relation: EditRelation,
    newRelation: () => Unit
) extends Component {
  val relationVar = Var(relation)
  val db = dbVar.now()
  val allIds =
    db.getRelations().map(r => List(r.subject, r.`object`, r.predicate)).flatten
  val allOptions =
    allIds.map(id => option(value := idToString(id), idToString(id)))
  val allPredicates = db.getRelations().map(_.predicate).distinct
  val allSubjects = db.getRelations().map(_.subject).distinct

  def body: HtmlElement = div(
    display.flex,
    flexDirection.row,
    Input(
      //_.readonly := true,
      _.placeholder := "Subject",
      _.showClearIcon := true,
      _.showSuggestions := true,
      value <-- relationVar.signal.map(_.subject.map(_.value).getOrElse("")),
      onInput.mapToValue --> { value =>
        relationVar.update(
          _.copy(subject =
            if (value.isEmpty()) None else Some(URI(value))
          )
        )
      },
       allIds.map(selOption)
    ),
    Input(
      _.placeholder := "Predicate",
      _.showClearIcon := true,
      _.showSuggestions := true,
      value <-- relationVar.signal.map(
        _.predicate.map(_.value).getOrElse("")
      ),
      onInput.mapToValue --> { value =>
        relationVar.update(
          _.copy(predicate =
            if (value.isEmpty()) None else Some(URI(value))
          )
        )
      },
      allIds.map(selOption)
    ),
    div(
      display.flex,
      flexDirection.column,
      Input(
        _.placeholder := "Object",
        _.showClearIcon := true,
        value <-- relationVar.signal.map(value => {
          value.`object` match {
            case Some(Value(v)) => v
            case Some(URI(v))    => ""
            case None => ""
          }

        }
        ),
        onInput.mapToValue --> { value =>
          relationVar.update(
            _.copy(`object` =
              if (value.isEmpty()) None else Some(Value(value))
            )
          )
        }
      ),
      select(
        value <-- relationVar.signal.map(
          _.`object`.map(idToString).getOrElse("")
        ),
        allOptions,
        onChange.mapToValue --> { value =>
          relationVar.update(
            _.copy(`object` =
              if (value.isEmpty()) None else Some(stringToId(value))
            )
          )
        },
      )
    ),
    button(
      "Add",
      onClick --> { _ => {
        val r = relationVar.now()
        dbVar.update(_.saveRelations(List(r.toRelation.get)))
      }}
    )
  )
}

case class Search(query: String, db: Var[Database]) extends Component {
  def body: HtmlElement = div(
    SearchQuery(query),
    SearchResults(db.now().search(query), db)
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
        onInput.mapToValue --> { query =>
          Router.router.pushState(MyPage.Search(query))
        }
      )
    )
  )
}

case class SearchResults(results: List[Relation], db: Var[Database])
    extends Component {
  def body: HtmlElement =
    table(
      roundedBorder,
      tbody(
        marginTop("1em"),
        results.map(e =>
          tr(
            td(relationSentence(e, db, "none"))
          )
        )
      )
    )
}

case class Add() extends Component {
  var somethingVar: Var[String] = Var("")
  var canAdd: Signal[Boolean] = somethingVar.signal.map(_.nonEmpty)
  def body: HtmlElement =
    div(
      h1("Add"),
      Input(
        _.placeholder := "Something",
        onInput.mapToValue --> { value =>
          somethingVar.update(_ => value)
        }
      ),
      div(
        display.flex,
        flexDirection.row,
        button(
          "Add Value",
          disabled <-- canAdd.map(!_),
          onClick --> { _ => {
            val something = somethingVar.now()
            println(something)
          }}
        ),
        button(
          "Add Thing",
          disabled <-- canAdd.map(!_),
          onClick --> { _ => {
            val something = somethingVar.now()
            println(something)
          }}
        )
      )
    )
}

def app(): HtmlElement = {
  val localData = dom.window.localStorage.getItem("db")
  val initialData = if (localData == null) data else localData
  val initial = initialData.fromJson[Database].getOrElse(Database.empty)
  val dbVar = Var(initial)
  val o = dbVar.signal
    .map(db => {
      dom.window.localStorage.setItem("db", db.toJson)
    })
    .map(s => span(""))
  div(
    NavBar(),
    div(
      height("100vh"),
      span(child <-- o), // todo
      div(
        marginLeft("2em"),
        display.flex,
        flexDirection.row,
        justifyContent.center,
        child <-- Router.router.currentPageSignal.map {
          case MyPage.Add => Add()
          case MyPage.HomePage =>
            div(
              h1("Relate"),
              Graph()
            )
          case MyPage.View(id) =>
            div(
              child <-- dbVar.signal.map(db =>
                ViewObject(db.get(id), dbVar)
              )
            )
          case MyPage.Search(query) => Search(query, dbVar)
          case MyPage.ViewDatabase =>
            pre(
              child <-- dbVar.signal.map(db =>
                code(
                  display.block,
                  width("0"),
                  js.JSON.stringify(js.JSON.parse(db.toJson), null, 2)
                )
              )
            )
        }
      )
    )
  )
}

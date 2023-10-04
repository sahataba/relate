package testvite

import scala.scalajs.js

import com.raquo.laminar.api.L.{*}
import zio.json._
import org.scalajs.dom

import Page as MyPage
import be.doeraene.webcomponents.ui5.*
import be.doeraene.webcomponents.ui5.configkeys.{ButtonDesign, IconName}

case class ViewObject(
    entity: Entity,
    db: Var[Database],
) extends Component {
  val selectedRelation: Var[Option[SelectedRelation]] = Var(None)

  def body: HtmlElement = div(
    Title("View: ", idToString(entity.id)),
    ViewRelations(entity, db, Some(selectedRelation)),
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
    entity: Entity,
    db: Var[Database],
    selectedRelation: Option[Var[Option[SelectedRelation]]],
) extends Component {
  def body: HtmlElement = Panel(
    _.headerText := "Relations",
    entity.relations.toList.map(r => ViewRelation(r, db, selectedRelation, "relation")),
    Add(db, Some(entity.id), selectedRelation)
  )
}

case class ViewReferences(
    references: References,
    db: Var[Database],
) extends Component {
  def body: HtmlElement = Panel(
    _.headerText := "References",
    references.toList.map(r => ViewRelation(r, db, None, "reference"))
  )
}

def toS(id: Id): String = id match {
  case id: URI   => "ID"
  case id: Value => id.value
}

def getName(id: Id, db: Var[Database]): String = {
  val e = db.now().get(id)
  e.relations
    .find(_.predicate == Predicate.name)
    .map(a => toS(a.`object`))
    .getOrElse(toS(id))
}

//expand with special predicat "name"
//think of combining add and search page
//predicate order: name
//one level depth
def viewId(id: Id, db: Var[Database], hide: Boolean = false): HtmlElement = id match {
  case id: Value =>
    Link(
      marginLeft("1em"),
      //aLink,
      if (hide) getName(id, db) else id.value,
      onClick --> { _ => Router.router.pushState(MyPage.View(id)) }
    )
  case id: URI =>
    Link(
      marginLeft("1em"),
      //aLink,
      if (hide) s"${getName(id, db)}" else id.value,
      onClick --> { _ => Router.router.pushState(MyPage.View(id)) }
    )
}

def selectRelation(relation: Relation, selectedRelationComp: Option[Var[Option[SelectedRelation]]], position: Position): HtmlElement = {
  selectedRelationComp match {
    case Some(selectedRelationVar) => {
      val selectedColor: Signal[String] = selectedRelationVar.signal.map(_.map(s => if(s.relationId == relation.id) "red" else "black").getOrElse("black"))
      Button(
        _.design := ButtonDesign.Transparent,
        _.icon := IconName.add,
        color <-- selectedColor,
        onClick --> { _ => selectedRelationVar.update(_ => Some(SelectedRelation(relationId = relation.id, position))) }
      )
    }
    case None => div()
  }
}

def relationSentence(relation: Relation, dbVar: Var[Database], selectedRelationComp: Option[Var[Option[SelectedRelation]]], viewKind: ViewKind): HtmlElement = {
  div(
    display.flex,
    flexDirection.row,
    alignItems.center,
    viewId(relation.id, dbVar, hide = true),
    if (viewKind == "relation") div() else viewId(relation.subject, dbVar, hide = true),
    selectRelation(relation, selectedRelationComp, "ExtractObjectSetPredicate"),
    viewId(relation.predicate, dbVar, hide= true),
    selectRelation(relation, selectedRelationComp, "SetPredicate"),
    if (viewKind == "reference") div() else viewId(relation.`object`, dbVar, hide = true)
  )
}

type ViewKind = "relation" | "reference" | "none"
case class ViewRelation(
    relation: Relation,
    db: Var[Database],
    selectedRelation: Option[Var[Option[SelectedRelation]]],
    viewKind: ViewKind = "none"
) extends Component {
  def body: HtmlElement =
    div(
      display.flex,
      flexDirection.row,
      relationSentence(relation, db, selectedRelation, viewKind),
      Button(
        _.design := ButtonDesign.Transparent,
        _.icon := IconName.delete,
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
  def body: HtmlElement = Panel(
    _.headerText := "New Relations",
    div(
      display.flex,
      flexDirection.row,
      child <-- relationsVar.signal.map(relations =>
        div(relations.map(r => AddRelation(dbVar, r, newRelation)))
      )
    )
  )
}

//reuse viewId here
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
    Button(
      "Add",
      onClick --> { _ => {
        val r = relationVar.now()
        dbVar.update(_.saveRelations(List(r.toRelation.get)))
      }}
    )
  )
}

case class Search(query: String, db: Var[Database]) extends Component {
  val queryVar: Var[String] = Var(query)
  val res =
    for {
      q <- queryVar.signal
      d <- db.signal
    } yield d.search(q)
  def body: HtmlElement = div(
    SearchQuery(queryVar),
    SearchResults(
      res,
      db,
      selectedRelationComp = None)
  )
}

case class SearchQuery(queryVar: Var[String]) extends Component {
  def body: HtmlElement = Panel(
    _.headerText := "Query",
    div(
      marginTop("1em"),
      display.flex,
      flexDirection.row,
      input(
        autoFocus(true),
        marginLeft("0.5em"),
        typ := "text",
        value <-- queryVar,
        onInput.mapToValue --> { query => queryVar.update(v => query)}
      )
    )
  )
}

case class AddPredicateLink(
  predicateId: URI,
  dbVar: Var[Database],
  selectedRelationComp: Option[Var[Option[SelectedRelation]]]) extends Component {

  def body: HtmlElement = selectedRelationComp match {
    case Some(selectedRelationVar) => Button(
      _.design := ButtonDesign.Transparent,
      _.icon := IconName.`slim-arrow-left`,
      onClick --> { _ => {
        selectedRelationVar.now() match {
          case Some(sr) =>  sr.position match {
            case "SetPredicate" => Manager.exec(dbVar)(SetPredicate(sr.relationId, predicateId))
            case "ExtractObjectSetPredicate" => Manager.exec(dbVar)(ExtractObjectSetPredicate(sr.relationId, predicateId))
          }
          case None => //todo
        }
      }}
    )
    case None => div("")
  }
}

case class SearchResults(
  resultsSignal: Signal[List[Relation]],
  dbVar: Var[Database],
  viewKind: ViewKind = "none",
  toThing: Option[Id] = None,
  selectedRelationComp: Option[Var[Option[SelectedRelation]]],//fix by converting to signals
) extends Component {
  def actions(e: Relation) =
    Button(
      _.design := ButtonDesign.Transparent,
      _.icon := IconName.delete,
      onClick --> { _ => dbVar.update(_.remove(e.id)) }
    )

  def body: HtmlElement =
    Panel(
      _.headerText := "Results",
      Table(
        _.slots.columns := Table.column(Label("")),
        _.slots.columns := Table.column(Label("Subject")),
        _.slots.columns := Table.column(Label("Predicate")),
        _.slots.columns := Table.column(Label("Object")),
        _.slots.columns := Table.column(Label("")),//object action
        _.slots.columns := Table.column(Label("")),
          children <-- resultsSignal.map(_.map(e =>
            Table.row(
              _.cell(viewId(e.id, dbVar, hide = true)),
              _.cell(if (viewKind == "relation") div() else div(
                display.flex,
                flexDirection.row,
                justifyContent.center,
                alignItems.center,
                AddPredicateLink(e.subject, dbVar, selectedRelationComp),
                viewId(e.subject, dbVar, hide = true)
              )),
              _.cell(div(
                display.flex,
                flexDirection.row,
                justifyContent.center,
                alignItems.center,
                AddPredicateLink(e.predicate, dbVar, selectedRelationComp),
                viewId(e.predicate, dbVar, hide= true)
              )),
              _.cell(
                if (viewKind == "reference")
                  div()
                else
                  div(
                    viewId(e.`object`, dbVar, hide = true),
                  )
              ),
              _.cell(
                if (viewKind != "reference")
                  Button(
                    hidden := toThing.isEmpty,
                    _.design := ButtonDesign.Transparent,
                    _.icon := IconName.add,
                    onClick --> { _ =>
                      toThing match {
                        case Some(to) => {
                          to match {
                            case to: URI => Manager.exec(dbVar)(LinkThing(e.`object`, to))
                            case to: Value =>
                          }
                        }
                        case None => 
                      }
                    }
                  )
                else
                  div()
              ),
              _.cell(actions(e))),
          ))
      )
    )
}

//todo: remove delete
//user adds subject from search result relations, to id predicate, by clicking on row,
case class Add(db: Var[Database], toThing: Option[Id], selectedRelationComp: Option[Var[Option[SelectedRelation]]]) extends Component {
  var somethingVar: Var[String] = Var("")
  var canAdd: Signal[Boolean] = somethingVar.signal.map(_.nonEmpty)
  val res =
    for {
      q <- somethingVar.signal
      d <- db.signal
    } yield if(q.isEmpty()) Nil else d.search(q)
  def body: HtmlElement =
    Panel(
      _.headerText := "Add",
      Input(
        _.placeholder := "Something",
        onInput.mapToValue --> { value =>
          somethingVar.update(_ => value)
        }
      ),
      Button(
        "Add Value",
        disabled <-- canAdd.map(!_),
        onClick --> { _ => Manager.exec(db)(AddValue(somethingVar.now(), toThing))}
      ),
      Button(
        "Add Named Thing",
        disabled <-- canAdd.map(!_),
        onClick --> { _ => Manager.exec(db)(AddNewThing(somethingVar.now(), toThing))}// add navigate
      ),
      child <-- canAdd.map(can => if (can) SearchResults(res, db, "none", toThing, selectedRelationComp) else div())
    )
}
//when we add thing as root one, we should navigate to its subject
//if we are on add to something, than we stay on current something

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
          case MyPage.Add(to) => Add(dbVar, to, None)
          case MyPage.HomePage =>
            div(
              Title("Relate"),
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

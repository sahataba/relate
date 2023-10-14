package testvite

import scala.scalajs.js

import com.raquo.laminar.api.L.{*}
import zio.json._
import org.scalajs.dom

import Page as MyPage
import be.doeraene.webcomponents.ui5.*
import be.doeraene.webcomponents.ui5.configkeys.{ButtonDesign, IconName}
import com.raquo.airstream.core.Signal

val selectionIcon = IconName.`slim-arrow-left`
val addingIcon = IconName.add

case class ViewObject(
    entity: Entity,
    db: Var[Database],
) extends Component {
  val selectedRelation: Var[Option[SelectedRelation]] = Var(None)

  def body: HtmlElement = div(
    Title("View: ", idToString(entity.id)),
    ViewRelations(entity, db, Some(selectedRelation)),
    ViewReferences(entity.references, db),
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

//if we are showing an id of object with only one relation, "not a name", than we display inner structure in "ID" view
def toS(id: Id): String = id match {
  case id: URI   => "ID"
  case id: Value => id.value
}

def getName(id: Id, db: Var[Database]): String = {
  val e = db.now().get(id)
  val namePredicate = e.relations.find(_.predicate == Predicate.name)
  val foundPredicate = namePredicate.orElse(e.references.headOption)
  foundPredicate
    .map(a => toS(a.`object`))//todo check this
    .getOrElse(toS(id))
}

//expand with special predicat "name"
//think of combining add and search page
//predicate order: name
//one level depth
//if there is no name, and there is only 1 predicate then expand with it, and if theres more than also add "more" sign
//expand = nested object
//execute = function
//elaborate = moving an object to predicate and setting object
//define = setting predicate
//contraints: we cant elaborate on nested object
//we cant elaborate on functions

def viewId(id: Id, db: Var[Database]): HtmlElement = id match {
  case id: Value =>
    Link(
      marginLeft("1em"),
      //aLink,
      getName(id, db),
      onClick --> { _ => Router.router.pushState(MyPage.View(id)) }
    )
  case id: URI =>
    Link(
      marginLeft("1em"),
      //aLink,
      s"${getName(id, db)}",
      onClick --> { _ => Router.router.pushState(MyPage.View(id)) }
    )
}

def selectRelation(relation: Relation, selectedRelationComp: Option[Var[Option[SelectedRelation]]], position: Position): HtmlElement = {
  selectedRelationComp match {
    case Some(selectedRelationVar) => {
      val selectedColor: Signal[String] = selectedRelationVar.signal.map(_.map(s => if(s.relationId == relation.id && s.position == position) "red" else "black").getOrElse("black"))
      Button(
        _.design := ButtonDesign.Transparent,
        _.icon := selectionIcon,
        color <-- selectedColor,
        onClick --> { _ => selectedRelationVar.update(_ => Some(SelectedRelation(relationId = relation.id, position))) }
      )
    }
    case None => div()
  }
}

def relationSentence(relation: Relation, dbVar: Var[Database], selectedRelationComp: Option[Var[Option[SelectedRelation]]], viewKind: ViewKind): HtmlElement = {
  div(
    simpleInline,
    viewId(relation.id, dbVar),
    if (viewKind == "relation") div() else viewId(relation.subject, dbVar),
    if(relation.predicate != Predicate.blank) selectRelation(relation, selectedRelationComp, "ExtractObjectSetPredicate") else div(),
    viewId(relation.predicate, dbVar),
    selectRelation(relation, selectedRelationComp, if(relation.predicate == Predicate.blank) "SetPredicate" else "ExtractObjectToObjectWithNewPredicate"),
    if (viewKind == "reference") div() else div(simpleInline, viewId(relation.`object`, dbVar), if (relation.predicate != Predicate.blank || relation.`object`.isInstanceOf[Value]) div() else selectRelation(relation, selectedRelationComp, "MoveObjectToPredicateAndSetObject")),
  )
}

//inline editing of first level objects

type ViewKind = "relation" | "reference" | "none"
case class ViewRelation(
    relation: Relation,
    db: Var[Database],
    selectedRelation: Option[Var[Option[SelectedRelation]]],
    viewKind: ViewKind = "none"
) extends Component {
  def body: HtmlElement =
    div(
      simpleInline,
      relationSentence(relation, db, selectedRelation, viewKind),
      Button(
        _.design := ButtonDesign.Transparent,
        _.icon := IconName.delete,
        onClick --> { _ => db.update(_.remove(relation.id)) }
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
      simpleInline,
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
      _.icon := selectionIcon,
      onClick --> { _ => {
        selectedRelationVar.now() match {
          case Some(sr) =>  sr.position match {
            case "SetPredicate" => Manager.exec(dbVar)(SetPredicate(sr.relationId, predicateId))
            case "ExtractObjectSetPredicate" => Manager.exec(dbVar)(ExtractObjectSetPredicate(sr.relationId, predicateId))
            case "ExtractObjectToObjectWithNewPredicate" => Manager.exec(dbVar)(ExtractObjectToObjectWithNewPredicate(sr.relationId, predicateId))
            case "MoveObjectToPredicateAndSetObject" => Manager.exec(dbVar)(MoveObjectToPredicateAndSetObject(sr.relationId, predicateId))
          }
          case None => //todo
        }
      }}
    )
    case None => div("")
  }
}
//add S8 case, to add object if predicate is already set
case class LinkThingButton(
  toThing: Option[Id] = None,
  dbVar: Var[Database],
  e: Relation,
  id: URI,
) extends Component {
  def body: HtmlElement =
    Button(
      hidden := toThing.isEmpty,
      _.design := ButtonDesign.Transparent,
      _.icon := addingIcon,
      onClick --> { _ =>
        toThing match {
          case Some(to) => {
            to match {
              case to: URI => Manager.exec(dbVar)(LinkThing(id, to))//add this to other positions
              case to: Value =>
            }
          }
          case None => 
        }
      }
    )
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

  val inSelection = selectedRelationComp.map(_.signal.map(_.isDefined)).getOrElse(Signal.fromValue(false))
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
              _.cell(viewId(e.id, dbVar)),
              _.cell(if (viewKind == "relation") div() else div(
                simpleInline,
                justifyContent.center,
                viewId(e.subject, dbVar),
                child <-- inSelection.map(inSelection => if(inSelection) AddPredicateLink(e.subject, dbVar, selectedRelationComp) else LinkThingButton(toThing, dbVar, e, e.subject)),
              )),
              _.cell(div(
                simpleInline,
                justifyContent.center,
                viewId(e.predicate, dbVar),
                child <-- inSelection.map(inSelection => if(inSelection) AddPredicateLink(e.predicate, dbVar, selectedRelationComp) else LinkThingButton(toThing, dbVar, e, e.predicate)),
              )),
              _.cell(
                if (viewKind == "reference")
                  div()
                else
                  div(
                    viewId(e.`object`, dbVar),
                  )
              ),
              _.cell(
                if (viewKind != "reference")
                  e.`object` match {
                    case id: URI => LinkThingButton(toThing, dbVar, e, id)
                    case v: Value => div()
                  }
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
        onClick --> { _ => {
          val someRelationId =
            selectedRelationComp.flatMap(_.now().map(_.relationId))//todo add position check
          someRelationId match {
            case Some(relationId) => Manager.exec(db)(MoveObjectToPredicateAndSetNewThing(relationId, somethingVar.now(), "value"))
            case None => Manager.exec(db)(AddNewValue(somethingVar.now(), toThing))
          }        
        }}
      ),
      Button(
        "Add Named Thing",
        disabled <-- canAdd.map(!_), //make both add value and add name thing to respect selectedrelation and expand position setting
        onClick --> { _ => {
          val someRelationId =
            selectedRelationComp.flatMap(_.now().map(_.relationId))//todo add position check
          someRelationId match {
            case Some(relationId) => Manager.exec(db)(MoveObjectToPredicateAndSetNewThing(relationId, somethingVar.now(), "object"))
            case None => Manager.exec(db)(AddNewThing(somethingVar.now(), toThing))
          }
        }}
      ),
      child <-- canAdd.map(can => if (can) SearchResults(res, db, "none", toThing, selectedRelationComp) else div())
    )
}
//when we add thing as root one, we should navigate to its subject
//if we are on add to something, than we stay on current something

def app(): HtmlElement = {
  val localData = dom.window.localStorage.getItem("db")
  val initialData = if (localData == null) data else localData
  val initial = initialData.fromJson[Database].getOrElse(Database.initial)
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
        simpleInline,
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

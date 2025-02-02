package relate

import com.raquo.laminar.api.L.{*}
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
  val selectedRelation: Option[Var[Option[SelectedRelation]]] = Some(Var(None))

  def body: HtmlElement = div(
    display.flex,
    flexDirection.row,
    div(
      ViewRelations(entity, db, selectedRelation),
      ViewReferences(entity.references, db),
    ),
    Add(db, Some(entity.id), selectedRelation)
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

def isSmallObject(id: Id, db: Database): Option[Relation] =
  val refs = db.get(id).relations
  if (refs.size == 1 && refs.head.predicate != Predicate.name) Some(refs.head) else None

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

def viewId(id: Id, db: Var[Database], selectedRelationComp: Option[Var[Option[SelectedRelation]]], level: Int = 0): HtmlElement = id match {
  case id: Value =>
    Link(
      marginLeft("1em"),
      //aLink,
      getName(id, db),
      onClick --> { _ => Router.router.pushState(MyPage.View(id)) }
    )
  case id: URI =>
    isSmallObject(id, db.now()).filter(_ => level == 0) match {
      case Some(relation) => relationSentence(relation, db, selectedRelationComp, "none", level + 1)
      case None => Link(
        marginLeft("1em"),
        //aLink,
        s"${getName(id, db)}",
        onClick --> { _ => Router.router.pushState(MyPage.View(id)) }
      )
    } 
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

def relationSentence(relation: Relation, dbVar: Var[Database], selectedRelationComp: Option[Var[Option[SelectedRelation]]], viewKind: ViewKind, level: Int = 0): HtmlElement = {
  div(
    simpleInline,
    if (level == 1) borderBottom("1px solid #aaa") else Modifier.empty,
    if (viewKind == "relation") div() else viewId(relation.subject, dbVar, selectedRelationComp, level),
    if(relation.predicate != Predicate.blank) selectRelation(relation, selectedRelationComp, "ExtractObjectSetPredicate") else div(),
    viewId(relation.predicate, dbVar, selectedRelationComp, level),
    selectRelation(relation, selectedRelationComp, if(relation.predicate == Predicate.blank) "SetPredicate" else "ExtractObjectToObjectWithNewPredicate"),
    if (viewKind == "reference")
      div()
    else
      div(
        simpleInline,
        viewId(relation.`object`, dbVar, selectedRelationComp, level),
        if (relation.`object`.isInstanceOf[Value])
          div()
        else
          if (relation.predicate == Predicate.blank)
            selectRelation(relation, selectedRelationComp, "MoveObjectToPredicateAndSetObject")
          else
            selectRelation(relation, selectedRelationComp, "ExtractObjectToPredicateWithNewObject")
      ),
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
      simpleInline,
      relationSentence(relation, db, selectedRelation, viewKind),
      Button(
        _.design := ButtonDesign.Transparent,
        _.icon := IconName.delete,
        onClick --> { _ => db.update(_.remove(relation.id)) }
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
            case "ExtractObjectToPredicateWithNewObject" => Manager.exec(dbVar)(ExtractObjectToPredicateWithNewObject(sr.relationId, predicateId))
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

package relate

import com.raquo.laminar.api.L.{*}
import be.doeraene.webcomponents.ui5.*
import be.doeraene.webcomponents.ui5.configkeys.{ButtonDesign, IconName}

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
              _.cell(viewId(e.id, dbVar, selectedRelationComp)),
              _.cell(if (viewKind == "relation") div() else div(
                simpleInline,
                justifyContent.center,
                viewId(e.subject, dbVar, selectedRelationComp),
                child <-- inSelection.map(inSelection => if(inSelection) AddPredicateLink(e.subject, dbVar, selectedRelationComp) else LinkThingButton(toThing, dbVar, e, e.subject)),
              )),
              _.cell(div(
                simpleInline,
                justifyContent.center,
                viewId(e.predicate, dbVar, selectedRelationComp),
                child <-- inSelection.map(inSelection => if(inSelection) AddPredicateLink(e.predicate, dbVar, selectedRelationComp) else LinkThingButton(toThing, dbVar, e, e.predicate)),
              )),
              _.cell(
                if (viewKind == "reference")
                  div()
                else
                  div(
                    viewId(e.`object`, dbVar, selectedRelationComp),
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
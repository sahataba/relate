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
    SearchResults(db.search(query))
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

object Main {
  final class DataItemID

  case class DataItem(id: DataItemID, label: String, value: Double)

  object DataItem {
    def apply(): DataItem = DataItem(DataItemID(), "?", Math.random())
  }

  val dataVar = Var[List[DataItem]](List(DataItem(DataItemID(), "one", 1.0)))
  val dataSignal = dataVar.signal
  val allValues = dataSignal.map(_.map(_.value))

  def main(args: Array[String]): Unit = {
    // Laminar initialization
    renderOnDomContentLoaded(dom.document.querySelector("#app"), appElement())
  }

  def appElement(): HtmlElement = {
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
              Database.dummy.get(id) match {
                case Some(e) => ViewObject(e)
                case None => div(h1("Not found"))
              })
            case Page.Search(query) => Search(query, Database.dummy)
          }
        )
      )
      //renderDataTable(),
      /* ul(
        li("Sum of values: ", child.text <-- allValues.map(_.sum)),
        li("Average value: ", child.text <-- allValues.map(vs => vs.sum / vs.size)),
      ),*/
    )
  }

  def renderDataTable(): HtmlElement = {
    table(
      thead(
        tr(th("Label"), th("Value"), th("Action")),
      ),
      tbody(
        children <-- dataSignal.split(_.id) { (id, initial, itemSignal) =>
          renderDataItem(id, itemSignal)
        }
      ),
      tfoot(
        tr(td(button("➕", onClick --> (_ => dataVar.update(data => data :+ DataItem()))))),
      ),
    )
  }

  def renderDataItem(id: DataItemID, item: Signal[DataItem]): HtmlElement = {
    val labelUpdater = dataVar.updater[String] { (data, newLabel) =>
      data.map(item => if item.id == id then item.copy(label = newLabel) else item)
    }

    val valueUpdater = dataVar.updater[Double] { (data, newValue) =>
      data.map(item => if item.id == id then item.copy(value = newValue) else item)
    }

    tr(
      td(inputForString(item.map(_.label), labelUpdater)),
      td(inputForDouble(item.map(_.value), valueUpdater)),
      td(button("🗑️", onClick --> (_ => dataVar.update(data => data.filter(_.id != id))))),
    )
  }

  def inputForString(valueSignal: Signal[String], valueUpdater: Observer[String]): Input = {
    input(
      typ := "text",
      controlled(
        value <-- valueSignal,
        onInput.mapToValue --> valueUpdater,
      ),
    )
  }

  def inputForDouble(valueSignal: Signal[Double], valueUpdater: Observer[Double]): Input = {
    val strValue = Var[String]("")
    input(
      typ := "text",
      controlled(
        value <-- strValue.signal,
        onInput.mapToValue --> strValue,
      ),
      valueSignal --> strValue.updater[Double] { (prevStr, newValue) =>
        if prevStr.toDoubleOption.contains(newValue) then prevStr
        else newValue.toString
      },
      strValue.signal --> { valueStr =>
        valueStr.toDoubleOption.foreach(valueUpdater.onNext)
      },
    )
  }
}

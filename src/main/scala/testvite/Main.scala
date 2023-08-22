package testvite

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import com.raquo.laminar.api.L.{*, given}

import org.scalajs.dom
import javax.swing.text.html.parser.Entity

type Relation = (Id, Id)
type Relationships = Set[Relation]
type Value = String
type Id = Int
case class Entity(id: Id)
object Pages {
  def viewObject(): HtmlElement = {
    div(
      h1("View Object"),
    )
  }

  def editObject(): HtmlElement = {
    div(
      h1("Edit Object"),
    )
  }

  def createObject(): HtmlElement = {
    div(
      h1("Create Object"),
    )
  }

  def search(): HtmlElement = {
    div(
      h1("Search"),
    )
  }
}

case class ViewObject(entity: Entity) extends Component {
  def body: HtmlElement = div(
    h1("View Object with id: ", entity.id),
  )
}

case class ViewValue(value: Value) extends Component {
  def body: HtmlElement = p(value)
}

case class ViewRelationships(relations: Relationships) extends Component {
  def body: HtmlElement = div(relations.toList.map(r => ViewRelation(r)))
}

case class ViewRelation(relation: Relation) extends Component {
  def body: HtmlElement = div(
    p(s"${relation._1} -> ${relation._2}"),
  )
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
      h1("Relate"),
      //renderDataTable(),
      ul(
        li("Sum of values: ", child.text <-- allValues.map(_.sum)),
        li("Average value: ", child.text <-- allValues.map(vs => vs.sum / vs.size)),
      ),
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

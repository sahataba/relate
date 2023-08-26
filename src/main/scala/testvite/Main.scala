package testvite

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.`import`
import scala.scalajs.js.annotation.JSImport

import com.raquo.laminar.api.L.{*, given}

import org.scalajs.dom
import scalacss.DevDefaults._

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
    windowEvents(_.onLoad).foreach { _ =>
      val sty = styleTag(CSS.render[String])
      dom.document.querySelector("head").appendChild(sty.ref)
      val containerNode = dom.document.querySelector("#app")
      render(
        containerNode,
        appElement(),
      )
    }(unsafeWindowOwner)
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

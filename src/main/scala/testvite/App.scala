package testvite

import scala.scalajs.js
import com.raquo.laminar.api.L.{*}
import org.scalajs.dom
import Page as MyPage
import zio.json._
import be.doeraene.webcomponents.ui5.*

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
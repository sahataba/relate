package testvite

import com.raquo.laminar.api._
import com.raquo.laminar.api.L
import com.raquo.waypoint._
import zio.json._

import java.util.UUID

sealed trait Page

object Page:
  case class View(id: ValueId) extends Page
  case object HomePage extends Page
  case class Search(query: String) extends Page
  case object ViewDatabase extends Page
  given JsonDecoder[ValueId] = JsonDecoder[Int].map(ValueId.apply)
  given JsonEncoder[ValueId] = JsonEncoder[Int].contramap(a => a.value)
  implicit val codec: JsonCodec[Page] = DeriveJsonCodec.gen[Page]

object Router:
  import Page._

  val homeRoute: Route[Page.HomePage.type, Unit] =
    Route.static(HomePage, root / endOfSegments)

  val viewDatabaseRoute: Route[Page.ViewDatabase.type, Unit] =
    Route.static(ViewDatabase, root / "database" / endOfSegments)

  val viewObjectRoute = Route[View, Int](
    encode = page => page.id.value,
    decode = arg => View(ValueId(arg)),
    pattern = root / "view" / segment[Int] / endOfSegments,
  )

  val searchRoute = Route[Search, String](
    encode = page => page.query,
    decode = arg => Search(arg),
    pattern = root / "search" / segment[String] / endOfSegments,
  )

  val router = new Router[Page](
    routes = List(
      homeRoute,
      viewObjectRoute,
      searchRoute,
      viewDatabaseRoute,
    ),
    getPageTitle = _.toString, // mock page title (displayed in the browser tab next to favicon)
    serializePage = page => page.toJson, // serialize page data for storage in History API log
    deserializePage = pageStr => pageStr.fromJson[Page].getOrElse(HomePage) // deserialize the above
  )(
    popStateEvents = L.windowEvents(_.onPopState), // this is how Waypoint avoids an explicit dependency on Laminar
    owner = L.unsafeWindowOwner // this router will live as long as the window
  )

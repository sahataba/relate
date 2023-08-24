package testvite

import com.raquo.laminar.api._
import com.raquo.laminar.api.L
import com.raquo.waypoint._
import zio.json._

import java.util.UUID

sealed trait Page

object Page:
  case class ViewObject(id: Int) extends Page
  case object HomePage extends Page

  implicit val codec: JsonCodec[Page] = DeriveJsonCodec.gen[Page]

object Router:
  import Page._

  val homeRoute: Route[Page.HomePage.type, Unit] =
    Route.static(HomePage, root / endOfSegments)

  val viewObjectRoute = Route[ViewObject, Int](
    encode = page => page.id,
    decode = arg => ViewObject(arg),
    pattern = root / "view" / segment[Int] / endOfSegments,
  )

  val router = new Router[Page](
    routes = List(
      homeRoute,
      viewObjectRoute,
    ),
    getPageTitle = _.toString, // mock page title (displayed in the browser tab next to favicon)
    serializePage = page => page.toJson, // serialize page data for storage in History API log
    deserializePage = pageStr => pageStr.fromJson[Page].getOrElse(HomePage) // deserialize the above
  )(
    popStateEvents = L.windowEvents(_.onPopState), // this is how Waypoint avoids an explicit dependency on Laminar
    owner = L.unsafeWindowOwner // this router will live as long as the window
  )

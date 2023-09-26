package testvite

import com.raquo.laminar.api.L.{*}


case class AddNewThing(
  something: String,
  toThing: Option[Id | Value]
)
case class AddValue(
  something: String,
  toThing: Option[Id]
)

case class LinkThing(
  id: Id,
  toThing: URI,
)
object Manager:
  def exec(db: Var[Database])(cmd: LinkThing) =
    val newRelations =
      List(
        Relation(
          id = URI.newId(),
          subject = cmd.toThing,
          `object` = cmd.id,
          predicate = Predicate.blank
        )
      )
    db.update(_.saveRelations(newRelations))
  def exec(db: Var[Database])(cmd: AddNewThing) =
    val newtThingId = URI.newId()
    val newRelations =
      List(
        Relation(
          id = URI.newId(),
          subject = newtThingId,
          `object` = Value(cmd.something),
          predicate = Predicate.name
        )
      ) ++ cmd.toThing.map({
        case t: URI => Relation(
          id = URI.newId(),
          subject = t,
          `object` = newtThingId,
          predicate = Predicate.blank
        )
        case t: Value => ???
      }).toList
    db.update(_.saveRelations(newRelations))
  def exec(db: Var[Database])(cmd: AddValue) = 
    val thingId = cmd.toThing match {
      case Some(URI(uri)) => URI(uri)
      case Some(Value(_)) => ???
      case None => URI.newId()
    }
    val newRelations =
      List(
        Relation(
          id = URI.newId(),
          subject = thingId,
          `object` = Value(cmd.something),
          predicate = Predicate.blank
        )
      )
    db.update(_.saveRelations(newRelations))
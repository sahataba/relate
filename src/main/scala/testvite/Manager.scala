package testvite

import com.raquo.laminar.api.L.{*}


case class AddNewThing(
  something: String,
  toThing: Option[Id | Value]
)
object Manager:
  def addNewThing(db: Var[Database])(cmd: AddNewThing) =
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
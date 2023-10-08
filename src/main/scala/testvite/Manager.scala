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
//DefinePredicate
case class SetPredicate(
  relationId: URI,
  predicateId: URI,
)
//DefinePredicateAndExtractObject
case class ExtractObjectSetPredicate(
  relationId: URI,
  predicateId: URI,
)
case class ExtractObjectToObjectWithNewPredicate(
  relationId: URI,
  predicateId: URI,
)
case class MoveObjectToPredicateAndSetObject(
  relationId: URI,
  predicateId: URI,
)
object Manager:
  def exec(dbVar: Var[Database])(cmd: SetPredicate): Unit =
    dbVar.update(db => {
      db.getRelation(cmd.relationId) match {
        case Some(relation) => {
          val newRelation = List(
            Relation(
              id = URI.newId(),
              subject = relation.subject,
              predicate = cmd.predicateId,
              `object` = relation.`object`,
            )
          )
          db.remove(cmd.relationId).saveRelations(newRelation)
        }
        case None => db
      }
    })
  def exec(dbVar: Var[Database])(cmd: ExtractObjectSetPredicate): Unit =
    dbVar.update(db => {
      db.getRelation(cmd.relationId) match {
        case Some(relation) => {
          val newObjectId = URI.newId()
          val newRelations = List(
            Relation(
              id = URI.newId(),
              subject = newObjectId,
              predicate = relation.predicate,
              `object` = relation.`object`,
            ),
            Relation(
              id = URI.newId(),//todo unstable ids. it doesnt matter
              subject = relation.subject,
              predicate = cmd.predicateId,
              `object` = newObjectId,
            )
          )
          db.remove(cmd.relationId).saveRelations(newRelations)
        }
        case None => db
      }
    })
  def exec(dbVar: Var[Database])(cmd: ExtractObjectToObjectWithNewPredicate): Unit =
    dbVar.update(db => {
      db.getRelation(cmd.relationId) match {
        case Some(relation) => {
          val newObjectId = URI.newId()
          val newRelations = List(
            Relation(
              id = URI.newId(),
              subject = newObjectId,
              predicate = cmd.predicateId,
              `object` = relation.`object`,
            ),
            Relation(
              id = URI.newId(),//todo unstable ids. it doesnt matter
              subject = relation.subject,
              predicate = relation.predicate,
              `object` = newObjectId,
            )
          )
          db.remove(cmd.relationId).saveRelations(newRelations)
        }
        case None => db
      }
    })
  def exec(dbVar: Var[Database])(cmd: MoveObjectToPredicateAndSetObject): Unit =
    dbVar.update(db => {
      db.getRelation(cmd.relationId) match {
        case Some(relation) => {
          val newRelations = List(
            Relation(
              id = URI.newId(),
              subject = relation.subject,
              predicate = relation.`object`.asInstanceOf[URI],
              `object` = cmd.predicateId,
            ),
          )
            
          db.remove(cmd.relationId).saveRelations(newRelations)
        }
        case None => db
      }
    })
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
package testvite

import zio.json._

case class URI(value: String)
case class Relation(
    id: URI,
    subject: URI,
    `object`: URI | Value,
    predicate: URI
) //split object field to two fields
case class EditRelation(
    id: URI,
    subject: Option[URI],
    `object`: Option[URI | Value],
    predicate: Option[URI]
) {
  def toRelation: Option[Relation] =
    for {
      subject <- subject
      `object` <- `object`
      predicate <- predicate
    } yield Relation(id, subject, `object`, predicate)//todo fix
}
type Relations = Set[Relation]
type References = Set[Relation]
case class Value(value: String)
type Id = URI | Value
case class Entity(id: URI | Value, relations: Relations, references: References)

object URI:
  given JsonDecoder[URI] = JsonDecoder[String].map(stringToRelationId)
  given JsonEncoder[URI] = JsonEncoder[String].contramap(relationIdToString)
  def newId(): URI = URI("id" + java.util.UUID.randomUUID().toString)

object Id:
  given JsonDecoder[Id] = JsonDecoder[String].map(stringToId)
  given JsonEncoder[Id] = JsonEncoder[String].contramap(idToString)

  //we want to automatically generate new Ids
  //user should never see ids, it sees viewFragment(id), in (3 | 2) tabs Relations/ References/
  //add icon inplace of unredable ids

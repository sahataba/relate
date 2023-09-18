package testvite

import zio.json._

case class URI(value: String)
case class Relation(
    subject: URI,
    `object`: URI | Value,
    predicate: URI
) //split object field to two fields
case class EditRelation(
    id: Int,
    subject: Option[URI],
    `object`: Option[URI | Value],
    predicate: Option[URI]
) {
  def toRelation: Option[Relation] =
    for {
      subject <- subject
      `object` <- `object`
      predicate <- predicate
    } yield Relation(subject, `object`, predicate)
}
type Relations = Set[Relation]
type References = Set[Relation]
case class Value(value: String)
type Id = URI | Value
case class Entity(id: URI | Value, relations: Relations, references: References)

object URI:
  given JsonDecoder[URI] = JsonDecoder[String].map(stringToRelationId)
  given JsonEncoder[URI] = JsonEncoder[String].contramap(relationIdToString)

object Id:
  given JsonDecoder[Id] = JsonDecoder[String].map(stringToId)
  given JsonEncoder[Id] = JsonEncoder[String].contramap(idToString)

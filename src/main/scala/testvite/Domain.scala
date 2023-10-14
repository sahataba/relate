package testvite

import zio.json._

case class URI(value: String)
case class Relation(
    id: URI,
    subject: URI,
    `object`: URI | Value,
    predicate: URI //finally add option to predicate, check rdf spec
) //split object field to two fields

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
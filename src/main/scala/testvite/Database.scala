package testvite
import zio.json._

val values: Map[Id, Value] = Map(
  ValueId(1) -> "person",
  ValueId(2) -> "michael",
  ValueId(3) -> "first name",
  ValueId(4) -> "john",
  ValueId(5) -> "last name",
  ValueId(6) -> "jackson",
  ValueId(7) -> "VAT 768",
  ValueId(8) -> "VAT",
  ValueId(9) -> "9",
  ValueId(10) -> "born",
  ValueId(11) -> "1958",
  ValueId(12) -> "year",
  ValueId(13) -> "born in",
  ValueId(14) -> "musician",
  ValueId(15) -> "basketball player",
  ValueId(16) -> "Sinead",
)

val relations = Set(
  Relation(RelationId(1), ValueId(1), ValueId(3)),
  Relation(RelationId(2), ValueId(1), ValueId(5)),
  Relation(RelationId(3), ValueId(3), ValueId(2)),
  Relation(RelationId(4), ValueId(1), ValueId(3)),
  Relation(RelationId(5), ValueId(3), ValueId(4)),
  Relation(RelationId(6), ValueId(5), ValueId(6)),
  Relation(RelationId(7), ValueId(1), ValueId(7)),
  Relation(RelationId(8), ValueId(1), ValueId(8)),
  Relation(RelationId(9), ValueId(8), ValueId(7)),
  Relation(RelationId(10), ValueId(9), ValueId(1)),
  Relation(RelationId(11), ValueId(9), RelationId(19)),
  Relation(RelationId(12), ValueId(9), RelationId(20)),
  Relation(RelationId(13), ValueId(9), RelationId(21)),
  Relation(RelationId(14), ValueId(9), RelationId(23)),
  Relation(RelationId(15), ValueId(10), ValueId(12)),
  Relation(RelationId(16), ValueId(9), ValueId(14)),
  Relation(RelationId(17), ValueId(9), ValueId(15)),
  Relation(RelationId(1), ValueId(14), ValueId(1)),
  Relation(RelationId(19), ValueId(3), ValueId(2), "which is"),
  Relation(RelationId(20), ValueId(5), ValueId(6)),
  Relation(RelationId(21), ValueId(8), ValueId(7)),
  Relation(RelationId(22), ValueId(11), ValueId(12), "is a"),
  Relation(RelationId(23), ValueId(10) , RelationId(22)),
  Relation(RelationId(24), ValueId(14), ValueId(16)),
)

case class Database(private val values: Map[Id, Value], private val relations: Set[Relation]):
  def search(query: String): List[Entity] =
    values
      .filter((id, value) => value.contains(query))
      .map((id, value) => Entity(
        id,
        value,
        relations.filter(r => r.from == id).toSet,
        relations.filter(r => r.to == id).toSet,
      )).toList
  def get(id: Id): Option[Entity] = values.get(id).map(value => Entity(
        id,
        value,
        relations.filter(r => r.from == id).toSet,
        relations.filter(r => r.to == id).toSet,
      ))
  def getRelation(id: RelationId) = relations.find(r => r.id == id)
  def saveRelations(newRelations: List[Relation]): Database = this.copy(relations = relations.concat(newRelations))
  def remove(id: RelationId): Database = this.copy(relations = relations.filter(r => r.id != id))
  def newRelationId(): RelationId = RelationId(this.relations.map(_.id.value).max + 1)

object Database:
  val dummy = Database(values, relations)
  given JsonDecoder[RelationId | ValueId] = JsonDecoder[String].map(stringToId)
  given JsonEncoder[RelationId | ValueId] = JsonEncoder[String].contramap(idToString)
  given JsonFieldDecoder[RelationId | ValueId] = JsonFieldDecoder[String].map(stringToId)
  given JsonFieldEncoder[RelationId | ValueId] = JsonFieldEncoder[String].contramap(idToString)
  given JsonCodec[Relation] = DeriveJsonCodec.gen[Relation]
  given JsonCodec[Database] = DeriveJsonCodec.gen[Database]
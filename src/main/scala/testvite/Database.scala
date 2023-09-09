package testvite
import zio.json._

case class Database(private val values: Map[ValueId, Value], private val relations: Set[Relation]):
  def getRelations(): List[Relation] = relations.toList
  def search(query: String): List[Entity] =
    values
      .filter((id, value) => value.contains(query))
      .map((id, value) => Entity(
        id,
        value,
        relations.filter(r => r.from == id).toSet,
        relations.filter(r => r.to == id).toSet,
      )).toList
  def get(id: ValueId): Option[Entity] = values.get(id).map(value => Entity(
        id,
        value,
        relations.filter(r => r.from == id).toSet,
        relations.filter(r => r.to == id).toSet,
      ))
  def getRelation(id: RelationId) = relations.find(r => r.id == id)
  def saveRelations(newRelations: List[Relation]): Database = this.copy(relations = relations.concat(newRelations))
  def remove(id: RelationId): Database = this.copy(relations = relations.filter(r => r.id != id))
  def newRelationId(): RelationId = RelationId(scala.util.Try{this.relations.map(_.id.value).max}.getOrElse(0) + 1)
  def newValue(value: Value): (ValueId, Database) = {
    val valueId = ValueId(values.keys.map(_.value).max + 1)
    val g = this.copy(values = values + (valueId -> value))
    (valueId, g)
  }

object Database:
  val empty = Database(Map.empty, Set.empty)
  given JsonDecoder[RelationId | ValueId] = JsonDecoder[String].map(stringToId)
  given JsonEncoder[RelationId | ValueId] = JsonEncoder[String].contramap(idToString)
  given JsonFieldDecoder[RelationId | ValueId] = JsonFieldDecoder[String].map(stringToId)
  given JsonFieldEncoder[RelationId | ValueId] = JsonFieldEncoder[String].contramap(idToString)
  given JsonFieldDecoder[ValueId] = JsonFieldDecoder[String].map(stringToValueId)
  given JsonFieldEncoder[ValueId] = JsonFieldEncoder[String].contramap(valueIdToString)
  given JsonCodec[Relation] = DeriveJsonCodec.gen[Relation]
  given JsonCodec[Database] = DeriveJsonCodec.gen[Database]
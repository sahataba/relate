package testvite
import zio.json._

case class Database(private val values: Map[ValueId, Value], private val relations: Set[Relation]):
  def getRelations(): List[Relation] = relations.toList
  def search(query: String): List[Entity] =
    values
      .filter((id, value) => value.contains(query))
      .map((id, value) => Entity(
        id,
        relations.filter(r => r.subject == id).toSet,
        relations.filter(r => r.`object` == id).toSet,
      )).toList
  def get(id: ValueId): Option[Entity] = values.get(id).map(value => Entity(
        id,
        relations.filter(r => r.subject == id).toSet,
        relations.filter(r => r.`object` == id).toSet,
      ))
  //def getRelation(id: RelationId) = relations.find(r => r.id == id)
  def saveRelations(newRelations: List[Relation]): Database = this.copy(relations = relations.concat(newRelations))
  def remove(id: Relation): Database = this.copy(relations = relations.-(id))
  def newValue(value: Value): (ValueId, Database) = {
    val valueId = ValueId(values.keys.map(_.value).max + 1)
    val g = this.copy(values = values + (valueId -> value))
    (valueId, g)
  }

object Database:
  val empty = Database(Map.empty, Set.empty)
  given JsonDecoder[URI | ValueId] = JsonDecoder[String].map(stringToId)
  given JsonEncoder[URI | ValueId] = JsonEncoder[String].contramap(idToString)
  given JsonFieldDecoder[URI | ValueId] = JsonFieldDecoder[String].map(stringToId)
  given JsonFieldEncoder[URI | ValueId] = JsonFieldEncoder[String].contramap(idToString)
  given JsonFieldDecoder[ValueId] = JsonFieldDecoder[String].map(stringToValueId)
  given JsonFieldEncoder[ValueId] = JsonFieldEncoder[String].contramap(valueIdToString)
  given JsonCodec[Relation] = DeriveJsonCodec.gen[Relation]
  given JsonCodec[Database] = DeriveJsonCodec.gen[Database]
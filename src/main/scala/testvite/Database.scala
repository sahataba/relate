package testvite
import zio.json._

case class Database(private val relations: Set[Relation]):
  def getRelations(): List[Relation] = relations.toList
  def search(query: String): List[Relation] = relations.filter(r => {
    r.`object` match {
      case id: Value => id.value.contains(query)
      case id: URI => id.value.contains(query)
    }
  }).toList
  def get(id: Id): Entity = Entity(
    id = id,
    relations.filter(r => {
      r.`subject` == id
    }),
    relations.filter(r => {
      r.`object` == id
    }))

  //def getRelation(id: RelationId) = relations.find(r => r.id == id)
  def saveRelations(newRelations: List[Relation]): Database = this.copy(relations = relations.concat(newRelations))
  def remove(id: Relation): Database = this.copy(relations = relations.-(id))


object Database:
  val empty = Database(Set.empty)
  given JsonDecoder[URI | Value] = JsonDecoder[String].map(stringToId)
  given JsonEncoder[URI | Value] = JsonEncoder[String].contramap(idToString)
  given JsonFieldDecoder[URI | Value] = JsonFieldDecoder[String].map(stringToId)
  given JsonFieldEncoder[URI | Value] = JsonFieldEncoder[String].contramap(idToString)
  given JsonFieldDecoder[Value] = JsonFieldDecoder[String].map(stringToValueId)
  given JsonFieldEncoder[Value] = JsonFieldEncoder[String].contramap(valueIdToString)
  given JsonCodec[Relation] = DeriveJsonCodec.gen[Relation]
  given JsonCodec[Database] = DeriveJsonCodec.gen[Database]
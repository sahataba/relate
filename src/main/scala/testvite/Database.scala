package relate
import zio.json._

case class Database(private val relations: Set[Relation]):
  def getRelations(): List[Relation] = relations.toList
  def search(query: String): List[Relation] = relations
    .filter(r => {
      r.`object` match {
        case id: Value => id.value.contains(query)
        case id: URI   => id.value.contains(query)
      }
    })
    .toList.sortBy(_.predicate.value)
  def get(id: Id): Entity = Entity(
    id = id,
    relations.filter(r => {
      r.`subject` == id
    }).toList.sortBy(_.predicate.value),
    relations.filter(r => {
      r.`object` == id
    }).toList.sortBy(_.predicate.value)//sortby is in collision with getName
  )

  def getRelation(id: URI) = relations.find(r => r.id == id)
  def saveRelations(newRelations: List[Relation]): Database =
    this.copy(relations = relations.concat(newRelations))
  def remove(id: URI): Database =
    this.copy(relations = relations.filter(r => r.id != id))

object Database:
  val systemRelations =
    Set(
      Relation(
        id = URI("system-name"),
        `subject` = URI("name"),
        `object` = Value("name"),
        predicate = URI("name")
      )
    )
  val initial = Database(systemRelations)
  given JsonDecoder[URI | Value] = JsonDecoder[String].map(stringToId)
  given JsonEncoder[URI | Value] = JsonEncoder[String].contramap(idToString)
  given JsonFieldDecoder[URI | Value] = JsonFieldDecoder[String].map(stringToId)
  given JsonFieldEncoder[URI | Value] =
    JsonFieldEncoder[String].contramap(idToString)
  given JsonFieldDecoder[Value] = JsonFieldDecoder[String].map(stringToValueId)
  given JsonFieldEncoder[Value] =
    JsonFieldEncoder[String].contramap(valueIdToString)
  given JsonCodec[Relation] = DeriveJsonCodec.gen[Relation]
  given JsonCodec[Database] = DeriveJsonCodec.gen[Database]

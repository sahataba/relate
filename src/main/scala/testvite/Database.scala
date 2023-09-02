package testvite

val entities: Map[Id, Value] = Map(
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
  Relation("1", ValueId(1), ValueId(3)),
  Relation("2", ValueId(1), ValueId(5)),
  Relation("3", ValueId(3), ValueId(2)),
  Relation("4", ValueId(1), ValueId(3)),
  Relation("5", ValueId(3), ValueId(4)),
  Relation("6", ValueId(5), ValueId(6)),
  Relation("7", ValueId(1), ValueId(7)),
  Relation("8", ValueId(1), ValueId(8)),
  Relation("9", ValueId(8), ValueId(7)),
  Relation("10", ValueId(9), ValueId(1)),
  Relation("11", ValueId(9), "19"),
  Relation("12", ValueId(9), "20"),
  Relation("13", ValueId(9), "21"),
  Relation("14", ValueId(9), "23"),
  Relation("15", ValueId(10), ValueId(12)),
  Relation("16", ValueId(9), ValueId(14)),
  Relation("17", ValueId(9), ValueId(15)),
  Relation("18", ValueId(14), ValueId(1)),
  Relation("19", ValueId(3), ValueId(2), "which is"),
  Relation("20", ValueId(5), ValueId(6)),
  Relation("21", ValueId(8), ValueId(7)),
  Relation("22", ValueId(11), ValueId(12), "is a"),
  Relation("23", ValueId(10) , "22"),
  Relation("24", ValueId(14), ValueId(16)),
)

case class Database(private val entities: Map[Id, Value], private val relations: Set[Relation]):
  def search(query: String): List[Entity] =
    entities
      .filter((id, value) => value.contains(query))
      .map((id, value) => Entity(
        id,
        value,
        relations.filter(r => r.from == id).toSet,
        relations.filter(r => r.to == id).toSet,
      )).toList
  def get(id: Id): Option[Entity] = entities.get(id).map(value => Entity(
        id,
        value,
        relations.filter(r => r.from == id).toSet,
        relations.filter(r => r.to == id).toSet,
      ))
  def getRelation(id: RelationId) = relations.find(r => r.id == id)
  def remove(id: RelationId): Database = this.copy(relations = relations.filter(r => r.id != id))

object Database:
  val dummy = Database(entities, relations)
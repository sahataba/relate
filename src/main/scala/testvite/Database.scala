package testvite

val entities: Map[Id, Value] = Map(
  1 -> "person",
  2 -> "michael",
  3 -> "first name",
  4 -> "john",
  5 -> "last name",
  6 -> "jackson",
  7 -> "VAT 768",
  8 -> "VAT",
  9 -> "9",
  10 -> "born",
  11 -> "1958",
  12 -> "year",
  13 -> "born in",
  14 -> "musician",
  15 -> "basketball player",
  16 -> "Sinead",
)

val relations = Set(
  Relation("1", 1, 3),
  Relation("2", 1, 5),
  Relation("3", 3, 2),
  Relation("4", 1, 3),
  Relation("5", 3, 4),
  Relation("6", 5, 6),
  Relation("7", 1, 7),
  Relation("8", 1, 8),
  Relation("9", 8, 7),
  Relation("10", 9, 1),
  Relation("11", 9, "19"),
  Relation("12", 9, "20"),
  Relation("13", 9, "21"),
  Relation("14", 9, "23"),
  Relation("15", 10, 12),
  Relation("16", 9, 14),
  Relation("17", 9, 15),
  Relation("18", 14, 1),
  Relation("19", 3, 2, "which is"),
  Relation("20", 5, 6),
  Relation("21", 8, 7),
  Relation("22", 11, 12, "is a"),
  Relation("23", 10 , "22"),
  Relation("24", 14, 16),
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
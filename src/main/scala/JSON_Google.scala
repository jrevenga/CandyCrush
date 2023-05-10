package JSON_Google{

import com.google.cloud.datastore._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization.write

import scala.collection.JavaConverters._

class JSON_Google {

  implicit val formats: DefaultFormats.type = DefaultFormats

  def main(): Unit = {
    val projectId = "subtle-lambda-385422"
    val kind = "Nuevo"
    val data = "data.json"

    // Configurar el cliente de Datastore
    val datastore: Datastore = DatastoreOptions.newBuilder()
      .setProjectId(projectId)
      .build()
      .getService

    // Consultar todas las entidades de un tipo específico
    val query: Query[Entity] = Query.newEntityQueryBuilder().setKind(kind).build()
    val entities: Iterator[Entity] = datastore.run(query, Array[ReadOption](): _*).asScala

    // Convertir las entidades a JSON
    // Convertir las entidades a JSON
    val entitiesJson: List[JValue] = entities.map(entityToJson).filter(_ != JNothing).toList

    val jsonString: String = write(entitiesJson)

    // Guardar el JSON en un archivo
    import java.nio.file.{Files, Paths}
    Files.write(Paths.get("C:\\Users\\PORTATIL\\Desktop\\Pagina\\public\\" + data), jsonString.getBytes)

    println(s"Se han exportado las entidades de $kind en el archivo $data")
  }

  val fieldMapping: Map[String, String] = Map(
    "name" -> "nombre",
    "score" -> "puntuacion",
    "date" -> "fecha",
    "duration" -> "duracion"
  )
  import java.time.{LocalDateTime, ZoneId}
  import java.time.format.DateTimeFormatter

  def entityToJson(entity: Entity): JValue = {
    val properties: Map[String, JValue] = entity.getNames.asScala.map { name =>
      name -> toJValue(entity.getValue(name))
    }.toMap

    val requiredFields = Set("name", "score", "date", "duration")

    if (requiredFields.subsetOf(properties.keySet)) {
      val dateString = properties("date").extract[String]
      val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
      val localDateTime = LocalDateTime.parse(dateString, dateTimeFormatter)
      val epochMillis = localDateTime.atZone(ZoneId.systemDefault()).toInstant.toEpochMilli

      JObject(
        fieldMapping("name") -> properties("name"),
        fieldMapping("score") -> properties("score"),
        fieldMapping("date") -> JLong(epochMillis+7200000),
        fieldMapping("duration") -> properties("duration")
      )
    } else {
      JNothing
    }
  }


  def toJValue(value: Value[_]): JValue = value match {
    case _: NullValue => JNull
    case booleanValue: BooleanValue => JBool(booleanValue.get())
    case longValue: LongValue => JInt(BigInt(longValue.get()))
    case doubleValue: DoubleValue => JDouble(doubleValue.get())
    case stringValue: StringValue => JString(stringValue.get())
    case timestampValue: TimestampValue => JString(timestampValue.get().toString)
    // Añade más tipos si es necesario
    case _ => JNothing
  }
}
}
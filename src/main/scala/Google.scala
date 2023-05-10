package Google {

  import com.google.cloud.datastore.StructuredQuery._
  import com.google.cloud.datastore._

  import java.time.LocalDateTime
  import java.time.format.DateTimeFormatter


  case class GameData(nombre: String, puntuacion: Int, fecha: LocalDateTime, duracion: Long)
class Google{
  def main(nombre:String, puntuacion:Int, duracion:Int): Unit = {
      println("GOOGLE_APPLICATION_CREDENTIALS: " + sys.env("GOOGLE_APPLICATION_CREDENTIALS"))

      val gameData = GameData(nombre, puntuacion, LocalDateTime.now, duracion)
      val alphaNumericString = generateAlphaNumericString(gameData)
      println(alphaNumericString)

      // Guardar la cadena alfanumérica en Google Cloud Datastore
      saveToDatastore(gameData.nombre, gameData.puntuacion, gameData.fecha.format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss")), gameData.duracion.toInt)
    }

    def generateAlphaNumericString(gameData: GameData): String = {
      val dateFormatter = DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
      s"${gameData.nombre}_${gameData.puntuacion}_${gameData.fecha.format(dateFormatter)}_${gameData.duracion}"
    }


    import com.google.cloud.datastore.{Datastore, DatastoreOptions, Entity, KeyFactory}

    def saveToDatastore(name: String, score: Int, date: String, duration: Int): Unit = {
      // Reemplaza "your-project-id" con el ID real de tu proyecto
      val projectId = "subtle-lambda-385422"
      val datastoreOptions = DatastoreOptions.newBuilder().setProjectId(projectId).build()
      val datastore: Datastore = datastoreOptions.getService
      val keyFactory: KeyFactory = datastore.newKeyFactory().setKind("Nuevo")
      val key = datastore.allocateId(keyFactory.newKey())
      val entity = Entity.newBuilder(key)
        .set("name", name)
        .set("score", score)
        .set("date", date)
        .set("duration", duration)
        .build()
      println(s"Saving entity: $entity") // Agrega esta línea para registrar la entidad que se va a guardar
      datastore.put(entity)
      println("Entity saved successfully") // Agrega esta línea para confirmar que la entidad se ha guardado correctamente


    }


  }
}
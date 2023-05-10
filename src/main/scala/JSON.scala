package Json_Amazon{
import java.sql.{Connection, DriverManager, ResultSet}
import play.api.libs.json._
import java.io.File


class JSON_Amazon {
  def main(args: Array[String]): Unit = {
    val endpoint = "candypap.comzgknmdyzd.eu-north-1.rds.amazonaws.com"
    val port = 3306
    val database = "candypap"
    val username = "admin"
    val password = "adminadmin"

    // create a connection to the database
    Class.forName("com.mysql.cj.jdbc.Driver")

    val url = s"jdbc:mysql://$endpoint:$port/$database"
    val connection: Connection = DriverManager.getConnection(url, username, password)

    //Datos a mostrar
    val sql = "SELECT * FROM puntos"
    val statement = connection.createStatement()
    val resultSet: ResultSet = statement.executeQuery(sql)

    //Crear JSON
    var jsonSeq = Seq[JsValue]()
    while (resultSet.next()) {
      val usuario = resultSet.getString("usuario")
      val puntuacion = resultSet.getInt("puntuacion")
      val fecha = resultSet.getDate("fecha")
      val duracion = resultSet.getInt("duracion")

      val puntosJSON = Json.obj(
        "nombre" -> usuario,
        "puntuacion" -> puntuacion,
        "fecha" -> fecha,
        "duracion" -> duracion
      )
      jsonSeq = jsonSeq :+ puntosJSON
    }

    val file = new File("dato.json")
    val jsonValue = Json.toJson(jsonSeq)
    val jsonString = Json.prettyPrint(jsonValue)
    val writer = new java.io.PrintWriter(file)
    writer.write(jsonString)
    writer.close()
  }
}
}
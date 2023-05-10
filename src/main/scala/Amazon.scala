
package Amazon{


import java.sql.SQLException._
import java.sql.{Connection,DriverManager, SQLException}
import java.sql.Connection

class Amazon{
  def main(usuario:String, puntuacion:Int, fecha:String, duracion:Int): Unit = {
    val endpoint = "candypap.comzgknmdyzd.eu-north-1.rds.amazonaws.com"
    val port = 3306
    val database = "candypap"
    val username = "admin"
    val password = "adminadmin"
    // create a connection to the database
    Class.forName("com.mysql.cj.jdbc.Driver")

    val url = s"jdbc:mysql://$endpoint:$port/$database"
    val connection: Connection = DriverManager.getConnection(url, username, password)

    insertarValores(connection: Connection, usuario:String, puntuacion:Int, fecha:String, duracion:Int)

  }

  def insertarValores(connection: Connection, usuario:String, puntuacion:Int, fecha:String, duracion:Int):Unit = {

    // execute a query
    try {
      val statement = connection.createStatement()
      val createTableSQL =
        """CREATE TABLE IF NOT EXISTS puntos (
          |  usuario VARCHAR(50),
          |  puntuacion INT,
          |  fecha DATE,
          |  duracion INT
          |)""".stripMargin
      statement.executeUpdate(createTableSQL)

      // Insertar algunos valores de ejemplo
      val insertSQL =
        """INSERT INTO puntos (usuario, puntuacion, fecha, duracion)
          |VALUES ('%s', %d, '%s', %d)"""
          .stripMargin.format(usuario, puntuacion, fecha, duracion)
      statement.executeUpdate(insertSQL)
    } catch {
      case e: SQLException =>
        println("Error al ejecutar la consulta:")
        e.printStackTrace()
    }
    finally {
      connection.close()
    }
  }
}
}
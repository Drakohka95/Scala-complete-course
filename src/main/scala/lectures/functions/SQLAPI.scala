package lectures.functions

/**
  * Представим себе, как бы мог выглядеть API для работы, например, с БД
  * Строить методы этого API будем через композицию уже определенных методов.
  *
  * Ваша задача реализовать метод execute, композируя методы из объекта SQLAPI
  * Метод execute из объекта SQLAPI должен выполнить следующие действия
  * * * * * залогировать ресурс
  * * * * * получить соединение с помощью метода connection
  * * * * * залогировать запрос
  * * * * * открыть соединение
  * * * * * выполнить SQL
  * * * * * залогировать результат
  *
  * В результате в консоль должно быть выведено сообщение
  *    some DB
  *    some SQL
  *    SQL has been executed. Congrats!
  *
  *
  * Обратите внимание на то, что композиция функций учит писать код в декларативном виде
  * Благодаря этому мы можем отделить реализацию методов от их применения и, в конечном итоге, иметь переиспользуемую
  * декларацию, которую можно применить, например, для настоящей БД
  *
  *
  */
class SQLAPI(resource: String) {

  case class Connection(resource: String, opened: Boolean = false) {

    private val result = "SQL has been executed. Congrats!"

    def open(): Connection = this.copy(opened = true)

    def execute(sql: String): String = if (opened) result else throw new Exception("You have to open connection before execute")

  }

  private def logParameter(prm: String): String  = {
    println(prm)
    prm
  }

  val connection = (resource: String) => Connection(resource)

 def execute(sql: String): String =  {
  // val res1  = connection (logParameter(resource))
   //val res1do = (connection compose logParameter) (resource)
  // val res2  = openConnection(connection (logParameter(resource))) (logParameter(sql))
  // val res21do  = openConnection((connection compose logParameter) (resource)) (logParameter(sql))
  // val res22do  = (connection compose logParameter andThen openConnection) (resource) (logParameter(sql))
   val res3 = logParameter(openConnection((connection compose logParameter) (resource)) (logParameter(sql)))
   val res3do = ((connection compose logParameter andThen openConnection) andThen logParameter ) (resource) (logParameter(sql))

  // val resfin = (connection andThen openConnection)(resource)(sql) //andThen logParameter _

   //logParameter(res2)
   "test"
 }
  // logParameter(openConnection(connection(sql))(sql))//(openConnection compose connection) (sql)




  def openConnection(connection: Connection): (String) => String =
    (sql: String) => {
      connection.open execute sql
  }

   new ((String) => Int) {
     override def apply(v1: String): Int = 1
   }
}

object SQLCheck extends App {

  new SQLAPI("some DB").execute("some SQL")

}

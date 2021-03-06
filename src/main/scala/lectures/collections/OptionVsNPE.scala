package lectures.collections

import scala.util.Random

/**
  * В этом задании Вам предстоит работать с очень нестабильным внешним сервисом.
  *
  * Для успешного завершения задания вы должны реализовать метод businessLogic в объекте OptionVsNPE
  * Этот метод должен делать следующее:
  * * * * * Получить и распечатать результат или, если была ошибка ResourceException,
  * распечатать "Try again with new resource" и повторить все заново
  * * * * * Получить ресурс через ResourceProducer
  * * * * * Если ресурс не получен, кидать ResourceException (throw new ResourceException)
  * * * * * Если ресурс удачно получен, на его основе получить Connection
  * * * * * Если соединение не получено, пробовать, пока соединение не будет получено
  * * * * * Вызвать у соединения метод result()
  * * * * * Если метод result возвращает Null, заменить его дефолтным сообщением из объекта Connection
  *
  * Для успешного решения задания:
  * * * * * Замените знаки вопроса реализацией методов
  * * * * * Нельзя менять содержимое объектов ConnectionProducer, ResourceProducer
  * * * * * Не должно быть ни одного NPE
  * * * * * Можно менять входные и выходные параметры методов Connection
  *
  * трейт FailUtil - содержит методы для эмуляции спорадических ошибок
  *
  *
  */
class ResourceException extends Exception("Ресурс не отвечает")

//лучше не трогать, модулирует ошибки
trait FailUtil {
  val failRate: Double

  def timeToFail = Math.random() > failRate
}

//не трогать
object ResourceProducer extends FailUtil {
  def produce = if (timeToFail) null else Resource(Random.alphanumeric.take(10).mkString)

  val failRate: Double = 0.3
}

//не трогать
object ConnectionProducer extends FailUtil {
  val failRate: Double = 0.5

  def produce(resource: Resource) = if (timeToFail) null else Connection(resource)

  def result(connection: Connection) = if (timeToFail) null else connection.resource.name
}

//можно менять входные и выходные данные
case class Connection(resource: Resource) {
  private val defaultResult = "something went wrong!"

  //ConnectionProducer.result(this)
  private val newResult = ConnectionProducer.result(this)
  def result(): String = Option(newResult).getOrElse(defaultResult)
}

case class Resource(name: String)

object OptionVsNPE extends App {
  //реализовать его
  def businessLogic: String = try {
    // ResourceProducer
    val newResource =
      Option(ResourceProducer.produce).getOrElse(throw new ResourceException)

    val newConnection = {
      Option(Connection(newResource)).getOrElse(Connection(newResource))
//      val connection = Connection(newResource)
//      if (connection != null) connection
//      else Connection(newResource)
    }

    val result: String = newConnection.result()
    println(result)
    result
  } catch {
    case e: ResourceException => {
      println("Try again with new resource")
      businessLogic
    }
  }

  businessLogic
}

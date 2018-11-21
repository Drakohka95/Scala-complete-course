package lectures.collections.comprehension

/**
  * Помогите курьерам разобраться с обслуживанием адресов
  *
  * Каждый день на работу выходит 'courierCount' курьеров
  * Им нужно обслужить 'addressesCount' адресов
  * Каждый курьер может обслужить courier.canServe адресов, но только при условии, что позволит дорожная ситуация.
  * Т.е. если trafficDegree < 5, то курьер обслужит все адреса, которые может, иначе - ни одного
  *
  * Входные данные для приложения содержат 2 строки
  * В первой строке - количество адресов, которые требуется обслужить
  * Во второй - количество курьеров, вышедших на работу.
  *
  * Ваша задача:
  *  Изучить код и переписать его так,
  *  что бы в нем не было ни одного цикла for, ни одной переменной или мутабильной коллекции
  *
  * Для этого используйте функции комбинаторы: filter, withFilter, fold, map, flatMap и т.д.
  *
  */

case class Traffic(degree: Double)

object Courier {
  def couriers(courierCount: Int): List[Courier] =
    (for (i <- 1 to courierCount) yield {
      Courier(i)
    }).toList
}

case class Courier(index: Int) {
  val canServe = (Math.random() * 10).toInt
}

object Address {
  def addresses(addressesCount: Int): List[Address] =
    (for (i <- 1 to addressesCount) yield {
      Address(s"$i$i$i")
    }).toList
}

case class Address(postIndex: String)

object CouriersWithComprehension extends App {

  import Address._
  import Courier._

  val sc = new java.util.Scanner(System.in)
  val addressesCount = sc.nextInt()
  val courierCount = sc.nextInt()
  val addrs = addresses(addressesCount)
  val cours = couriers(courierCount)

  // какие адреса были обслужены
  def serveAddresses(addresses: List[Address], couriers: List[Courier]) = {
    //    var accum = 0
    //        for (courier <- couriers;
    //             tr9afficDegree = traffic().degree;
    //             t <- 0 until courier.canServe if trafficDegree < 5 && accum < addresses.length
    //        ) yield {
    //          val addr = addresses(accum)
    //          accum = accum + 1
    //          addr
    //        }
    def min (a: Int, b:Int): Boolean = {if (a<b) true else false}
    def doWork(i: Int, n:Int): List[Address] = {
      if (min (n + couriers(i).canServe, addresses.size)) {
        if (min (i+2, couriers.size))
          addresses.slice(n, n + couriers(i).canServe) ++ doWork(i+1, n + couriers(i).canServe)
        else addresses.slice(n, n + couriers(i).canServe)
      }
      else addresses.slice(n, addresses.size)
    }
    doWork (0,0)
  }

  def traffic(): Traffic = new Traffic(Math.random() * 10)

  def printServedAddresses(addresses: List[Address], couriers: List[Courier]) = {
    serveAddresses(addresses, couriers).foreach(serve => println(serve.postIndex))
  }

  printServedAddresses(addrs, cours)
}

package lectures.cat

object Result {
  def bounce[T](t: Result[T]): T = t match {
    case Data(d: T)=> d
    case c: Call[T] => bounce(c.call())
  }

  def step[T](c: Call[T]): Result[T] = {
    c.call()
  }
}

trait Result[T]
case class Data[T](data: T) extends Result[T]
case class Call[T](call: () => Result[T]) extends Result[T]

class Operations(val name: String)

object EratosthenesApp extends App {

  import Result._

  def eratosthenes(numSeq: Seq[Int], n: Integer): Result[Seq[_]] = {
    if (numSeq.isEmpty || numSeq.last == n ) Data(numSeq)
    else Call(() => eratosthenes(numSeq.filterNot(i => i%n == 0 && i != n ), n + 1))
  }

  val testSeq = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 11, 12, 23)

  val c = Call { () => eratosthenes(testSeq, 2)}

  val r = bounce(c)

  print(r)
}
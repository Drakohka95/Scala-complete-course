package lectures.matching

import lectures.matching.SortingStuff._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random


/**
  * Короткий список самых востребованных генераторов:
  * Gen.alphaString
  * Gen.delay
  * Gen.oneOf
  * Gen.resultOf
  * Gen.zip
  * Gen.map
  * Gen.suchThat
  * Gen.mapOf
  * Gen.pic
  * Gen.choose
  *
  * Допишите 2 теста:
  * Для "find knife" теста создайте генератор, Option[Knife]. Тест должен показать, что если нож есть в вещах,
  * то метод findMyKnife его отыщет.
  *
  * Для "put boots ..." создайте генератор и проверьте правильность работы метода sortJunk по аналогии с предыдущими тестами.
  *
  */

class SortingStuffGeneratorBasedTest extends WordSpec with Matchers with PropertyChecks {

  val cheepWatchGen: Gen[Watches] = Gen.zip(Gen.choose(0f, 1000f), Gen.alphaStr).map(w => Watches(w._2, w._1))
  val bookGenerator: Gen[Book] = Gen.alphaStr.map(name => Book(name, Random.nextBoolean()))
  val interestingBookGen: Gen[Book] = bookGenerator.filter(_.isInteresting)
  val maybeKnife = Gen.oneOf(Some(Knife), None)
  val firm = Gen.oneOf((Gen.alphaStr.sample.getOrElse("China")), "Adidas", "Converse" )
  val allBoots: Gen[Boots] = Gen.zip(Gen.choose(0, 1000), firm).map(w => Boots(w._2, w._1))
  // Override configuration if you need
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSize = 10)

  val get: AfterWord = new AfterWord("have")

  "This test" should get {
    "proper cheep watch generator" in {
      forAll(cheepWatchGen) { (watch: Watches) => {
        watch.cost should be <= 1000f
      }
      }
    }
    "proper interesting book generator" in {
      val books = interestingBookGen
      forAll(books) { (book: Book) => {
        book shouldBe 'interesting
      }
      }
    }
  }

  "Sort stuff" should {
    "return collections" which {
      "total size is equal to item amount" in {
        val ms = generatorDrivenConfig.minSuccessful

        val books = (1 to ms) flatMap { _ => interestingBookGen.sample }
        val watches = (1 to ms) flatMap { _ => cheepWatchGen.sample }

        val StuffBox(goodBooks, niceWatches, _, junk) = SortingStuff.sortJunk(Random.shuffle(books ++ watches).toList)
        goodBooks should have size books.size
        niceWatches should have size 0
        junk should have size watches.size
      }
    }
    "find knife" which {
      "was occasionally disposed" in {
        val ms = generatorDrivenConfig.minSuccessful

        val books = (1 to ms) flatMap { _ => interestingBookGen.sample }
        val watches = (1 to ms) flatMap { _ => cheepWatchGen.sample }
        forAll(maybeKnife){(knifeOpt: Option[Knife.type]) =>
          val StuffBox(goodBooks, niceWatches, goodBoots, junk) = SortingStuff.sortJunk(Random.shuffle(books ++ watches ++ knifeOpt.toList).toList)
           SortingStuff.knifeIsInJunk shouldBe knifeOpt.isDefined
        }
      }
    }

    "put boots in a proper place" when {
      "boots were produced by Converse or Adidas" in {
        val ms = generatorDrivenConfig.minSuccessful

        val books = (1 to ms) flatMap { _ => interestingBookGen.sample }
        val watches = (1 to ms) flatMap { _ => cheepWatchGen.sample }
        val boots = (1 to ms) flatMap { _ => allBoots.sample }

        val StuffBox(goodBooks, niceWatches, goodBoots, junk) = SortingStuff.sortJunk(Random.shuffle(books ++ watches ++ boots).toList)
        goodBoots.size shouldBe boots.filter(b => (b.brand == "Converse" || b.brand == "Adidas")).size
      }
    }
  }

  //pendingUntilFixed
}

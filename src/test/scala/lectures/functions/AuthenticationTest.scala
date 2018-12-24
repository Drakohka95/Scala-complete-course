package lectures.functions

import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Gen
import lectures.functions.Authentication._
import lectures.functions.AuthenticationData.{cardUserCreds, cardUserCreds2, authUserCreds, authUserCreds2}
import org.scalatest.prop.PropertyChecks

import scala.util.Random

/**
  * Авторизация - это очень важно, поэтому нам необходимо покрыть тестами ответсвенный за нее код
  * (lectures.functions.Authentication)
  *
  * Для этого
  * * * * уберите extends App у Authentication
  * * * * замените AuthenticationData.testUsers соответствующими генераторами
  * * * * напишите
  * * * * * 2 теста на authByCard
  * * * * * 2 теста на authByLP
  * * * * * 1 тест на их композицию
  *
  */
class AuthenticationTest extends WordSpec with Matchers with PropertyChecks{

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSize = 10)

  val trashUser: Gen[User] = Gen.oneOf(AnonymousUser(), CardUser(), LPUser())
  val trashUserWithoutCardUser: Gen[User] = Gen.oneOf(AnonymousUser(), LPUser())
  val trashUserWithoutLPUser: Gen[User] = Gen.oneOf(AnonymousUser(), CardUser())

  "authByCard" should {
    "find user" in {
      val ms = generatorDrivenConfig.minSuccessful
      val testUser: List[User] = ((1 to ms) flatMap { _ => trashUser.sample }).toList
      val trueUser: List[User] = List(CardUser(1111, cardUserCreds), CardUser(2222, cardUserCreds2))

     (Random.shuffle(testUser ++ trueUser)).flatMap {u => authByCard.lift(u)}.nonEmpty shouldBe true
    }
    "not find user" in {
      val ms = generatorDrivenConfig.minSuccessful
      val testUser: List[User] = ((1 to ms) flatMap { _ => trashUserWithoutCardUser.sample }).toList

      testUser.flatMap {u => authByCard.lift(u)} shouldBe empty
    }
  }

  "authByLP" should {
    "not(!!!) find user" in {
      val ms = generatorDrivenConfig.minSuccessful
      val testUser: List[User] = ((1 to ms) flatMap { _ => trashUser.sample }).toList
      val notTrueUser: List[User] = List(LPUser(1111, authUserCreds), LPUser(2222, authUserCreds2))

      (Random.shuffle(testUser ++ notTrueUser)).flatMap {u => authByLP.lift(u)}.nonEmpty shouldBe false
    }
    "not find user" in {
      val ms = generatorDrivenConfig.minSuccessful
      val testUser: List[User] = ((1 to ms) flatMap { _ => trashUserWithoutLPUser.sample }).toList

      testUser.flatMap {u => authByLP.lift(u)} shouldBe empty
    }

    "composition of authByCard and authByLP" should {
      "contain the same user" in {
        val ms = generatorDrivenConfig.minSuccessful

        val testUser: List[User] = ((1 to ms) flatMap { _ => trashUser.sample }).toList
        val trueUserCard: List[User] = List(CardUser(1111, cardUserCreds), CardUser(2222, cardUserCreds2))
        val notTrueUserLP: List[User] = List(LPUser(3333, authUserCreds), LPUser(4444, authUserCreds2))

        val allTestUser = Random.shuffle(testUser ++ trueUserCard++notTrueUserLP)

        allTestUser.flatMap { u => (authByCard.orElse(authByLP)).lift(u) } should contain theSameElementsAs allTestUser.flatMap { u => (authByLP.orElse(authByCard)).lift(u) }
      }
    }

  }
}

package util

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import scala.util.Random
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalatestplus.play.PlaySpec
import models.AdType
import models.AdType._
import play.api.libs.json.JsValue
import play.api.mvc.Result
import play.api.test._
import play.api.test.Helpers._
import play.api.http.Writeable
import play.api.Application

object TestUtil extends PlaySpec {

  def sendFakeRequestAndMapResult[T](req: FakeRequest[T], f: (Future[Result] => Unit)*)(implicit w: Writeable[T], app: Application) = {
    route(app, req).map(result => f.foreach(_(result)))
  }

  val loginRequest = (userLoginJson: JsValue) => FakeRequest(POST, "/users/login").withJsonBody(userLoginJson)
  val sessionMustContainKV = (key: String, value: String) => (f: Future[Result]) => session(f).get(key) mustBe Some(value)
  val statusMustBe = (status: Int) => (f: Future[Result]) => Await.result(f, Duration.Inf).header.status mustBe status
  val redirectLocationMustBeSome = (location: String) => (f: Future[Result]) => redirectLocation(f) mustBe Some(location)
  val flashMustBeSome = (flashName: String, flashContent: String) => (f: Future[Result]) => flash(f).get(flashName) mustBe Some(flashContent)
  val contentMustInclude = (includee: String) => (f: Future[Result]) => contentAsString(f) must include(includee)

  private val specialChars: Seq[Char] = Array('!', '@', '#', '$', '%', '^', '&', '*', '(', ')')
  def generateUpToElements[A] = (gen: Gen[A], upperBound: Int) =>
    Gen.oneOf(1 to upperBound).flatMap { n =>
      Gen.listOfN(n, gen).flatMap { e => (e, n) }
    }

  def createGeneratorFromSequenceAndSize[A](generators: Seq[Gen[A]], min: Int, max: Int, f: (Gen[A], Int) => Gen[(List[A], Int)]): Gen[Seq[A]] = {
    val start = min - generators.size + 1
    val ending = max - generators.size + 1
    def recHelper(recGenerators: Seq[Gen[A]], accu: Seq[A], upperBound: Int, upperBoundSubtraction: Int): Gen[Seq[A]] =
      recGenerators match {
        case x :: xs if (x :: xs == generators) => Gen.choose(start, ending).flatMap(f(x, _))
          .flatMap { case (elements, n) => recHelper(xs, accu ++ elements, upperBound + 1, upperBoundSubtraction + n) }
        case x :: xs => f(x, upperBound - upperBoundSubtraction)
          .flatMap { case (elements, n) => recHelper(xs, accu ++ elements, upperBound + 1, upperBoundSubtraction + n) }
        case Nil => accu
      }
    recHelper(generators, Nil, ending, 0).suchThat(_.length >= min)
  }

  def properPasswordsUpTo(maxSize: Int): Gen[String] = {
    val generators: Seq[Gen[Char]] = List(Gen.alphaLowerChar, Gen.alphaUpperChar, Gen.numChar, Gen.oneOf(specialChars))
    createGeneratorFromSequenceAndSize(generators, 8, maxSize, generateUpToElements)
      .flatMap(Random.shuffle(_).mkString)
  }

  val properEmails: Gen[String] = Gen.alphaStr
    .suchThat(email => email.length > 0)
    .flatMap(_ + "@test.com")

  val properNames: Gen[String] = Gen.alphaStr

  def enumGen[T <: Enumeration](enum: T): Gen[enum.Value] = Gen.oneOf(enum.values.toSeq)

}
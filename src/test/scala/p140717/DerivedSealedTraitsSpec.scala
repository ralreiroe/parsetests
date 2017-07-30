package p140717

import org.scalatest.{FlatSpec, Matchers}
import julienrf.json.derived
import play.api.libs.json._


class DerivedSealedTraitsSpec extends FlatSpec with Matchers {

  "derived writes" should "work for Bar" in {
    case class Bar(s: String, i: Int)

//    val writes1: Writes[Bar] = derived.owrites()
//
//    println(Json.toJson(Bar("quux", 42))(writes1))

    val fooOWrites: OWrites[Bar] =
      derived.flat.owrites((__ \ "type").write[String])

    println(Json.toJson(Bar("quux", 44))(fooOWrites))

  }



  "derived writes" should "work for Foo" in {
    sealed trait Foo
    case class Bar(s: String, i: Int) extends Foo
    case object Baz extends Foo

    val writes1: Writes[Foo] = derived.owrites()

    println(Json.toJson(Bar("quux", 42))(writes1))

    val fooOWrites: OWrites[Foo] =
      derived.flat.owrites((__ \ "type").write[String])

    println(Json.toJson(Bar("quux", 42))(fooOWrites))

  }



  "derived writes" should "work for sealed traits" in {

    sealed trait PresentationHintSumType

    case object CollapseGroupUnderLabel extends PresentationHintSumType
    case object SummariseGroupAsGrid extends PresentationHintSumType

    implicit val writes: Writes[PresentationHintSumType] = derived.owrites()

    println(Json.toJson(CollapseGroupUnderLabel))
    println(Json.toJson(List(CollapseGroupUnderLabel, SummariseGroupAsGrid)))

    val res = Json.toJson(CollapseGroupUnderLabel)

    res.toString() shouldBe("""{"CollapseGroupUnderLabel":{}}""")
    res match {
      case a@JsObject(_) => a.keys shouldNot (be(empty))
      case _ => fail("expected JsObject")
    }
  }

  class PresentationHint      //<==== changed from sealed trait

  "derived" should "not work for" in {

    "val s: String = 1" shouldNot compile

    case object CollapseGroupUnderLabel extends PresentationHint()
    case object SummariseGroupAsGrid extends PresentationHint()

    implicit val writes: Writes[PresentationHint] = derived.owrites()

    println(Json.toJson(CollapseGroupUnderLabel))

    val res = Json.toJson(Json.toJson(CollapseGroupUnderLabel))

    res.toString() shouldBe("""{}""")


    res match {
      case a@JsObject(_) => a.keys shouldBe (empty)
      case _ => fail("expected JsObject")
    }

  }

}

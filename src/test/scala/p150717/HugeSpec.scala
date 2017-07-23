package p150717

import org.scalatest._
import play.api.libs.json._

class HugeSpec extends FlatSpec with Matchers {
  "json" should "parse into Huge" in {

    import play.api.libs.json._
    import play.api.libs.functional.syntax._

    // Let's pretend this is huge:
    case class Huge(a: Int, b: String, c: Boolean, d: List[Int])

    object Huge {
      val fields1to2: Reads[(Int, String)] = (
        (__ \ "a").read[Int] and
          (__ \ "b").read[String]
        ).tupled

      val fields3to4: Reads[(Boolean, List[Int])] = (
        (__ \ "c").read[Boolean] and
          (__ \ "d").read[List[Int]]
        ).tupled

      val f: ((Int, String), (Boolean, List[Int])) => Huge = {
        case ((a, b), (c, d)) => Huge(a, b, c, d)
      }

      implicit val hugeCaseClassReads: Reads[Huge] = (
        fields1to2 and fields3to4
        ) {
        f
      }

    }
    val jsonStr =
      """
      {
        "a": 3,
        "b": "gid",
        "c": true,
        "d": [4,5]
      }
    """


    val res = Json.parse(jsonStr).validate[Huge]

    res match {
      case s: JsSuccess[Huge] => println(s.get)
      case e: JsError => println(e)
    }
  }


}

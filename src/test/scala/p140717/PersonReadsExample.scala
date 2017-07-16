package p140717

import org.scalatest._

/**
  *
  * https://www.playframework.com/documentation/2.4.x/ScalaJsonInception
  * http://blog.jaceklaskowski.pl/2014/09/02/json-in-play-framework-with-jsvalue-and-reads.html
  */
class PersonReadsExample extends FlatSpec with Matchers {
  "json" should "parse into Person object" in {

    import play.api.libs.json._
    import play.api.libs.functional.syntax._


    case class Person(name: String, age: Int, lovesChocolate: Boolean)

    val jsonStr = """{ "name" : "Jacek", "age" : 41, "lovesChocolate": true }"""

    val json = play.api.libs.json.Json.parse(jsonStr)

    implicit val personReads = (
      (__ \ 'name).read[String] and
        (__ \ 'age).read[Int] and
        (__ \ 'lovesChocolate).read[Boolean]
      )(Person)

//    implicit val personReads = Json.reads[Person]   //gives error: related? : https://github.com/playframework/playframework/issues/1469

    val jacek: Person = json.as[Person]

    println(jacek)
  }
}

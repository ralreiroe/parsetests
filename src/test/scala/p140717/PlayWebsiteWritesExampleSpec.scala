package p140717

import org.scalatest._

case class Location(lat: Double, long: Double)
case class Resident(name: String, age: Int, role: Option[String])
case class Place(name: String, location: Location, residents: Seq[Resident])

class PlayWebsiteWritesExampleSpec extends FlatSpec with Matchers {

  import play.api.libs.json._

  implicit val locationWrites = new Writes[Location] {
    def writes(location: Location) = Json.obj(
      "latitude" -> location.lat,
      "longitude" -> location.long
    )
  }

  implicit val residentWrites = new Writes[Resident] {
    def writes(resident: Resident) = Json.obj(
      "name" -> resident.name,
      "age" -> resident.age,
      "role" -> resident.role
    )
  }

  implicit val placeWrites = new Writes[Place] {
    def writes(place: Place) = Json.obj(
      "name" -> place.name,
      "location" -> place.location,
      "residents" -> place.residents
    )
  }

  val place = Place(
    "Watership Down",
    Location(51.235685, -1.309197),
    Seq(
      Resident("Fiver", 4, None),
      Resident("Bigwig", 6, Some("Owsla"))
    )
  )

  "Place" should "convert into Json" in {

    val json = Json.toJson(place)

    println(json)


    val json2: JsValue = Json.obj(
      "name" -> "Watership Down",
      "location" -> Json.obj("latitude" -> 51.235685, "longitude" -> -1.309197),
      "residents" -> Json.arr(
        Json.obj(
          "name" -> "Fiver",
          "age" -> 4,
          "role" -> JsNull
        ),
        Json.obj(
          "name" -> "Bigwig",
          "age" -> 6,
          "role" -> "Owsla"
        )
      )
    )

    json shouldBe (json2)

    println(json)

  }

}

package p240717

import org.scalacheck.{Prop, Properties}
import org.scalacheck.Prop.forAll
import org.scalacheck.Test.Parameters
import play.api.libs.json._


object DumbJsonParseScalaCheckExample extends  Properties("uuu") {

  override def overrideParameters(p: Parameters) =
    p.withMinSuccessfulTests(10)



  property("double reverse") = forAll { (lst: List[Int]) =>
    lst.reverse.reverse == lst
  }


  case class FieldValueRaw(
                            id: String,
                            label: String,
                            format: Option[String] = None,
                            helpText: Option[String] = None,
                            optionHelpText: Option[List[String]] = None,
                            submitMode: Option[String] = None,
                            choices: Option[List[String]] = None,
                            fields: Option[List[FieldValueRaw]] = None,
                            mandatory: Option[String] = None,
                            multivalue: Option[String] = None,
                            total: Option[String] = None,
                            international: Option[String] = None,
                            infoText: Option[String] = None,
                            infoType: Option[String] = None,
                            shortName: Option[String] = None,
                            repeatsMax: Option[Int] = None,
                            repeatsMin: Option[Int] = None,
                            repeatLabel: Option[String] = None,
                            repeatAddAnotherText: Option[String] = None
                          )


  object FieldValueRaw {

    import play.api.libs.functional.syntax._

    implicit val fromJson: Reads[FieldValueRaw] = (
      (__ \ 'id).read[String] and
        (__ \ 'label).read[String] and
        (__ \ 'format).readNullable[String] and
        (__ \ 'helpText).readNullable[String] and
        (__ \ 'optionHelpText).readNullable[List[String]] and
        (__ \ 'submitMode).readNullable[String] and
        (__ \ 'choices).readNullable[List[String]] and
        (__ \ 'fields).lazyReadNullable(implicitly[Reads[List[FieldValueRaw]]]) and //Note: recursiveness here prevents macro use (see JsonParseTestGroup)
        (__ \ 'mandatory).readNullable[String] and
        (__ \ 'multivalue).readNullable[String] and
        (__ \ 'total).readNullable[String] and
        (__ \ 'international).readNullable[String] and
        (__ \ 'infoText).readNullable[String] and
        (__ \ 'infoType).readNullable[String] and
        (__ \ 'shortName).readNullable[String] and
        (__ \ 'repeatsMax).readNullable[Int] and
        (__ \ 'repeatsMin).readNullable[Int] and
        (__ \ 'repeatLabel).readNullable[String] and
        (__ \ 'repeatAddAnotherText).readNullable[String]
      ) (FieldValueRaw.apply _)
  }

  val jsonStr =
    """
      {
        "type": "group",
        "id": "gid",
        "label": "glabel",
        "format" : "horizontal",
        "repeatsMin":1,
        "repeatsMax":5,
        "repeatLabel":"repeatLabel",
        "repeatAddAnotherText":"repeatAddAnotherText",
        "fields": [
          {
            "type": "choice",
            "id": "cid",
            "label": "clabel",
            "choices": [
              "A",
              "B"
            ]
          }
        ]
      }
    """


  val res = Json.parse(jsonStr).validate[FieldValueRaw]

  property("json parse fail for any repeatsMin not a number") = Prop.forAll(org.scalacheck.Gen.oneOf(List("\"123\"", "{}", "true", "false"))) { (s: String) =>

    import play.api.libs.json.{Json, Reads, __}

    val res = Json.parse(jsonStr.replaceAll("1", s)).validate[FieldValueRaw]
    println(s)
    res.isInstanceOf[JsError]

  }

}

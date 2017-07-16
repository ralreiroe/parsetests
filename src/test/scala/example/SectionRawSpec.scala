package example

import org.scalatest._
import play.api.libs.json._

/** like FieldValueRawSpec but with enclosing Section */
class SectionRawSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {

    sealed trait FormatExpr
    final case class OrientationFormat(value: String) extends FormatExpr

    object FormatExpr {
      implicit val format: OFormat[FormatExpr] = {
        val reads: Reads[FormatExpr] = Reads {
          case JsString(formatAsStr) =>
            FormatParser.validate(formatAsStr) match {
              case Right(expr) => JsSuccess(expr)
              case Left(error) => JsError(error.toString)
            }
          case otherwise => JsError(s"Invalid format expression. Expected String, got $otherwise")
        }

        OFormat[FormatExpr](reads, format)
      }
    }

    sealed trait UnexpectedState extends Product with Serializable
    case class InvalidState(errorMsg: String) extends UnexpectedState
    case class InvalidStateWithJson(errorMsg: String, json: JsValue) extends UnexpectedState


    type Opt[A] = Either[UnexpectedState, A]

    import cats.Eval
    import cats.data.ReaderT
    import cats.instances.either._
    import cats.syntax.either._
    import parseback._
    import parseback.compat.cats._
    import parseback.util.Catenable

    object FormatParser {

      private def parse[A](parser: Parser[A]) = ReaderT[Opt, String, Catenable[A]] { expression =>
        parser(LineStream[Eval](expression)).value.leftMap { error =>
          val errors: String = error.map(_.render(expression)).mkString("\n")
          InvalidState(
            s"""|Unable to parse expression $expression.
                |Errors:
                |$errors""".stripMargin
          )
        }
      }

      private def reconstruct[A](cat: Catenable[A]) = ReaderT[Opt, String, A] { expression =>
        cat.uncons match {
          case Some((expr, _)) => Right(expr)
          case None => Left(InvalidState(s"Unable to parse expression $expression"))
        }
      }


      def validateWithParser[A](expression: String, parser: Parser[A]): Opt[A] = (for {
        catenable <- parse(parser)
        expr <- reconstruct(catenable)
      } yield expr).run(expression)


      def validate(expression: String): Opt[FormatExpr] = validateWithParser(expression, expr)

      lazy val expr: Parser[FormatExpr] = (
        anyWordExpression
        )

      lazy val anyWordExpression: Parser[FormatExpr] = (
        anyWordFormat ^^ { (loc, anyWord) => OrientationFormat(anyWord) }
        )

      val anyWordFormat = """\w+""".r
    }

    case class FieldId(value: String)

    object FieldId {

      val writes = Writes[FieldId](id => JsString(id.value))
      val reads = Reads[FieldId] {
        case JsString(value) => JsSuccess(FieldId(value))
        case otherwise => JsError(s"Invalid fieldId, expected JsString, got: $otherwise")
      }

      implicit val format = Format[FieldId](reads, writes)
    }

    case class FieldValueRaw(
      id: FieldId,
      label: String,
      format: Option[FormatExpr] = None,
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

      implicit val format: Reads[FieldValueRaw] = (
        (__ \ 'id).read[FieldId] and
          (__ \ 'label).read[String] and
          (__ \ 'format).readNullable[FormatExpr] and
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

    case class SectionRaw(title: String, fields: List[FieldValueRaw])

    object SectionRaw {

      import play.api.libs.functional.syntax._

      implicit val reads: Reads[SectionRaw] = (
        (__ \ 'title).read[String] and
          (__ \ 'fields).read[List[FieldValueRaw]]
        )(SectionRaw.apply _)

    }

    val jsonStr =
      """
      {
        "title": "section title",
        "fields": [
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
         ]
      }
    """


    val res = Json.parse(jsonStr).validate[SectionRaw]

    res match {
      case s: JsSuccess[SectionRaw] => {
        val place: SectionRaw = s.get
        println(place)
      }
      case e: JsError => {
        // error handling flow
      }
    }


  }
}

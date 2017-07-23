package p150717

import org.scalatest._
import play.api.libs.json._

class FieldValueRawSpec extends FlatSpec with Matchers {
  "json" should "parse into FieldValueRaw" in {

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
                              mandatory: Option[String] = None
                            )


    object FieldValueRaw {

      import play.api.libs.functional.syntax._

      implicit val reads: Reads[FieldValueRaw] = (
        (__ \ 'id).read[FieldId] and
          (__ \ 'label).read[String] and
          (__ \ 'format).readNullable[FormatExpr] and
          (__ \ 'helpText).readNullable[String] and
          (__ \ 'optionHelpText).readNullable[List[String]] and
          (__ \ 'submitMode).readNullable[String] and
          (__ \ 'choices).readNullable[List[String]] and
          (__ \ 'fields).lazyReadNullable(implicitly[Reads[List[FieldValueRaw]]]) and //Note: recursiveness here prevents macro use (see JsonParseTestGroup)
          (__ \ 'mandatory).readNullable[String]
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

    res match {
      case s: JsSuccess[FieldValueRaw] => {
        val place: FieldValueRaw = s.get
        println(place)
      }
      case e: JsError => {
        // error handling flow
      }
    }


  }
}

package p160717

import org.scalatest._
import play.api.libs.json._

class FormatParserSpec extends FlatSpec with Matchers {
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


    println(FormatParser.validate("blah"))

  }
}

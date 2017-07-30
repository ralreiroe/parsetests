package p160717

import org.scalatest._
import play.api.libs.json._

class PresentationHintParsingSpec4 extends FlatSpec with Matchers {
  "collapseGroupUnderLabel,summariseGroupAsGrid" should
    """parse into a
      |List(
      | CollapseGroupUnderLabel,
      | SummariseGroupAsGrid)
      | """.stripMargin in {

    sealed trait PresentationHint
    case object CollapseGroupUnderLabel extends PresentationHint
    case object SummariseGroupAsGrid extends PresentationHint

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

    object PresentationHintParser {

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


      def validate(expression: String): Opt[List[PresentationHint]] = validateWithParser(expression, presentationHints)

      lazy val presentationHints: Parser[List[PresentationHint]] = (
        presentationHint ~ "," ~ presentationHints ^^ { (loc, presHint, _, presHints) => presHint :: presHints }
        | presentationHint ^^ { (loc, presHint) => List(presHint) }
      )

      lazy val presentationHint: Parser[PresentationHint] = (
        "collapseGroupUnderLabel" ^^ { (loc, unparsed) => CollapseGroupUnderLabel } |
          "summariseGroupAsGrid" ^^ { (loc, unparsed) => SummariseGroupAsGrid }
        )
    }

    println(PresentationHintParser.validate("blah"))
    println(PresentationHintParser.validate("blah"))
    println(PresentationHintParser.validate("blah,blah2"))
    println(PresentationHintParser.validate("blah,summariseGroupAsGrid"))
    println(PresentationHintParser.validate("collapseGroupUnderLabel"))
    println(PresentationHintParser.validate("collapseGroupUnderLabel,summariseGroupAsGrid"))

  }
}

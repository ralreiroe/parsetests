package p160717

import org.scalatest._
import play.api.libs.json._

class PresentationHintParsingSpec extends FlatSpec with Matchers {
  "collapseGroupUnderLabel,summariseGroupAsGrid" should "parse into a PresentationHints(List(PresentationHint(collapseGroupUnderLabel), PresentationHint(summariseGroupAsGrid)))" in {

    sealed trait PresentationHintExpr
    final case class PresentationHint(value: String) extends PresentationHintExpr
    final case class PresentationHints(hints: List[PresentationHint]) extends PresentationHintExpr

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


      def validate(expression: String): Opt[PresentationHintExpr] = validateWithParser(expression, presentationHints)

      lazy val presentationHints: Parser[PresentationHints] = (
        presentationHint ~ "," ~ presentationHints ^^ { (loc, presHint, _, presHints) => PresentationHints(presHint :: presHints.hints) }
        | presentationHint ^^ { (loc, presHint) => PresentationHints(List(presHint)) }
      )

      lazy val presentationHint: Parser[PresentationHint] = (
        permissibleValues ^^ { (loc, anyWord) => PresentationHint(anyWord) }
        )

      val anyWordFormat = """\w+""".r

      val permissibleValues = """\bcollapseGroupUnderLabel\b|\bsummariseGroupAsGrid\b""".r
    }


    println(PresentationHintParser.validate("blah"))
    println(PresentationHintParser.validate("blah,blah2"))
    println(PresentationHintParser.validate("blah,summariseGroupAsGrid"))
    println(PresentationHintParser.validate("collapseGroupUnderLabel"))
    println(PresentationHintParser.validate("collapseGroupUnderLabel,summariseGroupAsGrid"))

  }
}

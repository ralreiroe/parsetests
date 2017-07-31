package p140717

import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json._

class PresentationHintFromToJsonSpec_OptOptionOfRLineupSpec extends FlatSpec with Matchers {

  "" should "" in {

    sealed trait PresentationHint
    case object CollapseGroupUnderLabel extends PresentationHint
    case object SummariseGroupAsGrid extends PresentationHint

    object PresentationHintParser extends ParserUtil {

      import parseback._

      type Opt[A] = Either[UnexpectedState, A]

      lazy val presentationHints: Parser[List[PresentationHint]] = (
        presentationHint ~ "," ~ presentationHints ^^ { (loc, presHint, _, presHints) => presHint :: presHints }
          | presentationHint ^^ { (loc, presHint) => List(presHint) }
        )

      lazy val presentationHint: Parser[PresentationHint] = (
        "collapseGroupUnderLabel" ^^ { (loc, unparsed) => CollapseGroupUnderLabel } |
          "summariseGroupAsGrid" ^^ { (loc, unparsed) => SummariseGroupAsGrid }
        )

      def parseToLeftRight(expression: String): Opt[List[PresentationHint]] = parseToLeftRightUsing(expression, presentationHints)

    }


    println((Json.parse("""{"repeatsMin":1}""") \ "repeatsMax").validateOpt[Int])
    println((Json.parse("""{"repeatsMax":1}""") \ "repeatsMax").validateOpt[Int])
    println((Json.parse("""{"repeatsMax":{}}""") \ "repeatsMax").validateOpt[Int])
    println((Json.parse("""{"repeatsMax":"1"}""") \ "repeatsMax").validateOpt[Int])
    println(toOpt((Json.parse("""{"repeatsMin":1}""") \ "repeatsMax").validateOpt[Int]))
    println(toOpt((Json.parse("""{"repeatsMax":1}""") \ "repeatsMax").validateOpt[Int]))
    println(toOpt((Json.parse("""{"repeatsMax":{}}""") \ "repeatsMax").validateOpt[Int]))
    println(toOpt((Json.parse("""{"repeatsMax":"1"}""") \ "repeatsMax").validateOpt[Int]))


    val leftRightMaybeInt: Opt[Option[Int]] = toOpt((Json.parse("""{"repeatsMax":1}""") \ "repeatsMax").validateOpt[Int])

    val jsval = Json.parse("""{"presentationHint": "collapseGroupUnderLabel,summariseGroupAsGrid"}""") \ "presentationHint"
    val leftRightMaybeString: Opt[Option[String]] = toOpt(jsval.validateOpt[String])

    import cats.implicits._
    val res: Opt[Option[List[PresentationHint]]] = for {
      maybeString <- leftRightMaybeString.right
      res <- maybeString.map(PresentationHintParser.parseToLeftRight).sequenceU
    } yield res


    println(res)

  }


  sealed trait UnexpectedState
  case class InvalidState(errorMsg: String) extends UnexpectedState
  case class InvalidStateWithJson(errorMsg: String, json: JsValue) extends UnexpectedState

  type Opt[A] = Either[UnexpectedState, A]


  def toOpt[A](result: JsResult[A]): Opt[A] = {
    result match {
      case JsSuccess(a, _) => Right(a)
      case JsError(errors) => Left(InvalidState(errors.map {
        case (path, validationErrors) =>
          s"Path: ${path.toString}, Errors: ${validationErrors.map(_.messages.mkString(",")).mkString(",")}"
      }.mkString(",")))
    }
  }



  trait ParserUtil {

    import cats.Eval
    import cats.data.ReaderT
    import cats.instances.either._
    import parseback.{LineStream, Parser}
    import parseback.compat.cats._
    import parseback.util.Catenable

    private def parse[A](parser: Parser[A]) = ReaderT[Opt, String, Catenable[A]] { expression =>
      import cats.syntax.either._
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

    def parseToLeftRightUsing[A](expression: String, parser: Parser[A]): Opt[A] = (for {
      catenable <- parse(parser)
      expr <- reconstruct(catenable)
    } yield expr).run(expression)

  }




}

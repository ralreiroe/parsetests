package p270717

import julienrf.json.derived
import org.scalatest.{FlatSpec, Matchers}
import p270717.PresentationHintParser.Opt
import play.api.libs.json.JsValue


/** =============================================== */

sealed trait PresentationHint
case object CollapseGroupUnderLabel extends PresentationHint
case object SummariseGroupAsGrid extends PresentationHint

//  implicit val reads: Reads[PresentationHint] = derived.reads() //turn JsValue into PresentationHint


/** =============================================== */


sealed trait UnexpectedState extends Product with Serializable
case class InvalidState(errorMsg: String) extends UnexpectedState
case class InvalidStateWithJson(errorMsg: String, json: JsValue) extends UnexpectedState


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

class PresentationHintFromToJsonSpec extends FlatSpec with Matchers {

  import play.api.libs.json._



  def toLeftRight[A](result: JsResult[A]): Opt[A] = {
    import cats.syntax.either._

    result match {
      case JsSuccess(a, _) => a.asRight
      case JsError(errors) => InvalidState(errors.map {
        case (path, validationErrors) =>
          s"Path: ${path.toString}, Errors: ${validationErrors.map(_.messages.mkString(",")).mkString(",")}"
      }.mkString(",")).asLeft
    }
  }

  def parseTToOptOfMaybeR[T: Reads, R](parsedToJsValue: JsValue, path: String, parseToLeftRightR: T => Opt[R]): Opt[Option[R]] = {

    val toJsResultOfMaybeT = (parsedToJsValue \ path).validateOpt[T]
    val leftRightMaybeT: Opt[Option[T]] = toLeftRight(toJsResultOfMaybeT)
    import cats.implicits._
    for {
      maybeT <- leftRightMaybeT.right
      maybeLeftRightR: Option[Opt[R]] = maybeT.map(parseToLeftRightR)
      leftRightMaybeR <- maybeLeftRightR.sequenceU
    } yield leftRightMaybeR
  }

  def parseString[R](parsedToJsValue: JsValue, path: String, parseToLeftRightR: String => Opt[R]): Opt[Option[R]] = {

    val toJsResultOfMaybeString = (parsedToJsValue \ path).validateOpt(Reads.StringReads)
    val leftRightMaybeString: Opt[Option[String]] = toLeftRight(toJsResultOfMaybeString)
    import cats.implicits._
    for {
      maybeT <- leftRightMaybeString.right
      maybeLeftRightR: Option[Opt[R]] = maybeT.map(parseToLeftRightR)
      leftRightMaybeR <- maybeLeftRightR.sequenceU
    } yield leftRightMaybeR
  }

  "Using derived reads, a presentation" should "transform to Json correctly" in {

    sealed trait PresentationHint
    case object CollapseGroupUnderLabel extends PresentationHint
    case object SummariseGroupAsGrid extends PresentationHint

    val jsvToPH: Reads[PresentationHint] = derived.reads() //works for a single hint
    //  val jsvToPH2: Reads[PresentationHint] = Json.reads[PresentationHint] //not supported

    val jsv = Json.parse(
      """{
          "CollapseGroupUnderLabel" : {}
        }""".stripMargin)

    val res = jsv.validate[PresentationHint](jsvToPH)

    println(res)

  }


  "using derived owrites, a list of presentation hints" should "parse to array of Json objects correctly" in {

    implicit val writes: Writes[PresentationHint] = derived.owrites() //needs to be implicit

    val res = Json.toJson(Some(List(CollapseGroupUnderLabel, SummariseGroupAsGrid)))

    println(res)

    res.toString() shouldBe ("""[{"CollapseGroupUnderLabel":{}},{"SummariseGroupAsGrid":{}}]""")

  }


  "using derived oformat, a list of presentation hint objects in Json" should "transform to case objects correctly" in {

    implicit val format: OFormat[PresentationHint] = derived.oformat() //(1) format is needed for a list, reads does not work, (2) implicit is required, explicit does not work

    val res2 = Json.parse("""[{"CollapseGroupUnderLabel":{}},{"SummariseGroupAsGrid":{}}]""").validate[List[PresentationHint]]

    println(res2)

    res2 shouldBe (JsSuccess(List(CollapseGroupUnderLabel, SummariseGroupAsGrid)))
  }

  "Using derived reads/format, a list of presentation hints" should "transform to and from Json correctly" in {

    sealed trait PresentationHint
    case object CollapseGroupUnderLabel extends PresentationHint
    case object SummariseGroupAsGrid extends PresentationHint

    implicit val format: OFormat[PresentationHint] = derived.oformat() //(1) format is needed for a list, reads does not work, (2) implicit is required, expicit does not work

    val hints = List(CollapseGroupUnderLabel, SummariseGroupAsGrid)
    val jsonString = Json.toJson(hints).toString()
    jsonString shouldBe ("""[{"CollapseGroupUnderLabel":{}},{"SummariseGroupAsGrid":{}}]""")

    Json.parse(jsonString).validate[List[PresentationHint]] shouldBe (JsSuccess(hints))

    //or shorter...

    Json.parse(Json.toJson(hints).toString()).validate[List[PresentationHint]] shouldBe (JsSuccess(hints))

  }


  "Using bespoke parser, json containing presentation hints" should "parse correctly into List[PresentationHint]" in {

    val jsonString =
      """{
          "presentationHint": "collapseGroupUnderLabel,summariseGroupAsGrid"
        }"""


    val parsedToJsValue: JsValue = Json.parse(jsonString)

    lazy val optMaybePresentationHintExpr: Opt[Option[List[PresentationHint]]] = parseTToOptOfMaybeR(parsedToJsValue, "presentationHint", PresentationHintParser.parseToLeftRight)

    optMaybePresentationHintExpr shouldBe (Right(Some(List(CollapseGroupUnderLabel, SummariseGroupAsGrid))))

  }

}

trait ParserUtil {

  import cats.instances.either._
  import cats.Eval
  import parseback.Parser
  import cats.data.ReaderT
  import parseback.LineStream
  import parseback.util.Catenable
  import parseback.compat.cats._

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


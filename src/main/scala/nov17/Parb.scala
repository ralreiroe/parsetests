package nov17

import org.parboiled2._

import scala.util.Try

/**
  * Parboiled2 Parser
  *
  */

/**
  * Actions("""
       actions{
         ACTION_NAME=[ELEMENT,ANOTHER_ELEMENT,SOMETHING_ELSE]
         ANOTHER_ACTION=[SOMETHING_HERE,ANOTHER_ONE]
       }""")
    results in

    Success(Vector(Action(ACTION_NAME,Vector(ELEMENT, ANOTHER_ELEMENT, SOMETHING_ELSE)), Action(ANOTHER_ACTION,Vector(SOMETHING_HERE, ANOTHER_ONE))))
    while

    Actions(
      """
       actions{
         ACTION_NAME=[ELEMENT,ANOTHER_ELEMENT,SOMETHING_ELSE]
         ANOTHER_ACTION=[SOMETHING_HERE,AN0THER_ONE]
       }""")
    results in

    Failure(ParseError(Position(109,4,39), ...
  */

object Actions {
  case class Action(name: String, elems: Seq[String])

  def apply(input: String): Try[Seq[Action]] = new Actions(input).Input.run()
}

class Actions(val input: ParserInput) extends Parser {
  import Actions._

  def Input = rule {WS ~ "actions{" ~ WS ~ ActionList ~ WS ~ "}" ~ WS ~ EOI}

  def ActionList = rule {FirstAction ~ zeroOrMore(WS ~ ActionDef ~> ((_: Seq[Action]) :+ _))}

  def FirstAction = rule {ActionDef ~> (Vector(_))}

  def ActionDef = rule {(capture(Name) ~ "=[" ~ ElementList ~ "]") ~> Action.apply _}

  def ElementList = rule {
    WS ~ FirstElement ~ zeroOrMore(
      WS ~ "," ~ WS ~ NextElement ~> ((_: Seq[String]) :+ _))
  }

  def FirstElement = rule {capture(Name) ~> (Vector(_))}

  def NextElement = rule {capture(Name)}

  def Name = rule {oneOrMore(CharPredicate.Alpha | '_')}

  def WS = rule {zeroOrMore(anyOf(" \n\r\t\f"))}
}
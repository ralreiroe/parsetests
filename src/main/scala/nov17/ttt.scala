package nov17

import org.parboiled2._

import scala.util.Try

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
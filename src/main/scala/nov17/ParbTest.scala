package nov17

/**
  * https://stackoverflow.com/questions/33868994/get-repeating-regex-groups-in-scala
  *
  * Success(Vector(Action(ACTION_NAME,Vector(ELEMENT, ANOTHER_ELEMENT, SOMETHING_ELSE)), Action(ANOTHER_ACTION,Vector(SOMETHING_HERE, ANOTHER_ONE))))
Failure(ParseError(Position(109,4,39), Position(109,4,39), <5 traces>))

  */
object ParbTest extends App {


  println(Actions("""
   actions{
     ACTION_NAME=[ELEMENT,ANOTHER_ELEMENT,SOMETHING_ELSE]
     ANOTHER_ACTION=[SOMETHING_HERE,ANOTHER_ONE]
   }"""))

  println(Actions(
    """
   actions{
     ACTION_NAME=[ELEMENT,ANOTHER_ELEMENT,SOMETHING_ELSE]
     ANOTHER_ACTION=[SOMETHING_HERE,AN0THER_ONE]
   }"""))
}

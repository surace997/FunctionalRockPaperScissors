package dangol.rockpaperscissors

import fpinscala.parsing.JSON.{JArray, JBool, JNumber, JObject, JString}
import fpinscala.parsing.{JSON, Location, ParseError}

import scala.::
import scala.collection.IterableOnce.iterableOnceExtensionMethods
object ProjectParsing {
  val jsonTxt =
  """{
    "tournaments": 1000,
    "roundsPerMatch": 1000,
    "players": [
      {
        "name": "Last Losing 1",
        "type": "LastLosingMovePlayer"
      },"""

  def go = {
    val P = fpinscala.parsing.Reference
    import fpinscala.parsing.ReferenceTypes.Parser
    val json: Parser[JSON] = JSON.jsonParser(P)
    val resultOfParsing = P.run(json)(jsonTxt) // this parses JSON input into a JSON object
    resultOfParsing.flatMap(j => unpack(j)).map(dto => println(dto)).map(_ => ())
    //resultOfParsing.flatMap(j => betterUnpackUsingForComprehension(j)).map(dto => println(dto)).map(_ => ())
    //resultOfParsing.flatMap(j => betterUnpackUsingFlatMap(j)).map(dto => println(dto)).map(_ => ())
  }

  case class SeasonDTO(
                        tournaments: Int,
                        roundsPerMatch: Int,
                        players: List[Map[String, Any]]
                      )

  def unpack(json: JSON): Either[ParseError,SeasonDTO] = {
    val res = json match {
      case jObject: JObject =>
        for {
          numberOfTournaments <- jObject.get("tournaments") match {
            case jString: JNumber => Right(jString.get.intValue)
            case _ => Left(ParseError(List((Location("Could not unpack companyName"),"companyName"))))
          }
          roundsPerMatch <- jObject.get("roundsPerMatch") match {
            case jString: JNumber => Right(jString.get.intValue)
            case _ => Left(ParseError(List((Location("Could not unpack ticker"),"ticker"))))
          }

          players <- jObject.get("players") match {
            case jString: JNumber => Right(List[Map[String,Any]])
            case _ => Left(ParseError(List((Location("Could not unpack shares"),"shares"))))
          }

        } yield SeasonDTO(numberOfTournaments,roundsPerMatch,players)
      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"),"Could not unpack JSON contents"))))
    }
    res
  }

  def unpackList(c: List[JSON], r: Either[ParseError,List[String]]): Either[ParseError,List[String]] =
    c match {
      case ::(head, next) => head match {
        case JString(v) => unpackList(next, r.flatMap(list => Right(v :: list)))
        case p: ParseError => Left(p)
      }
      case Nil => r
    }


}

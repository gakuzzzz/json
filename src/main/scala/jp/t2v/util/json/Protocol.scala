package jp.t2v.util.json

private[json] trait Protocol {
  
  trait ToJson[-A] {
    def apply(value: A): String
  }

  implicit def toJsonable[A](value: A) = new {
    def toJson(implicit t: ToJson[A]): String = t(value)
  }

  def serializeJson[A](value: A)(implicit t: ToJson[A]): String = t(value)

  case class ParseFailure(source: String, targetType: String, reason: String = "", subReasons: Map[String, ParseFailure] = Map())

  import scala.util.parsing.combinator.JavaTokenParsers
  import scala.util.control.Exception.allCatch
  
  sealed abstract class JsonValue
  case class PrimitiveValue(value: String) extends JsonValue
  case object NoneValue extends JsonValue
  case class ArrayValue(value: Seq[JsonValue]) extends JsonValue
  case class ObjectValue(value: Map[String, JsonValue]) extends JsonValue
  
  private[json] trait JsonParser extends JavaTokenParsers {

    def unescapeDoubleQuote(in: String): String = 
      in.substring(1, in.length() - 1).replaceAllLiterally("""\"""", """"""").replaceAllLiterally("""\\""", """\""")

    def unescapeSingleQuote(in: String): String = 
      in.substring(1, in.length() - 1).replaceAllLiterally("""\'""", """'""").replaceAllLiterally("""\\""", """\""") // "
      
    def doubleQuoted: Parser[String] = """"([^"\\]|\\\\|\\")*"""".r ^^ unescapeDoubleQuote
    def singleQuoted: Parser[String] = """'([^'\\]|\\\\|\\')*'""".r ^^ unescapeSingleQuote
    
    def string: Parser[PrimitiveValue] = (doubleQuoted | singleQuoted) ^^ PrimitiveValue
    def boolean: Parser[PrimitiveValue] = ("true" | "false") ^^ PrimitiveValue
    def number: Parser[PrimitiveValue] = ("NaN" | floatingPointNumber) ^^ PrimitiveValue
    
    def none: Parser[JsonValue] = ("null" | "undefined") ^^ {_ => NoneValue}

    def value: Parser[JsonValue] = none | number | boolean | string | array | obj
    
    def array: Parser[ArrayValue] = "["~>repsep(value, ",")<~"]" ^^ ArrayValue
    def member: Parser[(String, JsonValue)] = string~":"~value ^^ {case s~":"~v => (s.value, v)}
    def obj: Parser[ObjectValue] = "{"~>repsep(member, ",")<~"}" ^^ {m => ObjectValue(m.toMap)}
  }
  
  trait FromJson[+A] {
    def apply[B >: A](json: JsonValue): Either[ParseFailure, B] =
      try {
        parser(json)
      } catch {
        case cause => Left(ParseFailure("", "", cause.getMessage))
      }

    def parser: PartialFunction[JsonValue, Either[ParseFailure, A]]
  }

  trait FromPrimitiveValue[A] extends FromJson[A] {
    def parser = {
      case PrimitiveValue(s) => apply(s)
    }
    def apply(value: String): Either[ParseFailure, A]
  }

  trait FromArrayValue[A] extends FromJson[A] {
    def parser = {
      case ArrayValue(n) => apply(n)
    }
    def apply(value: Seq[JsonValue]): Either[ParseFailure, A]
  }

  trait FromObjectValue[A] extends FromJson[A] {
    def parser = {
      case ObjectValue(n) => apply(n)
    }
    def apply(value: Map[String, JsonValue]): Either[ParseFailure, A]
  }
  
  def parseJson[A](json: String)(implicit f: FromJson[A]): Either[ParseFailure, A] = {
    val parser = new JsonParser {}
    val result = parser.parseAll(parser.value, json)
    if (result.successful) f(result.get)
    else Left(ParseFailure(json, ""))
  }
  
  private[json] def FromPrimitiveValue[A](f: String => A) = new FromPrimitiveValue[A] {
    def apply(value: String) = Right(f(value))
  }
  
  implicit val stringFromJson = FromPrimitiveValue[String] (identity)

  implicit val intFromJson = FromPrimitiveValue[Int] (_.toInt)

  implicit def optionFromJson[A](implicit f: FromJson[A]) = new FromJson[Option[A]] {
    def parser = {
      case NoneValue => Right(None)
      case v => f(v).right.map(a => Some(a))
    }
  }
  
  implicit def arrayFromJson[A](implicit f: FromJson[A], m: ClassManifest[A]) = new FromArrayValue[Array[A]] {
    def apply(value: Seq[JsonValue]) = {
      Left(null)
//      Right(value.map(v => f(v).right.get).toArray)
    }
  }

}
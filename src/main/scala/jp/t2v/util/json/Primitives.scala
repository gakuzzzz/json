package jp.t2v.util.json

private[json] trait Primitives {
  self: Protocol =>
    
  private[json] def ToJson[A](f: A => String) = new ToJson[A] {
    def apply(value: A): String = f(value) 
  }
  
  implicit val stringToJson = ToJson[String] {
    "\"" + _.replaceAllLiterally("""\""", """\\""").replaceAllLiterally("\"", """\"""") + "\"" // "
  }
  implicit val intToJson = ToJson[Int](_.toString)
  implicit val shortToJson = ToJson[Short](_.toString)
  implicit val longToJson = ToJson[Long](_.toString)
  implicit val doubleToJson = ToJson[Double](_.toString)
  implicit val floatToJson = ToJson[Float](_.toString)
  implicit val booleanToJson = ToJson[Boolean](_.toString)
  implicit val bigIntToJson = ToJson[BigInt](_.toString)
  implicit val bigDecimalToJson = ToJson[BigDecimal](_.toString)

  implicit def traversableToJson[A](implicit t: ToJson[A]) = new ToJson[Traversable[A]] {
    def apply(value: Traversable[A]): String = value.map(_.toJson).mkString("[", ", ", "]")
  }

  implicit def mapToJson[A, B](implicit t1: ToJson[A], t2: ToJson[B]) = new ToJson[Traversable[(A, B)]] {
    def apply(value: Traversable[(A, B)]): String = value match {
      case m: Map[A, B] => m.map {case (k, v) => k.toString.toJson + ": " + v.toJson}.mkString("{", ", ", "}")
      case _ => value.map {case (k, v) => "[" + k.toJson + ", " + v.toJson + "]"}.mkString("[", ", ", "]")
    }
  }

  implicit def arrayToJson[A](implicit t: ToJson[A]) = new ToJson[Array[A]] {
    def apply(value: Array[A]): String = value.map(_.toJson).mkString("[", ", ", "]")
  }

  implicit def optionToJson[A](implicit t: ToJson[A]) = ToJson[Option[A]] {
    case None => "null"
    case Some(v) => v.toJson
  }
  
  
}
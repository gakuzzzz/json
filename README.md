# t2v-json

# やりたいこと

カジュアルかつ型安全に Scalaオブジェクト <--> JSON文字列 の変換したい。

JSON化:

    scala> import jp.t2v.util.json._
    import jp.t2v.util.json._
    
    scala> List(10, 20, 30).toJson
    res0: String = [10, 20, 30]
    
    scala> List(List("aa", "bb"), List(), List("dd", "ee", "ff")).toJson
    res1: String = [["aa", "bb"], [], ["dd", "ee", "ff"]]

:

    scala> case class User(id: Int, name: String)
    defined class User
    
    scala> case class Message(user: User, body: String, lang: String)
    defined class Message
    
    scala> List(
         |   Message(User(1, "Tom"),  "Hello", "en"), 
         |   Message(User(2, "John"), "World", "en"), 
         |   Message(User(3, "Bob"),  "Scala", "en")
         |).toJson
    res2: String = [{"user": {"id": 1, "name": "Tom"},  "body": "Hello", "lang": "en"}, 
                    {"user": {"id": 2, "name": "John"}, "body": "World", "lang": "en"}, 
                    {"user": {"id": 3, "name": "Bob"},  "body": "Scala", "lang": "en"}]

Parse:

    scala> val source = """[["aa", "bb"], [], ["dd", "ee", "ff"]]"""
    res0: String = [["aa", "bb"], [], ["dd", "ee", "ff"]]

    scala> parseJson[List[List[[String]]](source)
    res1: Either[ParseFailure, List[List[java.lang.String]]] = 
            Right(List(List(aa, bb), List(), List(dd, ee, ff)))

    scala> parseJson[Seq[Array[[String]]](source)
    res2: Either[ParseFailure, Seq[Array[java.lang.String]]] =
            Right(Seq(Array(aa, bb), Array(), Array(dd, ee, ff)))

    scala> parseJson[List[List[[Int]]](source)
    res1: Either[ParseFailure, List[List[Int]]] = 
            Left(ParseFailure(could not parse "aa" to Int, ...))


# 現状できること

# 現状できないこと




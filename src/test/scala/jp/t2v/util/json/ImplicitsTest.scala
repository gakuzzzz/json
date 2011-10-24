package jp.t2v.util.json;

import org.junit.Test
import org.junit.Assert._
import Implicits._

class ImplicitsTest {

  @Test
  def IntのList {
	assertEquals("[10, 20, 30]", List(10, 20, 30).toJson)
  }
	
  @Test
  def 文字列のList {
	assertEquals("""["aaa", "bbb", "ccc"]""", List("aaa", "bbb", "ccc").toJson)
  }
  
  @Test
  def BooleanのList {
	assertEquals("[true, false, true]", List(true, false, true).toJson)
  }

  @Test
  def DoubleのList {
	assertEquals("[2.3, 4.5, 6.391]", List(2.3d, 4.5d, 6.391d).toJson)
  }
  
  @Test
  def ネストしたList {
    val actual = List(List(1, 2, 3), List(4, 5), List(6, 7, 8, 9)).toJson
    val expected = "[[1, 2, 3], [4, 5], [6, 7, 8, 9]]" 
    assertEquals(expected, actual)
  }

  @Test
  def 配列 {
    val actual = Array(Array(1, 2, 3), Array(4, 5), Array(6, 7, 8, 9)).toJson
    val expected = "[[1, 2, 3], [4, 5], [6, 7, 8, 9]]" 
    assertEquals(expected, actual)
  }
  
  
  @Test
  def マップ {
    val actual = List(
        Map("aaa" -> List(1, 2), "bbb" -> List(3)), 
        Map("ccc" -> List(4, 5, 6), "ddd" -> List())
      ).toJson
    val expected = """[{"aaa": [1, 2], "bbb": [3]}, {"ccc": [4, 5, 6], "ddd": []}]"""
    assertEquals(expected, actual)
  }
  
  @Test
  def タプル2のIterable {
    val actual = Iterable(
        ("aaa", 10),
        ("bbb", 20)
      ).toJson
    val expected = """[["aaa", 10], ["bbb", 20]]"""
    assertEquals(expected, actual)
  }

  @Test 
  def option {
    val actual = List(Some(10), None, Some(30)).toJson
    val expected = "[10, null, 30]"
    assertEquals(expected, actual)
  }

  @Test
  def Tupe3 {
    val actual = ("hoge", 10, false).toJson
    val expected = """["hoge", 10, false]"""
    assertEquals(expected, actual)
  }
  
  @Test
  def caseClass {
    case class Test(name: String, age: Int) extends Product2[String, Int] {
      override def _1 = name
      override def _2 = age
    }
    
    val actual = Test("Hoge", 30).toJson
    val expected = """{"name": "Hoge", "age": 30}"""
    assertEquals(expected, actual)
  }
  @Test
  def nestedCaseClass {
    case class Address(zip: Int, country: String) extends Product2[Int, String] {
      override def _1 = zip
      override def _2 = country
    }
    case class Parson(name: String, age: Int, address: Address) extends Product3[String, Int, Address] {
      override def _1 = name
      override def _2 = age
      override def _3 = address
    }
    
    val actual = Parson("Jon Smith", 30, Address(4000, "Japan")).toJson
    val expected = """{"name": "Jon Smith", "age": 30, "address": {"zip": 4000, "country": "Japan"}}"""
    assertEquals(expected, actual)
  }
  
  @Test
  def nestedCaseClass2 {
    case class Address(zip: Int, country: String) extends Product2[Int, String] {
      override def _1 = zip
      override def _2 = country
    }
    case class Parson(name: String, age: Int, address: Address) extends Product3[String, Int, Address] {
      override def _1 = name
      override def _2 = age
      override def _3 = address
    }
    
    val actual = List(
        Parson("Jon Smith", 30, Address(4000, "Japan")),
        Parson("Jon Smith", 30, Address(4000, "Japan")),
        Parson("Jon Smith", 30, Address(4000, "Japan"))).toJson
    val expected = 
      """|[
         |{"name": "Jon Smith", "age": 30, "address": {"zip": 4000, "country": "Japan"}}, 
         |{"name": "Jon Smith", "age": 30, "address": {"zip": 4000, "country": "Japan"}}, 
         |{"name": "Jon Smith", "age": 30, "address": {"zip": 4000, "country": "Japan"}}
         |]""".stripMargin.replaceAllLiterally("\r\n", "")
    assertEquals(expected, actual)
  }

  @Test
  def parseString {
    val actual = parseJson[String](""""hoge"""")
    val expected = Right("hoge")
    assertEquals(expected, actual)
  }
  
  @Test
  def parseEscapedString {
    val actual = parseJson[String](""""ho\"ge"""")
    val expected = Right("""ho"ge""")
    assertEquals(expected, actual)
  }

  @Test
  def parseEscapedString2 {
    val actual = parseJson[String](""""ho\\\"ge"""")
    val expected = Right("""ho\"ge""")
    assertEquals(expected, actual)
  }
  
  @Test
  def parseEscapedString3 {
    val actual = parseJson[String](""""ho\\"ge"""")
    assertTrue(actual.isLeft)
  }

  @Test
  def parseInt {
    val actual = parseJson[Int]("10")
    val expected = Right(10)
    assertEquals(expected, actual)
  }
  
  @Test
  def parseIntNegative {
    val actual = parseJson[Int]("-10")
    val expected = Right(-10)
    assertEquals(expected, actual)
  }

  @Test
  def parsePointedInt {
    val actual = parseJson[Int]("10.03")
    assertTrue(actual.isLeft)
//    println(actual)
  }

}

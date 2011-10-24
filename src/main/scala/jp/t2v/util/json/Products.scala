package jp.t2v.util.json

private[json] trait Products {
  self: Protocol with Primitives =>
  
  implicit def product1ToJson[T1](implicit t: ToJson[T1]) = new ToJson[Product1[T1]] {
    def apply(value: Product1[T1]): String = value match {
      case v if v.productPrefix == "Tuple1" => "[" + v._1.toJson + "]"
      case _ => 
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson
          ).mkString("{", ", ", "}")
    }
  }

  implicit def product2ToJson[T1, T2](implicit t1: ToJson[T1], t2: ToJson[T2]) =
    new ToJson[Product2[T1, T2]] {
      def apply(value: Product2[T1, T2]): String = value match {
        case v if v.productPrefix == "Tuple2" =>
          Seq(v._1.toJson, v._2.toJson).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product3ToJson[T1, T2, T3](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3]) =
    new ToJson[Product3[T1, T2, T3]] {
      def apply(value: Product3[T1, T2, T3]): String = value match {
        case v if v.productPrefix == "Tuple3" =>
          Seq(v._1.toJson, v._2.toJson, v._3.toJson).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson
          ).mkString("{", ", ", "}")
      }
    }

  implicit def product4ToJson[T1, T2, T3, T4](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4]) =
    new ToJson[Product4[T1, T2, T3, T4]] {
      def apply(value: Product4[T1, T2, T3, T4]): String = value match {
        case v if v.productPrefix == "Tuple4" =>
          Seq(v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product5ToJson[T1, T2, T3, T4, T5](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5]) =
    new ToJson[Product5[T1, T2, T3, T4, T5]] {
      def apply(value: Product5[T1, T2, T3, T4, T5]): String = value match {
        case v if v.productPrefix == "Tuple5" =>
          Seq(v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product6ToJson[T1, T2, T3, T4, T5, T6](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6]) =
    new ToJson[Product6[T1, T2, T3, T4, T5, T6]] {
      def apply(value: Product6[T1, T2, T3, T4, T5, T6]): String = value match {
        case v if v.productPrefix == "Tuple6" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson
          ).mkString("{", ", ", "}")
      }
    }

  implicit def product7ToJson[T1, T2, T3, T4, T5, T6, T7](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7]) =
    new ToJson[Product7[T1, T2, T3, T4, T5, T6, T7]] {
      def apply(value: Product7[T1, T2, T3, T4, T5, T6, T7]): String = value match {
        case v if v.productPrefix == "Tuple7" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson
          ).mkString("{", ", ", "}")
      }
    }

  implicit def product8ToJson[T1, T2, T3, T4, T5, T6, T7, T8](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8]) =
    new ToJson[Product8[T1, T2, T3, T4, T5, T6, T7, T8]] {
      def apply(value: Product8[T1, T2, T3, T4, T5, T6, T7, T8]): String = value match {
        case v if v.productPrefix == "Tuple8" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product9ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9]) =
    new ToJson[Product9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] {
      def apply(value: Product9[T1, T2, T3, T4, T5, T6, T7, T8, T9]): String = value match {
        case v if v.productPrefix == "Tuple9" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product10ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10]) =
    new ToJson[Product10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] {
      def apply(value: Product10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): String = value match {
        case v if v.productPrefix == "Tuple10" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product11ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11]) =
    new ToJson[Product11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] {
      def apply(value: Product11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]): String = value match {
        case v if v.productPrefix == "Tuple11" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product12ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11], 
      t12: ToJson[T12]) =
    new ToJson[Product12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] {
      def apply(value: Product12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]): String = value match {
        case v if v.productPrefix == "Tuple12" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson, v._12.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson,
            fields(11) + ": " + value._12.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product13ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11], 
      t12: ToJson[T12], t13: ToJson[T13]) =
    new ToJson[Product13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] {
      def apply(value: Product13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]): String = value match {
        case v if v.productPrefix == "Tuple13" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson, v._12.toJson, v._13.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson,
            fields(11) + ": " + value._12.toJson,
            fields(12) + ": " + value._13.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product14ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11], 
      t12: ToJson[T12], t13: ToJson[T13], t14: ToJson[T14]) =
    new ToJson[Product14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] {
      def apply(value: Product14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]): String = value match {
        case v if v.productPrefix == "Tuple14" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson, v._12.toJson, v._13.toJson, v._14.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson,
            fields(11) + ": " + value._12.toJson,
            fields(12) + ": " + value._13.toJson,
            fields(13) + ": " + value._14.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product15ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11], 
      t12: ToJson[T12], t13: ToJson[T13], t14: ToJson[T14], t15: ToJson[T15]) =
    new ToJson[Product15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] {
      def apply(value: Product15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]): String = value match {
        case v if v.productPrefix == "Tuple15" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson, v._12.toJson, v._13.toJson, v._14.toJson, v._15.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson,
            fields(11) + ": " + value._12.toJson,
            fields(12) + ": " + value._13.toJson,
            fields(13) + ": " + value._14.toJson,
            fields(14) + ": " + value._15.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product16ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11], 
      t12: ToJson[T12], t13: ToJson[T13], t14: ToJson[T14], t15: ToJson[T15], t16: ToJson[T16]) =
    new ToJson[Product16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] {
      def apply(value: Product16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]): String = value match {
        case v if v.productPrefix == "Tuple16" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson, v._12.toJson, v._13.toJson, v._14.toJson, v._15.toJson, 
            v._16.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson,
            fields(11) + ": " + value._12.toJson,
            fields(12) + ": " + value._13.toJson,
            fields(13) + ": " + value._14.toJson,
            fields(14) + ": " + value._15.toJson,
            fields(15) + ": " + value._16.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product17ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11], 
      t12: ToJson[T12], t13: ToJson[T13], t14: ToJson[T14], t15: ToJson[T15], t16: ToJson[T16], 
      t17: ToJson[T17]) =
    new ToJson[Product17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] {
      def apply(value: Product17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]): String = value match {
        case v if v.productPrefix == "Tuple17" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson, v._12.toJson, v._13.toJson, v._14.toJson, v._15.toJson, 
            v._16.toJson, v._17.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson,
            fields(11) + ": " + value._12.toJson,
            fields(12) + ": " + value._13.toJson,
            fields(13) + ": " + value._14.toJson,
            fields(14) + ": " + value._15.toJson,
            fields(15) + ": " + value._16.toJson,
            fields(16) + ": " + value._17.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product18ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11], 
      t12: ToJson[T12], t13: ToJson[T13], t14: ToJson[T14], t15: ToJson[T15], t16: ToJson[T16], 
      t17: ToJson[T17], t18: ToJson[T18]) =
    new ToJson[Product18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] {
      def apply(value: Product18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]): String = value match {
        case v if v.productPrefix == "Tuple18" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson, v._12.toJson, v._13.toJson, v._14.toJson, v._15.toJson, 
            v._16.toJson, v._17.toJson, v._18.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson,
            fields(11) + ": " + value._12.toJson,
            fields(12) + ": " + value._13.toJson,
            fields(13) + ": " + value._14.toJson,
            fields(14) + ": " + value._15.toJson,
            fields(15) + ": " + value._16.toJson,
            fields(16) + ": " + value._17.toJson,
            fields(17) + ": " + value._18.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product19ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11], 
      t12: ToJson[T12], t13: ToJson[T13], t14: ToJson[T14], t15: ToJson[T15], t16: ToJson[T16], 
      t17: ToJson[T17], t18: ToJson[T18], t19: ToJson[T19]) =
    new ToJson[Product19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] {
      def apply(value: Product19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]): String = value match {
        case v if v.productPrefix == "Tuple19" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson, v._12.toJson, v._13.toJson, v._14.toJson, v._15.toJson, 
            v._16.toJson, v._17.toJson, v._18.toJson, v._19.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson,
            fields(11) + ": " + value._12.toJson,
            fields(12) + ": " + value._13.toJson,
            fields(13) + ": " + value._14.toJson,
            fields(14) + ": " + value._15.toJson,
            fields(15) + ": " + value._16.toJson,
            fields(16) + ": " + value._17.toJson,
            fields(17) + ": " + value._18.toJson,
            fields(18) + ": " + value._19.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product20ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11], 
      t12: ToJson[T12], t13: ToJson[T13], t14: ToJson[T14], t15: ToJson[T15], t16: ToJson[T16], 
      t17: ToJson[T17], t18: ToJson[T18], t19: ToJson[T19], t20: ToJson[T20]) =
    new ToJson[Product20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] {
      def apply(value: Product20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]): String = value match {
        case v if v.productPrefix == "Tuple20" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson, v._12.toJson, v._13.toJson, v._14.toJson, v._15.toJson, 
            v._16.toJson, v._17.toJson, v._18.toJson, v._19.toJson, v._20.toJson 
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson,
            fields(11) + ": " + value._12.toJson,
            fields(12) + ": " + value._13.toJson,
            fields(13) + ": " + value._14.toJson,
            fields(14) + ": " + value._15.toJson,
            fields(15) + ": " + value._16.toJson,
            fields(16) + ": " + value._17.toJson,
            fields(17) + ": " + value._18.toJson,
            fields(18) + ": " + value._19.toJson,
            fields(19) + ": " + value._20.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product21ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11], 
      t12: ToJson[T12], t13: ToJson[T13], t14: ToJson[T14], t15: ToJson[T15], t16: ToJson[T16], 
      t17: ToJson[T17], t18: ToJson[T18], t19: ToJson[T19], t20: ToJson[T20], t21: ToJson[T21]) =
    new ToJson[Product21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] {
      def apply(value: Product21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]): String = value match {
        case v if v.productPrefix == "Tuple21" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson, v._12.toJson, v._13.toJson, v._14.toJson, v._15.toJson, 
            v._16.toJson, v._17.toJson, v._18.toJson, v._19.toJson, v._20.toJson, 
            v._21.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson,
            fields(11) + ": " + value._12.toJson,
            fields(12) + ": " + value._13.toJson,
            fields(13) + ": " + value._14.toJson,
            fields(14) + ": " + value._15.toJson,
            fields(15) + ": " + value._16.toJson,
            fields(16) + ": " + value._17.toJson,
            fields(17) + ": " + value._18.toJson,
            fields(18) + ": " + value._19.toJson,
            fields(19) + ": " + value._20.toJson,
            fields(20) + ": " + value._21.toJson
          ).mkString("{", ", ", "}")
      }
    }
  
  implicit def product22ToJson[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](
      implicit t1: ToJson[T1], t2: ToJson[T2], t3: ToJson[T3], t4: ToJson[T4], t5: ToJson[T5], 
      t6: ToJson[T6], t7: ToJson[T7], t8: ToJson[T8], t9: ToJson[T9], t10: ToJson[T10], t11: ToJson[T11], 
      t12: ToJson[T12], t13: ToJson[T13], t14: ToJson[T14], t15: ToJson[T15], t16: ToJson[T16], 
      t17: ToJson[T17], t18: ToJson[T18], t19: ToJson[T19], t20: ToJson[T20], t21: ToJson[T21], 
      t22: ToJson[T22]) =
    new ToJson[Product22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] {
      def apply(value: Product22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]): String = value match {
        case v if v.productPrefix == "Tuple22" =>
          Seq(
            v._1.toJson, v._2.toJson, v._3.toJson, v._4.toJson, v._5.toJson, 
            v._6.toJson, v._7.toJson, v._8.toJson, v._9.toJson, v._10.toJson, 
            v._11.toJson, v._12.toJson, v._13.toJson, v._14.toJson, v._15.toJson, 
            v._16.toJson, v._17.toJson, v._18.toJson, v._19.toJson, v._20.toJson, 
            v._21.toJson, v._22.toJson
          ).mkString("[", ", ", "]")
        case _ =>
          val fields = value.getClass.getDeclaredFields.map(_.getName.toJson)
          Seq(
            fields(0) + ": " + value._1.toJson,
            fields(1) + ": " + value._2.toJson,
            fields(2) + ": " + value._3.toJson,
            fields(3) + ": " + value._4.toJson,
            fields(4) + ": " + value._5.toJson,
            fields(5) + ": " + value._6.toJson,
            fields(6) + ": " + value._7.toJson,
            fields(7) + ": " + value._8.toJson,
            fields(8) + ": " + value._9.toJson,
            fields(9) + ": " + value._10.toJson,
            fields(10) + ": " + value._11.toJson,
            fields(11) + ": " + value._12.toJson,
            fields(12) + ": " + value._13.toJson,
            fields(13) + ": " + value._14.toJson,
            fields(14) + ": " + value._15.toJson,
            fields(15) + ": " + value._16.toJson,
            fields(16) + ": " + value._17.toJson,
            fields(17) + ": " + value._18.toJson,
            fields(18) + ": " + value._19.toJson,
            fields(19) + ": " + value._20.toJson,
            fields(20) + ": " + value._21.toJson,
            fields(21) + ": " + value._22.toJson
          ).mkString("{", ", ", "}")
      }
    }

}
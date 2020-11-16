package com.zx80live.gofp.bootstrap

import scala.annotation.tailrec

@deprecated("use com.zx80live.gofp.bootstrap.refactored")
object GoTypes {
  val GoBool = "bool"
  val GoString = "string"
  val GoInt = "int"
  val GoInt8 = "int8"
  val GoInt16 = "int16"
  val GoInt32 = "int32"
  val GoInt64 = "int64"
  val GoUInt = "uint"
  val GoUInt8 = "uint8"
  val GoUInt16 = "uint16"
  val GoUInt32 = "uint32"
  val GoUInt64 = "uint64"
  val GoUIntPtr = "uintptr"
  val GoByte = "byte"
  val GoRune = "rune"
  val GoFloat32 = "float32"
  val GoFloat64 = "float64"
  val GoComplex64 = "complex64"
  val GoComplex128 = "complex128"
  val GoAny = "Any"

  def array(t: String): String = s"[]$t"

  val boolTypes: Seq[String] = Seq(GoBool)
  val numericTypes: Seq[String] = Seq(
    GoRune,
    GoByte,
    GoInt,
    GoInt8,
    GoInt16,
    GoInt32,
    GoInt64,
    GoUInt,
    GoUInt8,
    GoUInt16,
    GoUInt32,
    GoUInt64,
    GoUIntPtr,
    GoFloat32,
    GoFloat64,
    GoComplex64,
    GoComplex128,
  )
  val stringTypes: Seq[String] = Seq(GoString)
  val otherTypes: Seq[String] = Seq(GoAny)

  val baseTypes: Seq[String] = boolTypes ++ numericTypes ++ stringTypes ++ otherTypes
  val arrayTypes: Seq[String] = baseTypes.map(array)
  val nestedArrayTypes: Seq[String] = arrayTypes.map(array)

  val types: Seq[String] = baseTypes ++ arrayTypes
  val names: Seq[(String, String)] = types.map(t => (t, toName(t)))

  def toName(t: String): String = {
    @tailrec
    def replaceBrackets(str: String): String = if (str.contains("[]")) {
      val tmp = str.replaceFirst("\\[\\]", "") + "Arr"
      replaceBrackets(tmp)
    } else {
      str
    }

    replaceBrackets(t).replaceAll("\\{\\}", "").capitalize
  }
}

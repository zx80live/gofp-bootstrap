package com.zx80live.gofp.bootstrap

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
  val GoInterface = "interface{}"

  def array(gotype: String): String = s"[]$gotype"

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
  val otherTypes: Seq[String] = Seq(GoInterface)

  val baseTypes: Seq[String] = boolTypes ++ numericTypes ++ stringTypes ++ otherTypes
  val arrayTypes: Seq[String] = baseTypes.map(t => s"[]$t")
  val allTypes: Seq[String] = baseTypes ++ arrayTypes

  def toName(t: String): String = {
    def replaceBrackets(str: String) = if (str.contains("[]")) {
      str.replaceAll("\\[\\]", "") + "Arr"
    } else {
      str
    }

    replaceBrackets(t).replaceAll("\\{\\}", "").capitalize
  }
}

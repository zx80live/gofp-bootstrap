package com.zx80live.gofp.bootstrap.refactored

object Types {
  val baseTypes = Seq(
    BaseType("int"),
  )

  val arrayTypes: Seq[ArrayType] = (baseTypes ++ baseTypes.map(ArrayType) ++ baseTypes.map(OptionType)).map(ArrayType)
  val optionTypes: Seq[OptionType] = (baseTypes ++ baseTypes.map(OptionType)).map(OptionType)
  val listTypes: Seq[ListType] = (baseTypes ++ baseTypes.map(ListType) ++ baseTypes.map(OptionType)).map(ListType)


  def main(args: Array[String]): Unit = {
    baseTypes foreach println
    println()
////    arrayTypes foreach  println
////    println()
    optionTypes foreach println
    println()
    listTypes foreach println

  }

  //  val GoBool = "bool"
  //  val GoString = "string"
  //  val GoInt = "int"
  //  val GoInt8 = "int8"
  //  val GoInt16 = "int16"
  //  val GoInt32 = "int32"
  //  val GoInt64 = "int64"
  //  val GoUInt = "uint"
  //  val GoUInt8 = "uint8"
  //  val GoUInt16 = "uint16"
  //  val GoUInt32 = "uint32"
  //  val GoUInt64 = "uint64"
  //  val GoUIntPtr = "uintptr"
  //  val GoByte = "byte"
  //  val GoRune = "rune"
  //  val GoFloat32 = "float32"
  //  val GoFloat64 = "float64"
  //  val GoComplex64 = "complex64"
  //  val GoComplex128 = "complex128"
  //  val GoAny = "Any"
  //
  //  val types: Seq[String] = Seq(
  //    GoBool,
  //    GoString,
  //    GoInt,
  //    GoInt8,
  //    GoInt16,
  //    GoInt32,
  //    GoInt64,
  //    GoUInt,
  //    GoUInt8,
  //    GoUInt16,
  //    GoUInt32,
  //    GoUInt64,
  //    GoUIntPtr,
  //    GoByte,
  //    GoRune,
  //    GoFloat32,
  //    GoFloat64,
  //    GoComplex64,
  //    GoComplex128,
  //    GoAny)
  //
  //    def toName(t: String): String = t.capitalize
}

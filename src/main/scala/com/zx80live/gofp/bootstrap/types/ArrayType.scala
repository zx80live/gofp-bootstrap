package com.zx80live.gofp.bootstrap.types

/*
 raw:
   []int
   [][]int
   []OptionInt

 declare:
   type IntArr []int
   type IntArrArr [][]int
   type OptionIntArr []OptionInt

 cons:
   var a IntArr = []int { 1,2,3 }
   var a IntArrArr = [][]int { []int{1,2,3}, []int{4,5,6} }
   var a OptionIntArr = []OptionInt { Int(1), Int(2), Int(3) }

 usage:
   func <view>ToString
   func IntArrToString(e []int) string
   func IntArrArrToString(e [][]int) string
   func OptionIntArrToString(e []OptionInt)

   func IntArrEquals(a, b []int) bool
   func IntArrArrEquals(a, b [][]int) bool
   func OptionIntArrEquals(a, b []OptionInt) bool
*/
case class ArrayType(underlined: Type) extends TraversableType {
  override def raw: String = s"[]${underlined.raw}"

  override def rawFrom(t: Type): String = s"[]${t.raw}"

  override def nilNameFrom(t: Type): String = rawFrom(t)

  override def alias: String = underlined match {
    case _: BaseType => s"${underlined.view}Arr"
    case _ => s"${underlined.alias}Arr"
  }

  override def declaration: String = s"type $alias $raw"
}

object ArrayType {
  def underlinedTypes: Seq[Type] = BaseType.types ++ BaseType.types.map(ArrayType.apply) ++ BaseType.types.map(OptionType.apply)
  def types: Seq[ArrayType] = underlinedTypes.map(ArrayType.apply)
}

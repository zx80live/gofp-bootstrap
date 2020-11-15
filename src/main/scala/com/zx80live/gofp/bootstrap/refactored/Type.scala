package com.zx80live.gofp.bootstrap.refactored

trait Type {
  def raw: String
  def view: String
  def declare: String = s"// declare does not supported for $raw"

  override def toString: String = s"$raw \t $view \t $declare"
}

/*
 raw:
   int, string, bool

 view:
   Int, String, Bool

 usage:
   func <view>ToString(e <this.raw>) <TO.raw>
   func IntToString(e int) string

   func <view>Equals(a, b <raw>) bool
   func IntEquals(a, b int) bool
 */
case class BaseType(value: String) extends Type {
  override def raw: String = value

  override def view: String = raw.capitalize
}

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
case class ArrayType(underlined: Type) extends Type {
  override def raw: String = s"[]${underlined.raw}"

  override def view: String = s"${underlined.view}Arr"

  override def declare: String = s"type $view $raw"
}

/*
 raw:
   OptionInt
   OptionOptionInt

 declare:
   type OptionInt struct { value *int }
   type OptionOptionInt struct { value *OptionInt }


 cons:
   var e IntOption = Int(1)
   var e IntOptionOption = IntInt(1)

 usage:
   func IntOptionToString(e IntOption) string
   func IntOptionOptionToString(e IntOptionOption) string

 extension:
   func (e IntOption) ToString() string
   func (e IntOptionOption) ToString() string
   func (e IntOption) MapInt(f IntToInt) OptionInt
   func (e IntOption) FlatMapIntOption(f IntToIntOption) OptionInt

 */
case class OptionType(underlined: Type) extends Type {
  override def raw: String = s"${underlined.view}Option"

  override def view: String = raw

  override def declare: String = s"type $raw struct { value *${underlined.raw} }"
}

case class ListType(underlined: Type) extends Type {
  override def raw: String = s"${underlined.view}List"

  override def view: String = raw

  override def declare: String =
    s"""
       |type $raw struct {
       |  head *${underlined.raw}
       |  tail *${raw}
       |}""".stripMargin
}


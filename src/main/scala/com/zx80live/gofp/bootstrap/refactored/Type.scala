package com.zx80live.gofp.bootstrap.refactored

trait Type {
  def underlined: Type
  def raw: String
  def alias: String = raw
  def view: String = alias
  def declare: String = ""
  def consView: String = ""
  def core: Type = underlined match {
    case SuperType => this
    case _ => underlined.core
  }

  override def toString: String =
    s"""
       |raw:        $raw
       |underlined: ${underlined.raw}
       |core:       ${core.raw}
       |alias:      $alias
       |view:       $view        // func ${view}ToString() string
       |consView:   $consView    ${if(consView.nonEmpty) " \t\t\t // func " + consView + "(...) " + raw else ""}
       |declare:    $declare
       |""".stripMargin
}

case object SuperType extends Type {
  override def underlined: Type = this

  override def raw: String = "<super>"
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

  override def underlined: Type = SuperType

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

  override def alias: String = underlined match {
    case _: BaseType => s"${underlined.view}Arr"
    case _ => s"${underlined.alias}Arr"
  }

  override def declare: String = s"type $alias $raw"
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

  override def consView: String = underlined match {
    case _: BaseType => s"${underlined.view}"
    case _: OptionType => s"${underlined.consView}${underlined.core.view}"
  }
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

  override def consView: String = s"Make$raw"
}


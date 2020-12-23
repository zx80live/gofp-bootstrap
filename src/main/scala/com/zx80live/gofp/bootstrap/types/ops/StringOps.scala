package com.zx80live.gofp.bootstrap.types.ops

import com.zx80live.gofp.bootstrap.types.BaseType

object StringOps {
  val t: String = BaseType.GoString.boxedRaw

  def funcToArray: String = {
    s"""
       |func (s $t) ToArray() RuneArray { return RuneArray([]rune(s)) }
       |""".stripMargin
  }

  def funcToLetterArray: String = {
    s"""
       |func (s $t) ToLetterArray() StringArray { return RuneArray([]rune(s)).MapString(func(r rune) string { return fmt.Sprintf("%c", r) } ) }""".stripMargin
  }

  def funcStartsWith: String =
    s"""
       |func (s $t) StartsWith(str string) bool {
       |  return strings.HasPrefix(string(s), str)
       |}""".stripMargin

  def funcEndsWith: String =
    s"""
       |func (s $t) EndsWith(str string) bool {
       |  return strings.HasSuffix(string(s), str)
       |}""".stripMargin

  def funcContains: String =
    s"""
       |func (s $t) Contains(str string) bool {
       |  return strings.Contains(string(s), str)
       |}""".stripMargin

  def funcSplit: String =
    s"""
       |func (s $t) Split(sep string) StringArray {
       |  return StringArray(strings.Split(string(s), sep))
       |}""".stripMargin

  def funcRegexGroups: String =
    s"""
       |func (s $t) RegexGroups(regex string) StringArray {
       |  return StringRegexGroups(regex)(string(s))
       |}""".stripMargin

  def funcRegexGroupsCompiled: String =
    s"""
       |func (s $t) RegexGroupsCompiled(r *regexp.Regexp) StringArray {
       |  return RegexGroups(r)(string(s))
       |}""".stripMargin

  def funcTake: String =
    s"""
       |func (s $t) Take(n int) String {
	     |  if n > len(s) {
       |		return s
       |	} else if n < 0 {
       |		return ""
       |	} else {
       |		return s[0:n]
       |	}
       |}""".stripMargin

  def funcTakeRight: String =
    s"""
       |func (s $t) TakeRight(n int) String {
	     |  l := len(s)
       |	if n > l {
       |		return s
       |	} else if n < 0 {
       |		return ""
       |	} else {
       |		return s[l-n:l]
       |	}
       |}""".stripMargin

  def funcDrop: String =
    s"""
       |func (s $t) Drop(n int) String {
       |  l := len(s)
       |  if n > l {
       |    return ""
       |  } else if n < 0 {
       |    return s
       |  } else { return s[n:l] }
       |}""".stripMargin

  def funcDropRight: String =
    s"""
       |func (s $t) DropRight(n int) String {
       |  l := len(s)
       |  if n > l {
       |    return ""
       |  } else if n < 0 {
       |    return s
       |  } else { return s[0:l-n] }
       |}""".stripMargin

  def funcTrim: String =
    s"""
       |func (s $t) Trim(cutset string) string {
       |  return strings.Trim(string(s), cutset)
       |}""".stripMargin

  def functions: Seq[String] = Seq(
    funcToArray,
    funcToLetterArray,
    funcStartsWith,
    funcEndsWith,
    funcContains,
    funcSplit,
    funcRegexGroups,
    funcRegexGroupsCompiled,
    funcTake,
    funcTakeRight,
    funcDrop,
    funcDropRight,
    funcTrim
  )

}

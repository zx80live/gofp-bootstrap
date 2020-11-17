package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.functions.{FuncEquals, FuncToString}

trait TraversableType extends Type {
  def rawFrom(t: Type): String
  def nilNameFrom(t: Type): String

  def funcToString: String =
    s"""
       |func (m $raw) ToString() string { return ${FuncToString.name(this)}(m) }""".stripMargin

  def funcEquals: String =
    s"""
       |func (m1 $raw) Equals(m2 $raw) bool { return ${FuncEquals.name(this)}(m1, m2) }""".stripMargin
}

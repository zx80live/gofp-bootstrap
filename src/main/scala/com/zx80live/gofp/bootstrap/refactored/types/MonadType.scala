package com.zx80live.gofp.bootstrap.refactored.types

import com.zx80live.gofp.bootstrap.refactored.functions.{FuncFilter, FuncMap}

trait MonadType extends TraversableType {

  def funcCons: String

  def funcForeach: String

  def funcFilter: String =
    s"""
       |func (m $raw) Filter(p ${Predicate.name(underlined)}) $raw { return ${FuncFilter.name(this)}(m, p) }""".stripMargin

  def funcMap(out: Type): String =
    s"""
       |func (m $raw) Map${out.view}(f func(${underlined.raw}) ${out.raw}) ${rawFrom(out)} { return ${FuncMap.name(this, out)}(m, f) }""".stripMargin


}

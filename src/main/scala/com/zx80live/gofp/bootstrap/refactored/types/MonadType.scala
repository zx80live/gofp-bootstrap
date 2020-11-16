package com.zx80live.gofp.bootstrap.refactored.types

trait MonadType extends Type {
//  def transformer(t: Type): Transformer = Transformer(underlined, t)
//
//  def funcMap(t: Type): String

  def funcCons: String

  def funcForeach: String
}

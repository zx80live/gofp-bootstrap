package com.zx80live.gofp.bootstrap.refactored.types

trait MonadType {
  this: Type =>

  def funcCons: String

  def funcForeach: String
}

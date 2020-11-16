package com.zx80live.gofp.bootstrap.refactored.types

trait TraversableType extends Type {
  def rawFrom(t: Type): String
  def nilNameFrom(t: Type): String
}

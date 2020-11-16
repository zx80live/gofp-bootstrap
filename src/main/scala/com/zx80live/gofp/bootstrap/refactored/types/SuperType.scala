package com.zx80live.gofp.bootstrap.refactored.types

case object SuperType extends Type {
  override def underlined: Type = this

  override def raw: String = "<super>"
}

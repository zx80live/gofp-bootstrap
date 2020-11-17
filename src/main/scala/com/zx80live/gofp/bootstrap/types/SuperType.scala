package com.zx80live.gofp.bootstrap.types

case object SuperType extends Type {
  override def underlined: Type = this

  override def raw: String = "<super>"
}

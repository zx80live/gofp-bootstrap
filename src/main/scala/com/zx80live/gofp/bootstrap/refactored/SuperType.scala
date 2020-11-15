package com.zx80live.gofp.bootstrap.refactored

case object SuperType extends Type {
  override def underlined: Type = this

  override def raw: String = "<super>"
}

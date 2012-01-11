package com.osinka.subset

/** RichString
  */
class FieldBlank(val name: String) {
  class SubsetBlank[Self](self:Self) {
    def of[T] = Subset[T,Self](name, self)
  }

  def fieldOf[T]: Field[T] = Field[T](name)

  def subset[Self](self: Self) = new SubsetBlank[Self](self)
}

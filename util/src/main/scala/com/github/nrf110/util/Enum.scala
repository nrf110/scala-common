package com.github.nrf110.util

import scala.reflect.ClassTag

/**
 * Base trait for enums. Advantages over scala.Enumeration:
 * 1) case object allows use of enumerations in match statements
 * 2) sealed trait allows for ensuring all cases are handled in match statement
 * 3) case classes allow for more fields in base sealed trait than just a name
 *
 * For details see http://stackoverflow.com/a/4958905
**/
abstract class Enum[A : ClassTag] {
  type EnumType = A
  private[this] val className = implicitly[ClassTag[A]].runtimeClass.getCanonicalName

  trait Value extends Serializable { self: A =>
    val name = toString
  }

  abstract class NamedValue(override val name: String) extends Value { self: A => }

  val values: Traversable[Value]
  private[this] lazy val nameToValueMap = values.map(v => (v.name,v.asInstanceOf[A])).toMap
  def withName(name : String) : Option[A] = nameToValueMap.get(name)
  /**
   * @throws IllegalArgumentException if there is no enumeration value with the given name
   * @return the enumeration value with the given name
   * */
  def parse(value: String) : A = {
    withName(value).getOrElse(throw new IllegalArgumentException(s"Invalid $className $value!"))
  }
}

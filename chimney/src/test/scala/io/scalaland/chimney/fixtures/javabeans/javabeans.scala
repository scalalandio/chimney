package io.scalaland.chimney.fixtures.javabeans

import scala.annotation.{nowarn, unused}

case class CaseClassNoFlag(id: String, name: String)

case class CaseClassWithFlagMethod(id: String, name: String) {
  def flag: Boolean = true
}

case class CaseClassWithFlag(id: String, name: String, flag: Boolean) {
  def equalsToBean(jbswf: JavaBeanSourceWithFlag): Boolean =
    id == jbswf.getId && name == jbswf.getName && flag == jbswf.isFlag
}

case class CaseClassWithFlagRenamed(id: String, name: String, renamedFlag: Boolean)

class JavaBeanSource(id: String, name: String) {
  def getId: String = id

  def getName: String = name
}

@nowarn("msg=unset private variable")
class JavaBeanSourceWithFlag(private var id: String, private var name: String, private var flag: Boolean) {

  def getId: String = id

  def getName: String = name

  def isFlag: Boolean = flag
}

class JavaBeanSourceWithAmbiguity(var id: String, var name: String, var flag: Boolean, val dupa: Int = 10) {

  def getId: String = id

  def getName: String = name

  def isFlag: Boolean = flag
}

class JavaBeanTarget {
  private var id: String = scala.compiletime.uninitialized
  private var name: String = scala.compiletime.uninitialized
  private var flag: Boolean = scala.compiletime.uninitialized

  def setId(id: String): Unit =
    this.id = id

  def setName(name: String): Unit =
    this.name = name

  def setFlag(flag: Boolean): Unit =
    this.flag = flag

  // make sure that only public setters are taken into account
  protected def setFoo(foo: Unit): Unit = ()

  @unused private def setBar(bar: Int): Unit = ()

  def getId: String = id

  def getName: String = name

  def isFlag: Boolean = flag

  override def equals(obj: Any): Boolean = obj match {
    case jbt: JavaBeanTarget => this.id == jbt.getId && this.name == jbt.getName && this.flag == jbt.isFlag
    case _                   => false
  }
}

class JavaBeanTargetNoIdSetter {
  private var id: String = scala.compiletime.uninitialized

  def withId(id: String): Unit =
    this.id = id

  // make sure that only public setters are taken into account
  protected def setFoo(foo: Unit): Unit = ()

  @unused private def setBar(bar: Int): Unit = ()

  def getId: String = id

  override def equals(obj: Any): Boolean = obj match {
    case jbt: JavaBeanTarget => this.id == jbt.getId
    case _                   => false
  }
}

class JavaBeanTargetNonUnitSetter {
  private var id: String = scala.compiletime.uninitialized
  private var name: String = scala.compiletime.uninitialized
  private var flag: Boolean = scala.compiletime.uninitialized

  def getId(): String = id
  def setId(id: String): Unit = this.id = id

  def getName(): String = name
  def setName(name: String): JavaBeanTargetNonUnitSetter = {
    this.name = name
    this
  }

  def isFlag(): Boolean = flag
  def setFlag(id: Boolean): JavaBeanTargetNonUnitSetter = {
    this.flag = flag
    this
  }

  override def equals(obj: Any): Boolean = obj match {
    case jbt: JavaBeanTargetNonUnitSetter =>
      this.id == jbt.getId() && this.name == jbt.getName() && this.flag == jbt.isFlag()
    case _ => false
  }
}

case class EnclosingCaseClass(ccNoFlag: CaseClassNoFlag)

class EnclosingBean {
  private var ccNoFlag: CaseClassNoFlag = scala.compiletime.uninitialized

  def getCcNoFlag: CaseClassNoFlag = ccNoFlag

  def setCcNoFlag(ccNoFlag: CaseClassNoFlag): Unit = this.ccNoFlag = ccNoFlag

  override def equals(obj: Any): Boolean = obj match {
    case eb: EnclosingBean => this.ccNoFlag == eb.ccNoFlag
    case _                 => false
  }
}

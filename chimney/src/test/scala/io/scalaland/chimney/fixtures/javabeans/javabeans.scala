package io.scalaland.chimney.fixtures.javabeans

case class CaseClassNoFlag(id: String, name: String)

case class CaseClassWithFlagMethod(id: String, name: String) {
  def flag: Boolean = true
}

case class CaseClassWithFlag(id: String, name: String, flag: Boolean) {
  def equalsToBean(jbswf: JavaBeanSourceWithFlag): Boolean = {
    id == jbswf.getId && name == jbswf.getName && flag == jbswf.isFlag
  }
}

class JavaBeanSource(id: String, name: String) {
  def getId: String = id

  def getName: String = name
}

class JavaBeanSourceWithFlag(private var id: String, private var name: String, private var flag: Boolean) {
  def getId: String = id

  def getName: String = name

  def isFlag: Boolean = flag
}

class JavaBeanTarget {
  private var id: String = _
  private var name: String = _
  private var flag: Boolean = _

  def setId(id: String): Unit = {
    this.id = id
  }

  def setName(name: String): Unit = {
    this.name = name
  }

  def setFlag(flag: Boolean): Unit = {
    this.flag = flag
  }

  // make sure that only public setters are taken into account
  protected def setFoo(foo: Unit): Unit = ()

  private def setBar(bar: Int): Unit = ()

  def getId: String = id

  def getName: String = name

  def isFlag: Boolean = flag

  override def equals(obj: Any): Boolean = {
    obj match {
      case jbt: JavaBeanTarget =>
        this.id == jbt.getId && this.name == jbt.getName && this.flag == jbt.isFlag
      case _ =>
        false
    }
  }
}

case class EnclosingCaseClass(ccNoFlag: CaseClassNoFlag)

class EnclosingBean {
  private var ccNoFlag: CaseClassNoFlag = _

  def getCcNoFlag: CaseClassNoFlag = ccNoFlag

  def setCcNoFlag(ccNoFlag: CaseClassNoFlag): Unit = {
    this.ccNoFlag = ccNoFlag
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case eb: EnclosingBean =>
        this.ccNoFlag == eb.ccNoFlag
      case _ =>
        false
    }
  }
}

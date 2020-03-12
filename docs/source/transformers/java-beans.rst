Java beans
==========

Beside Scala case classes, Chimney supports transformation
of Java beans.

Reading from Java beans
-----------------------

Chimney supports automatic field renaming for classes that
follow Java beans naming convention. Let's assume
the following classes.

.. code-block:: scala

  class MyBean(private var id: Long,
               private var name: String,
               private var flag: Boolean) {
      def getId: Long = id
      def getName: String = name
      def isFlag: Boolean = flag
  }

  case class MyCaseClass(id: Long, name: String, flag: Boolean)

The conversion works only if you explicitly enable it with
``.enableBeanGetters`` operation.

.. code-block:: scala

  new MyBean(1L, "beanie", true)
    .into[MyCaseClass]
    .enableBeanGetters
    .transform
  //  MyCaseClass(1L, "beanie", true)


.. note::

  Chimney matches accessor methods solely based on name and
  return type, and has no way of ensuring that a method named
  similarly to a getter is idempotent and does not actually
  perform side effects in its body.


Writing to Java beans
---------------------

Dual to reading, Chimney supports transforming types into Java beans.

Chimney considers as bean a class that:

- primary constructor is public and parameter-less
- contains at least one, single-parameter setter method that returns ``Unit``

Chimney will then require data sources for all such setters.

.. code-block:: scala

  class MyBean {
    private var id: Long = _
    private var name: String = _
    private var flag: Boolean = _

    def getId: Long = id
    def setId(id: Long): Unit = { this.id = id }

    def getName: String = name
    def setName(name: String): Unit = { this.name = name }

    def isFlag: Boolean = flag
    def setFlag(flag: Boolean): Unit = { this.flag = flag }
  }

The conversion works if you explicitly enable it with ``.enableBeanSetters``.

.. code-block:: scala

  val obj = MyCaseClass(10L, "beanie", true)
  val bean = obj
    .into[MyBean]
    .enableBeanSetters
    .transform

Chimney generates code equivalent to:

.. code-block:: scala

  val bean = new MyBean
  bean.setId(obj.id)
  bean.setName(obj.name)
  bean.setFlag(obj.flag)

Current limitations
-------------------

Currently it's not possible to override or provide values
for missing setters.

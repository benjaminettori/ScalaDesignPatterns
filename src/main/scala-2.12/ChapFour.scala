import scala.collection.mutable

/**
  * Created by bettori on 6/18/2017.
  */
object ChapFour {

  // Generics

  trait Adder {
    // Can think of implicit parameter as where condition in C# generics (where T: class, for instance)
    def sum[T] (a: T, b: T)(implicit numeric: Numeric[T]): T = {
    //def sum[T <: Numeric[T]](a: T, b: T): T = {
      // numeric is the IntIsIntegral object for ints (Numeric.scala).
      // scala defines an implicit object to use for Numeric[T] when a and b are Int
      // If that didn't exist, could supply T => Numeric[T] implicit conversion method instead of implicit parameter
      // IntIsIntegral trait provides all Int operations such as plus, minus, etc...
      // IntIsIntegral is an implementation of Numeric[Int]
      // It allows generic numeric operations for Int

      // There are corresponding Float implementations

      numeric.plus(a, b)
    }
  }

  class Container[T] (data: T) {
    def compare(other: T) = data.equals(other)
  }

  object GenericsExamples extends Adder {
    def main(args: Array[String]): Unit = {
      System.out.println(s"1 + 3 = ${sum(1, 3)}")

      System.out.println(s"1.2 + 6.7 = ${sum(1.2, 6.7)}")

      // This will not work because String does not map to Numeric
      // There is no implicit value for type String

      // System.out.println(s"abc + cde = ${sum("abc", "cde")}")

      var stringContainer = new Container("abc")
      System.out.println(stringContainer.compare("cde"))
    }
  }


  /// Abstract Types

  trait ContainerAT {
    type T
    val data: T

    def compare(other: T) = data.equals(other)
  }

  class StringContainer(val data: String) extends ContainerAT {
    // override type to set it in class
    override type T = String
  }

  object AbstractTypesExample {
    def main(args: Array[String]) : Unit = {
      val stringContainer = new StringContainer("some text")
      System.out.println(stringContainer.compare("some text"))
    }
  }

  // Generic vs. abstract types
  abstract class PrintData
  abstract class PrintMaterial
  abstract class PrintMedia

  trait Printer {
    type Data <: PrintData
    type Material <: PrintMaterial
    type Media <: PrintMedia

    def print(data: Data, material: Material, media: Media) = {
      s"Printing $data with $material material on $media media"
    }
  }

  case class Paper() extends PrintMedia
  case class Air() extends PrintMedia
  case class Text() extends PrintData

  case class Model() extends PrintData
  case class Toner() extends PrintMaterial
  case class Plastic() extends PrintMaterial

  class LaserPrinter extends Printer {
    type Media = Paper
    type Data = Text
    type Material = Toner
  }

  class ThreeDPrinter extends Printer {
    type Media = Air
    type Data = Model
    type Material = Plastic
  }

  object PrinterExample {
    def main(args: Array[String]): Unit = {
      val laser = new LaserPrinter
      val threeD = new ThreeDPrinter

      System.out.println(laser.print(Text(), Toner(), Paper()))
      System.out.println(threeD.print(Model(), Plastic(), Air()))
    }
  }

  // Generic
  trait GenericPrinter[Data <: PrintData, Material <: PrintMaterial, Media <: PrintMedia] {
    def print(data: Data, material: Material, media: Media) = {
      s"Printing $data with $material material on $media media."
    }
  }

  class GenericLaserPrinter[Data <: Text, Material <: Toner, Media <: Paper] extends GenericPrinter[Data, Material, Media]
  class GenericThreeDPrinter[Data <: Model, Material <: Plastic, Media <: Air] extends GenericPrinter[Data, Material, Media]

  object GenericPrinterEx {
    def main(args: Array[String]): Unit = {

      // sub-typed generic
      val genericLaser = new GenericLaserPrinter[Text, Toner, Paper]
      System.out.println(genericLaser.print(Text(), Toner(), Paper()))

      // more generic
      class GenericPrinterImpl[Data <: PrintData, Material <: PrintMaterial, Media <: PrintMedia] extends GenericPrinter[Data, Material, Media]

      val genericPrinter = new GenericPrinterImpl[Text, Toner, Paper]

      System.out.println(genericPrinter.print(Text(), Toner(), Paper()))
    }
  }

  //Ad hoc polymorphism
  object AdderObj {
    trait AdderAdhoc[T] {
      def sum(a: T, b: T): T
    }

    // [T : AdderAdhoc] is syntactic sugar for
    // sum[T](a: T, b: T)(implicit x: AdderAdhoc[T])
    def sum[T: AdderAdhoc](a: T, b: T): T = {
      // implicitly means convert T to AdderAdhoc[T]
      implicitly[AdderAdhoc[T]].sum(a, b)
    }

    // need to tell program how to convert Int to Adder[Int]
    implicit val int2Adder: AdderAdhoc[Int] = new AdderAdhoc[Int] {
      override def sum(a: Int, b: Int) : Int = a + b
    }

    implicit val string2Adder: AdderAdhoc[String] = new AdderAdhoc[String] {
      override def sum(a: String, b: String): String = s"$a concatenated with $b"
    }

    // for all numeric types
    implicit def numeric2Adder[T: Numeric]: AdderAdhoc[T] = new AdderAdhoc[T] {
      override def sum(a: T, b: T): T = implicitly[Numeric[T]].plus(a, b)
    }
  }

  object AdhocPoly {
    def main(args: Array[String]) : Unit = {
      // can only do this if int2Adder object above is defined
      System.out.println(s"The sum of 1 and 2 is ${AdderObj.sum(1,2)}")

      System.out.println(s"The sume of abc + def is ${AdderObj.sum("abc", "def")}")

      // numeric implementation
      System.out.println(s"The sum of 1.3 and 2.6 is ${AdderObj.sum(1.3,2.6)}")

    }
  }

  // Self types

  trait Persister[T] {
    // we are pulling in Database[T] functionality
    // that way we can use the save() method from Database[T]
    self: Database[T] =>
      def persist(data: T): Unit = {
        System.out.println("Calling persist")
        self.save(data)
      }
  }

  trait Database[T] {
    def save(data: T)
  }

  trait MemoryDatabase[T] extends Database[T] {
    val db: mutable.MutableList[T] = mutable.MutableList.empty

    override def save(data: T): Unit = {
      System.out.println("Saving to in memory database")
      db.+=:(data)
    }
  }

  trait FileDatabase[T] extends Database[T] {
    override def save(data: T): Unit = {
      System.out.println("Saving to file.")
    }
  }

  // class must pull in both Persister[T] and Database[T] with self type
  class FilePersister[T] extends Persister[T] with FileDatabase[T]
  class MemoryPersister[T] extends Persister[T] with MemoryDatabase[T]

  object PersisterExample {
    def main(args: Array[String]): Unit = {
      val filePersister = new FilePersister[String]
      val memoryPersister = new MemoryPersister[Int]

      filePersister.persist("Something")
      memoryPersister.persist(10)
    }
  }




  def main(args: Array[String]): Unit = {
    GenericsExamples.main(Array())
    AbstractTypesExample.main(Array())
    PrinterExample.main(Array())
    GenericPrinterEx.main(Array())
    AdhocPoly.main(Array())
    PersisterExample.main(Array())
  }

}

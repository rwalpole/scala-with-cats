import scala.util.Try

/*
Chapter 4: Monads
Informally a monad is anything with a constructor and a flatMap method for example Option, List and Future
In Scala for comprehensions are intended for processing monads
However there is no Monad supertype in the Scala library which Cats helps to resolve
In simple terms a monad is: a mechanism for sequencing computations
 */

def parseInt(str: String): Option[Int] = Try(str.toInt).toOption

def divide(a: Int, b: Int): Option[Int] = if(b==0) None else Some(a/b)

// fail-fast error handling: a None at any step results in a None overall
def stringDivideBy(aStr: String, bStr: String): Option[Int] = {
  parseInt(aStr).flatMap{aNum =>
    parseInt(bStr).flatMap{bNum =>
      divide(aNum, bNum)
    }
  }
}

stringDivideBy("6","2")
stringDivideBy("6","0")
stringDivideBy("6","foo")
stringDivideBy("bar","2")


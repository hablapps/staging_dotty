import scala.language.implicitConversions
import scala.quoted._
import scala.quoted.staging.{run, withQuoteContext, Toolbox}

import org.junit.Test
import org.junit.Assert._

/**
Shifting the Stage
Staging with Delimited Control
https://dl.acm.org/doi/pdf/10.1145/1480945.1480962

A Monadic Approach for Avoiding Code Duplication when
Staging Memoized Functions
https://dl.acm.org/doi/pdf/10.1145/1111542.1111570
*/


class ShiftStage {
  // Needed to run or show quotes

  given Toolbox = Toolbox.make(getClass.getClassLoader)

  // TIMED

  import java.util.concurrent.TimeUnit

  def timedAux[A](code: => A, times: Int = 1): (A, Long) = {
      def aux(code: => A): (A, Long) = {
          val start = System.nanoTime
          val result: A = code
          val end = System.nanoTime
          (result, end-start)
      }

      val (a, time) = aux(code)
      val avg = (time :: List.fill(times-1)(aux(code)).map(_._2)).reduce(_ + _) / times
      (a, avg)
  }

  implicit class TimeOp[A](code: => A){
    def timed: (A, Long) =
        timedAux(code, 5)
    def timed(times: Int = 1): (A, Long) =
        timedAux(code, times)
  }

  implicit class ShowTimes[A](result: (A, Long)){

    def millis: A = {
        val mill = TimeUnit.NANOSECONDS.toMillis(result._2)
        println(s"$mill millis")
        result._1
    }

    def nanos: A = {
        println(s"${result._2} nanos")
        result._1
    }
  }

  // FIX

  val table = collection.mutable.Map[Any,Any]()

  def memoizedFix[K, V](f: (=> (K => V)) => (K => V))(n: K): V = {
    lazy val a: K => V = f(a)
    if (!table.contains(n)) table += n -> a(n)
    table(n).asInstanceOf[V]
  }

  def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }



  @Test def rawGib(): Unit = {
    def gib(x: Int, y: Int)(n: Int): Int =
      if (n==0) x
      else if (n==1) y
      else gib(x,y)(n-1) + gib(x,y)(n-2)

    assertEquals(gib(1,1)(3), 3)

    println(gib(1,1)(40).timed.millis)
  }

  @Test def fixedGib(): Unit = {

    def gibopen(x: Int, y: Int)(self: => (Int => Int))(n: Int): Int =
      if (n == 0) x
      else if (n == 1) y
      else self(n-1) + self(n-2)

    def gibfix(x: Int, y: Int): Int => Int =
      fix(gibopen(x,y))

    assertEquals(gibfix(1,1)(3), 3)
  }

  @Test def memoizedGib(): Unit = {

    def gibopen(x: Int, y: Int)(self: => (Int => Int))(n: Int): Int =
      if (n == 0) x
      else if (n == 1) y
      else self(n-1) + self(n-2)

    def gibfix(x: Int, y: Int): Int => Int =
      memoizedFix(gibopen(x,y))

    assertEquals(memoizedFix(gibopen(1,1))(3), 3)

    println(gibfix(1,1)(40).timed.millis)
  }

  @Test def naiveStagedGib(): Unit = {

    def stagedGib(x: Expr[Int], y: Expr[Int])(n: Int)(given QuoteContext): Expr[Int] =
      if (n==0) x
      else if (n==1) y
      else '{${stagedGib(x,y)(n-1)} + ${stagedGib(x,y)(n-2)}}

    withQuoteContext{
      println(stagedGib(Expr(3), Expr(4))(6).show)
    }

    def sGib(x: Int, y: Int)(n: Int): Int =
      run(stagedGib(Expr(x), Expr(y))(n))

    assertEquals(sGib(1,1)(3), 3)
  }

  @Test def scopeExtrusion: Unit = {

    withQuoteContext{
      var code: Expr[Int] = Expr(1)
      val f: Expr[Int => Unit] = '{y => {${code=Expr(2); code}; ()}}
      println(s"Code: ${code.show}")
    }
  }

  @Test def letInsertion: Unit = {

    def stagedOpenGib(x: Expr[Int], y: Expr[Int])(given QuoteContext)(
        self: => (Int => Expr[Int]))(n: Int): Expr[Int] =
      if (n==0) x
      else if (n==1) y
      else '{${self(n-1)} + ${self(n-2)}}

    def stagedGibfix(x: Expr[Int], y: Expr[Int])(given QuoteContext): Int => Expr[Int] =
      memoizedFix[Int, Expr[Int]](stagedOpenGib(x,y))

    withQuoteContext{
      println(stagedGibfix(Expr(3), Expr(4)).apply(6).show)
    }

    def sGib(x: Int, y: Int)(n: Int): Int =
      run(stagedGibfix(Expr(x), Expr(y))(n))

    assertEquals(sGib(1,1)(3), 3)
  }

  @Test def monadicGib: Unit = {

    def monadicGib(x: Expr[Int], y: Expr[Int])(given QuoteContext)(
        self: => (Int => Expr[Int]))(n: Int): Expr[Int] =
      if (n==0) x
      else if (n==1) y
      else '{${self(n-1)} + ${self(n-2)}}

    def stagedGibfix(x: Expr[Int], y: Expr[Int])(given QuoteContext): Int => Expr[Int] =
      memoizedFix[Int, Expr[Int]](stagedOpenGib(x,y))

    withQuoteContext{
      println(stagedGibfix(Expr(3), Expr(4)).apply(6).show)
    }

    def sGib(x: Int, y: Int)(n: Int): Int =
      run(stagedGibfix(Expr(x), Expr(y))(n))

    assertEquals(sGib(1,1)(3), 3)
  }

}

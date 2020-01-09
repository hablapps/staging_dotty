import scala.language.implicitConversions
import org.junit.Test
import org.junit.Assert._

import TestMacrosDef._

import scala.quoted._
import scala.quoted.staging.{run, withQuoteContext, Toolbox}

class TestMacros{
  given Toolbox = Toolbox.make(getClass.getClassLoader)

  @Test def t1(): Unit = {
    println(run(andsImpl('{true && false && true && false})))
    println(ands(true && true && true && true))
    // println(ands(true && false && true))
    // println(ands(true && false))
    // assertEquals(ands(true && false && true), 4)
  }

  @Test def poly(): Unit  = {

    val f: [A, B] => (given Type[A], Type[B]) => (A => B) => A => B =
      [A, B] => (given tA: Type[A], tB: Type[B]) => (f: A => B) => (a: A) => f(a)

    def implE[A: Type, B: Type](f: Expr[A => B], a: Expr[A])(given QuoteContext): Expr[B] =
      '{$f($a)}

    // polyValues('{ })
  }

  @Test def curry(): Unit = {
    withQuoteContext(println('{[A, B] => (f: A => B) => (a: A) => f(a)}.show))
    // run(curryHowardImpl('{[A, B] => (f: A => B) => (a: A) => f(a)}))
    // println(run(curryHowardImpl1('{(f: Int => String) => (a: Int) => f(a)})))
    println(run(curryHowardImpl1('{(f: String) => 0})))


    def f: (given Int) => String => Int = (given i: Int) => s => s.length
    val g: (given Boolean) => Int => Boolean = (given b: Boolean) => i => i>0
    // withQuoteContext(println('{${Expr(f)}}))


  }
}

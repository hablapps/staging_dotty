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

}

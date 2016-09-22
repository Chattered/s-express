// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Copyright (c) 2014-2016 University of Edinburgh
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package org.ensime.sexp

import collection.breakOut
import scala.collection.immutable.ListMap
import scalaz._
import Scalaz._

/**
 * An S-Expression is either
 *
 * 1. an atom (i.e. symbol, string, number)
 * 2. of the form `(x . y)` where `x` and `y` are S-Expressions (i.e. cons)
 *
 * Everything else is just sugar.
 */
sealed abstract class Sexp {
  //  override def toString = compactPrint
  def compactPrint = SexpCompactPrinter(this)
  def prettyPrint = SexpPrettyPrinter(this)

  def convertTo[T](implicit reader: SexpReader[T]): T = reader.read(this)

  private[sexp] def isList: Boolean = false
}

final case class SexpCons(x: Sexp, y: Sexp) extends Sexp {
  private[sexp] override val isList = y.isList
}

sealed trait SexpAtom extends Sexp
final case class SexpChar(value: Char) extends SexpAtom
final case class SexpString(value: String) extends SexpAtom
final case class SexpNumber(value: BigDecimal) extends SexpAtom
final case class SexpSymbol(value: String) extends SexpAtom
case object SexpNil extends SexpAtom {
  private[sexp] override def isList = true
}
// https://www.gnu.org/software/emacs/manual/html_node/elisp/Float-Basics.html
case object SexpPosInf extends SexpAtom
case object SexpNegInf extends SexpAtom
case object SexpNaN extends SexpAtom

object SexpNumber {
  def apply(n: Int) = new SexpNumber(BigDecimal(n))
  def apply(n: Long) = new SexpNumber(BigDecimal(n))
  def apply(n: Double) = n match {
    case _ if n.isNaN => SexpNil
    case _ if n.isInfinity => SexpNil
    case _ => new SexpNumber(BigDecimal(n))
  }
  def apply(n: BigInt) = new SexpNumber(BigDecimal(n))
  def apply(n: String) = new SexpNumber(BigDecimal(n))
  def apply(n: Array[Char]) = new SexpNumber(BigDecimal(n))
}

/** Sugar for ("a" . ("b" . ("c" . nil))) */
object SexpList {
  def apply(els: Sexp*): Sexp = apply(els.toList)

  def apply(els: List[Sexp]): Sexp = els.foldRight(SexpNil: Sexp) {
    case (head, tail) => SexpCons(head, tail)
  }

  def unapply(sexp: Sexp): Option[List[Sexp]] =
    if (!sexp.isList) None
    else {
      def rec(s: Sexp): List[Sexp] = s match {
        case SexpNil => Nil
        case SexpCons(car, cdr) => car :: rec(cdr)
        case _ => throw new IllegalStateException("Not a list: " + s)
      }
      val res = rec(sexp)
      if (res.isEmpty) None
      else Some(res)
    }
}

/**
 * Sugar for (:k1 v1 :k2 v2)
 * [keyword symbols](https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Type.html):
 *
 * `SexpData` is defined as a type alias in `package.scala`
 */
object SexpData {
  def apply(kvs: (SexpSymbol, Sexp)*): Sexp = apply(kvs.toList)

  def apply(kvs: List[(SexpSymbol, Sexp)]): Sexp =
    if (kvs.isEmpty)
      SexpNil
    else {
      val mapped = ListMap(kvs: _*)
      require(mapped.size == kvs.size, "duplicate keys not allowed: " + mapped.keys)
      require(mapped.keys.forall(_.value.startsWith(":")), "keys must start with ':' " + mapped.keys)
      SexpList(kvs.flatMap { case (k, v) => k :: v :: Nil }(breakOut): List[Sexp])
    }

  def unapply(sexp: Sexp): Option[SexpData] = sexp match {
    case SexpList(values) =>
      // order can be important in serialised forms
      val props = {
        values.grouped(2).collect {
          case List(SexpSymbol(key), value) if key.startsWith(":") =>
            (SexpSymbol(key), value)
        }
      }.foldLeft(ListMap.empty[SexpSymbol, Sexp]) {
        case (res, el) =>
          // in elisp, first entry wins
          if (res.contains(el._1)) res else res + el
      }
      // props.size counts unique keys. We only create data when keys
      // are not duplicated or we could introduce losses
      if (values.isEmpty || 2 * props.size != values.size)
        None
      else
        Some(props)

    case _ => None
  }
}

object Sexp {
  implicit def SexpIsOrder: Order[Sexp] = new Order[Sexp] {
    def order(x: Sexp, y: Sexp): Ordering = (x,y) match {
      case (SexpChar(c1), SexpChar(c2)) => c1 ?|? c2
      case (SexpChar(_), _) => Ordering.LT
      case (_, SexpChar(_)) => Ordering.GT
      case (SexpString(s1), SexpString(s2)) => s1 ?|? s2
      case (SexpString(_), _) => Ordering.LT
      case (_, SexpString(_)) => Ordering.GT
      case (SexpNumber(m), SexpNumber(n)) => m ?|? n
      case (SexpNumber(_), _) => Ordering.LT
      case (_, SexpNumber(_)) => Ordering.GT
      case (SexpSymbol(s1), SexpSymbol(s2)) => s1 ?|? s2
      case (SexpSymbol(_), _) => Ordering.LT
      case (_, SexpSymbol(_)) => Ordering.GT
      case (SexpNil, SexpNil) => Ordering.EQ
      case (SexpNil, _) => Ordering.LT
      case (_, SexpNil) => Ordering.GT
      case (SexpPosInf, SexpPosInf) => Ordering.EQ
      case (SexpPosInf, _) => Ordering.LT
      case (_, SexpPosInf) => Ordering.GT
      case (SexpNegInf, SexpNegInf) => Ordering.EQ
      case (SexpNegInf, _) => Ordering.LT
      case (_, SexpNegInf) => Ordering.GT
      case (SexpNaN, SexpNaN) => Ordering.EQ
      case (SexpNaN, _) => Ordering.LT
      case (_, SexpNaN) => Ordering.GT
      case (SexpCons(car1, cdr1), SexpCons(car2, cdr2)) =>
        (car1 ?|? car2) |+| (cdr2 ?|? cdr2)
      case (SexpCons(_, _), _) => Ordering.LT
      case (_, SexpCons(_, _)) => Ordering.GT
    }
  }
}

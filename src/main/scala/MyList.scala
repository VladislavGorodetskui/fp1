package com.korolyagaylo.fplab1

import scala.annotation.tailrec
import MyList.*

enum MyList[+A]:
  case MyNil
  case MyCons(hd: A, tl: MyList[A])

  override def toString: String =
    @scala.annotation.tailrec
    def go(sb: StringBuilder, as: MyList[A]): String = {
      as match {
        case MyNil =>
          sb.result
        case MyCons(h, t) =>
          go(
            sb
              .append(h)
              .append(if t == MyNil then "]" else ", "),
            t
          )
      }
    }
    go(new StringBuilder("["), this)

object MyList:
  def apply[A](xs: A*): MyList[A] = of(xs*)
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }


def spans[A](xs: MyList[A]): MyList[MyList[A]] =
  def go(xs: MyList[A], acc: MyList[A], n: A): MyList[MyList[A]] =
    xs match
      case MyNil => MyCons(acc, MyNil)
      case MyCons(hd, tl) =>
        if (hd == n)
          go(tl, MyCons(hd, acc), hd)
        else
          MyCons(acc, go(tl, MyCons(hd, MyNil), hd))
  xs match
    case MyNil => MyNil
    case MyCons(hd, tl) => go(xs, MyNil, hd)

//def groupBy[A,B](xs: MyList[A], f: A => B): Map[B, MyList[A]] =
//  @tailrec
//  def revers[A](xs: MyList[A], ys: MyList[A] = MyNil): MyList[A] =
//    xs match
//      case MyNil => ys
//      case MyCons(hd, tl) => revers(tl, MyCons(hd, ys))
//  @tailrec
//  def go(xs: MyList[A], f: A => B, map: Map[B, MyList[A]]): Map[B, MyList[A]] =
//    xs match
//      case MyNil => map
//      case MyCons(hd, tl) =>
//        go(tl, f, map.updated(f(hd), MyCons(hd, map.getOrElse(f(hd), MyNil))))
//  go(revers(xs), f, Map.empty)

def groupBy[A,B](xs: MyList[A], f: A => B): Map[B, MyList[A]] =
  xs match
    case MyNil => Map.empty
    case MyCons(hd, tl) =>
      val will = groupBy(tl, f)
      val fhd = f(hd)
      will.updated(fhd, MyCons(hd, will.getOrElse(fhd, MyNil)))

@main def run(): Unit =
  println("Hello World")
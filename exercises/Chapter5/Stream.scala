// package fpinscala.laziness

import Stream._

trait Stream[+A] {

    // Exercise 1
    def toList: List[A] = {
        @annotation.tailrec
        def go(s: Stream[A], acc: List[A]): List[A] = s match {
            case Cons(h,t) => go(t(), h() :: acc)
            case _ => acc
        }
        go(this, List()).reverse
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = { // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
        this match {
            case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
            case _ => z
        }
    }

    def exists(p: A => Boolean): Boolean = {
        foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
    }

    @annotation.tailrec
    final def find(f: A => Boolean): Option[A] = this match {
        case Empty => None
        case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }

    // Exercise 2
    def take(n: Int): Stream[A] = {
        this match {
            case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
            case Cons(h, _) if n == 1 => cons(h(), empty)
            case _ => empty
        }
    }

    // Exercise 2
    @annotation.tailrec
    final def drop(n: Int): Stream[A] = {
        this match {
            case Cons(_, t) if n > 0 => t().drop(n - 1)
            case _ => this
        }
    }

    // Exercise 3
    def takeWhile(p: A => Boolean): Stream[A] = {
        this match { 
            case Cons(h,t) if p(h()) => cons(h(), t() takeWhile p)
            case _ => empty 
        }
    }

    // Exercise 4
    def forAll(p: A => Boolean): Boolean = {
        foldRight(true)((a, b) => p(a) && b)
    }

    // Exercise 5
    def takeWhileRight(p: A => Boolean): Stream[A] = {
        foldRight(empty[A])((h, t) => if(p(h)) cons(h, t) else empty)
    }

    // Exercise 6
    def headOption: Option[A] = {
        foldRight(None: Option[A])((h, _) => Some(h))
    }

    // Exercise 7

    def mapRight[B](f: A => B): Stream[B] = {
        foldRight(empty[B])((h, t) => cons(f(h), t))
    }

    // Exercise 7

    def filterRight(f: A => Boolean): Stream[A] = {
        foldRight(empty[A])((h, t) => if(f(h)) cons(h, t) else t)
    }

    // Exercise 7

    def appendRight[B>:A](s: => Stream[B]): Stream[B] = {
        foldRight(s)((h, t) => cons(h, t))
    }

    // Exercise 7

    def flatmapRight[B](f: A => Stream[B]): Stream[B] = {
        foldRight(empty[B])((h, t) => f(h) appendRight t)
    }


    def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = Stream.cons(1, ones)
    def from(n: Int): Stream[Int] = sys.error("todo")

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}
//package fpinscala.state


trait RNG {
    def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
    // NB - this was called SimpleRNG in the book text

    case class Simple(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
            val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
            val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
            (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
        }
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    // Exercise 1

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i, r) = rng.nextInt
        (if (i < 0) -(i + 1) else i, r)
    }

    // Exercise 2

    def double(rng: RNG): (Double, RNG) = {
        val (i, r) = nonNegativeInt(rng)
        (i / (Int.MaxValue.toDouble + 1), r)
    }

    // Exercise 3

    def intDouble(rng: RNG): ((Int,Double), RNG) = {
        val (i, rng1) = rng.nextInt
        val (d, rng2) = double(rng1)
        ((i, d), rng2)
    }

    // Exercise 3

    def doubleInt(rng: RNG): ((Double,Int), RNG) = {
        val ((i, d), rngOut) = intDouble(rng)
        ((d, i), rngOut)
    }

    // Exercise 3

    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
        val (d1, rng1) = double(rng)
        val (d2, rng2) = double(rng1)
        val (d3, rng3) = double(rng2)
        ((d1, d2, d3), rng3)
    }

    // Exercise 4

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        if (count <= 0) {
            (List(), rng)
        } else {
            val (x, r1)  = rng.nextInt
            val (xs, r2) = ints(count - 1)(r1)
            (x :: xs, r2)
        }
    }

    // Exercise 5

    val doubleViaMap: Rand[Double] = {
        map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
    }

    // Exercise 6

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
            val (a, r1) = ra(rng)
            val (b, r2) = rb(r1)
            (f(a, b), r2)
        }
    }

    // Exercise 7

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
        fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
    }

    // Exercise 8

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
        rng => {
            val (a, r1) = f(rng)
            g(a)(r1) // We pass the new state along
        }
    }

    // Exercise 9

    def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
        flatMap(s)(a => unit(f(a)))

}

object State {
    def unit[S, A](a: A): State[S, A] =
        State(s => (a, s))

    // Exercise 10

    def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] = {
        sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
    }

    def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = {
        sequenceViaFoldRight(sas)
    }

    type Rand[A] = State[RNG, A]

}

case class State[S,+A](run: S => (A, S)) {

    def unit[S, A](a: A): State[S, A] =
        State(s => (a, s))

    // Exercise 10

    def map[B](f: A => B): State[S, B] = {
        flatMap(a => unit(f(a)))
    }
    
    // Exercise 10

    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
        flatMap(a => sb.map(b => f(a, b)))
    }
    
    // Exercise 10

    def flatMap[B](f: A => State[S, B]): State[S, B] = {
        State(s => {
            val (a, s1) = run(s)
            f(a).run(s1)
        })
    }
}

// Exercise 11

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
    def update = {
        (i: Input) => (s: Machine) =>
            (i, s) match {
                case (_, Machine(_, 0, _)) => s
                case (Coin, Machine(false, _, _)) => s
                case (Turn, Machine(true, _, _)) => s
                case (Coin, Machine(true, candy, coin)) =>
                    Machine(false, candy, coin + 1)
                case (Turn, Machine(false, candy, coin)) =>
                    Machine(true, candy - 1, coin)
            }
    }
}



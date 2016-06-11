// Driver script to test/demonstrate the code written in State.scala

:paste State.scala

// Exercise 1

val simpleRNG = RNG.Simple(1000)
val (n1, rng2) = RNG.nonNegativeInt(simpleRNG)
RNG.nonNegativeInt(rng2)

// Exercise 2

RNG.double(rng2)

// Exercise 3

RNG.intDouble(simpleRNG)
RNG.doubleInt(simpleRNG)
RNG.double3(simpleRNG)

// Exercise 4

RNG.ints(10)(simpleRNG)

// Exercise 5

RNG.doubleViaMap(rng2)

// Exercise 6

def addTwoInts(a: Int, b: Int): Int = {
    a + b
}

RNG.map2(RNG.unit(3), RNG.unit(5))(addTwoInts)

// Exercise 7

val listOfRands = List(RNG.unit(3), RNG.unit(3), RNG.unit(3))

RNG.sequence(listOfRands)

// Exercise 8

RNG.flatMap(RNG.unit(1))(x => RNG.unit(x + 1))

// Exercise 9

def plusTwo(a: Int): Int = {
    a + 2
}

RNG._map(RNG.unit(1))(plusTwo)


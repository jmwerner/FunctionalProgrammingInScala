:paste Stream.scala

// Exercise 1
Stream(1, 2, 3, 4).toList

// Exercise 2
Stream(1, 2, 3, 4).take(3).toList
Stream(1, 2, 3, 4).take(2).toList

Stream(1, 2, 3, 4).drop(3).toList
Stream(1, 2, 3, 4).drop(2).toList

// Exercise 3

// Function for testing, assume we're always passing it an integer 
def isGreaterThanFive(a: Int): Boolean = {
    if(a > 5) { 
        true
    } else {
        false
    }
}

Stream(1, 2, 3, 4, 5, 6, 7, 8).takeWhile(isGreaterThanFive).toList

Stream(10, 6, 3, 2).takeWhile(isGreaterThanFive).toList

// Exercise 4
Stream(1, 2, 3, 4).forAll(isGreaterThanFive)

Stream(10, 11, 112).forAll(isGreaterThanFive)

// Exercise 5
Stream(1, 2, 3, 4, 5, 6, 7, 8).takeWhileRight(isGreaterThanFive).toList

Stream(10, 6, 3, 2).takeWhileRight(isGreaterThanFive).toList

// Exercise 6
Stream(1, 2, 3).headOption

Stream(10, 20, 30, 50).headOption

// Exercise 7

// Another function for testing. Assume it's always getting an integer, ok? 
def timesTwo(a: Int): Int = {
    a * 2
}

def timesTwoStream(a: Int): Stream[Int] = {
    Stream(a * 2)
}


Stream(10, 20, 30).mapRight(timesTwo).toList

Stream(1, 4, 6, 8).filterRight(isGreaterThanFive).toList

Stream(1, 2, 3).appendRight(Stream(10)).toList

Stream(1, 2, 3).flatmapRight(timesTwoStream).toList

// Exercise 8

Stream(1, 2, 3).constant(10)

// Exercise 9

Stream(1, 2, 3, 4, 4).from(2)

// Exercise 10

Stream.fibs.take(10).toList

// Exercise 11








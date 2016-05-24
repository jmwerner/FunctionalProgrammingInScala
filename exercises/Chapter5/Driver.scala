:paste Stream.scala

// Exercise 1
Stream(1,2,3,4).toList

// Exercise 2
Stream(1,2,3,4).take(3).toList
Stream(1,2,3,4).take(2).toList

Stream(1,2,3,4).drop(3).toList
Stream(1,2,3,4).drop(2).toList

// Exercise 3

// Function for testing, assume we're always passing it an integer 
def logicalFunction(a: Int): Boolean = {
    if(a > 5) { 
        true
    } else {
        false
    }
}

Stream(1,2,3,4,5,6,7,8).takeWhile(logicalFunction).toList

Stream(10,6,3,2).takeWhile(logicalFunction).toList



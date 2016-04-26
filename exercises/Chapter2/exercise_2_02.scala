def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
        if (n >= (as.length - 1)) {
            true
        } else {
            if(ordered(as(n), as(n + 1))){
                loop(n + 1)
            } else {
                false
            }
        }
    }
    loop(0)
}


// Testing section

def functionForOrdering(a: Int, b: Int): Boolean = {
    a < b
}

val array1 = Array(1,2,3,4,5,6)
val array2 = Array(1,2,5,3,2,4)

isSorted(array1, functionForOrdering)

isSorted(array2, functionForOrdering)



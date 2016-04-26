
def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
}

// Illustration using previous function

def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a,b)
}

def testFunction(a: Int, b: Int): Int = {
    a + b
}

def curryFunction = curry(testFunction)


def uncurriedFunction = uncurry(curryFunction)

uncurriedFunction(10,20)
// woo hoo! 

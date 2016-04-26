def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a,b)
}

// Illustration
def testFunction(a: Int, b: Int): Int = {
    a + b
}

def curryFunction = curry(testFunction)

def curryFunction2 = curryFunction(5)

curryFunction2(10)


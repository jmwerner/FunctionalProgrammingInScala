
def compose[A,B,C](f: B => C, g: A => B): A => C = {
    x => f(g(x))
}

// Illustration
def plusOne(a: Int): Int = {
    a + 1
}

def timesTwo(a: Int): Int = {
    a * 2
}

def compositionOne = compose(plusOne, timesTwo)

// 5 * 2 + 1
compositionOne(5)

def compositionTwo = compose(timesTwo, plusOne)

// (5 + 1) * 2
compositionTwo(5)

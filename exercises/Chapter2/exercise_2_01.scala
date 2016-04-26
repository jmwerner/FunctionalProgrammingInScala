
// Returns the nth fibonacci number (starting with 0, weirdly)

def fib(n: Int): Int = {
    @annotation.tailrec
    def g(n: Int): Int = 
        if(n <= 1) {
            0
        } else {
            if(n == 2) {
                1
            } else {
                g(n-1) + g(n-2)
            } 
        }
    g(n)
}

fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)
fib(7)


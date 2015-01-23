## Implementation of a caching wrapper for matrix inversion

## Matrix inversion is an expensive operation under general conditions;
## caching the result can boost performance of composite algorithms
## when the same inverse is needed a sufficient number of times.
## This is also a "design pattern" known as memoization:
##   http://en.wikipedia.org/wiki/Memoization,
## which itself is a specific type of wrapper or decorator pattern:
##   http://en.wikipedia.org/wiki/Decorator_pattern.

## A typical lifecycle might be:
##   x <- matrix(...)
##   m <- makeCacheMatrix(x)
##   y <- cacheSolve(m)
## ...or any sequence of operations with the above three lines interspersed
## `randomly' (stochastically, perhaps), but which serves a useful purpose.

## makeCacheMatrix returns a list of accessor & functions

makeCacheMatrix <- function(x = matrix()) {

    # Initialize the inverse matrix to NULL
    # note that x is initialized in the original call to makeCachematrix,
    # so that this line below completes the initial set() operation:
    inv <- NULL

    # Define the setter function that sets the input matrix x to a new value;
    # this replaces the initial x value passed in and re-initalizes
    # inv to NULL (hopefully only if necessary, if I get productive).
    set <- function(y) {
        # The cached inverse must be reset to NULL iff x is redefined (differently).
        # This potential optimization must, however, rely on a tolerance/threshold/
        # epsilon/radius, or lower limiting value, to determine if y is sufficiently close
        # to the previous matrix x; below, we use the L2 matrix norm with a hard-
        # coded threshold of 10^(-15); if we were willing to store an `eps' internally,
        # we would probably do best to calculate it in setinv below as indicated there.
        #
        # This optimized version resets inv only if y has changed `enough':
        #inv <<- if (is.matrix(x) &&  is.matrix(x)
        #            && nrow(x) == nrow(y)
        #            && ncol(x) == ncol(y)
        #            && norm(x-y) < 1e-15) {inv} else {NULL}
        inv <<- NULL
        # re-define x
        x <<- y
    }

    # Define the getter function to return the current input matrix.
    get <- function() x

    # Define the setter and getter functions for the output matrix;
    # setinv() must be called by the function that actually does the heavy lifting.
    setinv <- function(z) inv <<- z
    getinv <- function() inv

    # Here is a potential optimization to go with the one above,
    # with an internal `eps'; a more rigorous approach would surely take
    # into account not only the limiting error in doubles, but also the
    # condition number of x and/or its inverse.
    # But as a first approach, we use here an estimate of the machine error
    # in the representation of x & its inverse, as calculated from the
    # Euclidean L2 norm of the difference of x*inv with the identity matrix
    # (in practice, a suitable multiple of this might need to be chosen).
    # This would replace the simpler setinv above.
    #
    #setinv <- function(z) {
    #  eps <<-norm(diag(nrow(x))-x*inv,'2')
    #  inv <<- z
    #}

    # Return the four accessor functions that comprise this wrapper (this
    # makes them `public' functions, i.e. named elements visible with `$').
    list( set = set
        , get = get
        , setinv = setinv
        , getinv = getinv )
}


## Calculate and cache the inverse of matrix x on the first call,
## but retrieve and return the cached inverse on subsequent calls.
## The first argument is the object returned from above function makeCacheMatrix;
## subsequent arguments are passed on to the heavy lifter, solve().

cacheSolve <- function(x, ...) {
    ## Return the inverse of matrix x (assumed invertible)
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("returning cached inverse")
        return(inv)
    }
    message("calculating & caching inverse")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## Implementation of a caching wrapper for matrix inversion
##
## Matrix inversion is an expensive operation under general conditions;
## caching the result can boost performance of composite algorithms
## when the same inverse is needed a sufficient number of times.
## This is also a "design pattern" known as memoization:
##   http://en.wikipedia.org/wiki/Memoization,
## which itself is a specific type of wrapper or decorator pattern:
##   http://en.wikipedia.org/wiki/Decorator_pattern.
##

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
        # the cached inverse must be reset to NULL iff x is redefined (differently)
        # TODO: test whether this works for 2nd, 3rd, ... redefinitions
        #       ...or whether it can be modified to work using get()...
        #inv <<- if (x == y) {inv} else {NULL}
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
    inv <- x$getmean()
    if(!is.null(inv)) {
        message("returning cached inverse")
        return(inv)
    }
    message("calculating & caching inverse")
    data <- x$get()
    inv <- solve(data, ...)
    x$setmean(inv)
    inv
}

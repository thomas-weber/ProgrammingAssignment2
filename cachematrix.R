## The following two functions create an object for storing and retrieving a
## matrix and its inverse in an optimised manner by only calculating the
## inverse if it is really needed (i.e. if the underlying matrix has changed)

## makeCacheMatrix constructs an object with 4 functions for setting and
## getting a matrix and its inverse. It implements a cache for the inverse.
## cacheSolve calculates the inverse of the matrix in the object created by
## makeCacheMatrix(). The inverse is cached and only calculated if needed.


## This function creates a special "matrix" object that can cache its inverse.
## The object consists of a list with 4 functions, the data is cached inside
## the functions' environment.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # the inverse of the matrix

    # set(): replace the previous matrix with the new matrix,
    # but do not recalculate the inverse. Instead, the inverse is set to NULL
    # to show the need for a  recalculation.
    set <- function(new.matrix) {
        x <<- new.matrix
        inverse <<- NULL
    }

    # get(): return the cached matrix
    get <- function() x

    # set.inverse(): set the new matrix inverse, calculated elsewhere
    set.inverse <- function(new.inverse) inverse <<- new.inverse

    # get.inverse(): return the inverse matrix
    get.inverse <- function() inverse

    # List with the functions, to be returned
    list(set = set, get = get, set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## This function returns the inverse of the matrix in the matrix "object" x.
## If the inverse has already been calculated, is is being taken from the
## cache of x. Otherwise, the function calculates the inverse, places it in the
## cache and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # get inverse from the object and check if it has already been calculated
    inverse <- x$get.inverse()
    if (!is.null(inverse)) {
        message("Inverse already calculated, getting cached data")
        return (inverse)
    }

    # If we reach this point, the inverse was not calculated, so we have to
    # calculate it

    # get the original matrix
    original.matrix <- x$get()

    # calculate the inverse (it has not been calculated before)
    inverse <- solve(original.matrix)

    # copy the calculated inverse back into the matrix "object", so that the
    # next call to get.inverse() will retrieve the cached object
    x$set.inverse(inverse)

    # return the calculated inverse
    inverse
}

## Test function that runs our code (optional)
testCache <- function() {
    # test matrix and its inverse
    x <- matrix(c(1,2,3,4), nrow=2, ncol=2)
    x.inv <- solve(x)

    # create object with x and fill the cache
    obj <- makeCacheMatrix(x)
    obj.inv <- cacheSolve(obj)

    # check that the original matrix is in the cache
    if (!identical(x, obj$get())) message("Error")
    # check that the calculated inverse is correct
    if (!identical(x.inv, obj.inv)) message("Error")
    # check that the inverse is in the cache
    if (!identical(x.inv, obj$get.inverse())) message("Error")

    # Fill the object again, with the same matrix x (yes, I am lazy)
    obj$set(x)
    # check that the original matrix is in the cache
    if (!identical(x, obj$get())) message("Error")
    # Check that the cache has been invalidate
    if (!is.null(obj$get.inverse())) message("Error")
}

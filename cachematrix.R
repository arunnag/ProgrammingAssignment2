## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix(x) creates and returns a cacheable matrix object
## implements get(), set(x) , getInv(), setInv(inverse)
## set(x) set updated the new matrix and cleares the cached matrix inverse so that it is recomputed for the new matrix
## get(x) gets the cahced matrix
## getInv() gets the cached inverse for the matrix and NULL when it is not computed yet
## setInv(inverse) cahces the computed inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() {
                x
        }

        getInv <- function() {
                inv
        }

        setInv <- function(inverse) {
                inv <<- inverse
        }

        list(set = set, get = get, getInv = getInv, setInv = setInv)
}

## cacheSolve(x) takes a cacheable matrix object and computes/returns it's inverse
## if the inverse is not computed yet(getInv() returns NULL) it computes the inverse and caches it in cacheable matrix object
## if the inverse is computed already it returns the cahced inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()

        if (!is.null(inv)) {
                #message("Retriving cached inverse")
                return (inv)
        }

        #message("Computing inverse and caching it")
        # null implies that inverse is not computed/invalid for this matrix
        mat <- x$get()
        inv <- solve(mat)
        x$setInv(inv)
        inv
}

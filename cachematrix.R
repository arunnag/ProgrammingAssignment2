## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()

        if (!is.null(inv)) {
                #message("retriving cached data")
                return (inv)
        }
        #print("computing inv")
        # null implies that inverse is not computed/invalid for this matrix
        mat <- x$get()
        inv <- solve(mat)
        x$setInv(inv)
        inv
}

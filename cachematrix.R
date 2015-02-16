## A pair of functions that can cache the inverse of a matrix



## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	inverted <- NULL

    setMatrix <- function(aMatrix) {
            x <<- aMatrix
            inverted <<- NULL
    }

    getMatrix <- function() {
    	x
    }

    setInverseMatrix <- function(inverse) {
    	inverted <<- inverse
    }

    getInverseMatrix <- function() {
    	inverted
    }

    list(set = setMatrix, get = getMatrix,
         setInverse = setInverseMatrix,
         getInverse = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.
## Assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inverted <- x$getInverseMatrix()

    if(!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }

    data <- x$getMatrix()

    inverted <- solve(data, ...)

    x$setInverseMatrix(inverted)

    inverted
}

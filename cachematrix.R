## As matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## computing it repeatedly we write a pair of functions to enable
## cacheing of the inverse of a matrix.

## Note: the code assumes that all matrices are invertiable,
## and there is no error checking to catch cases when matrices
## are univertable


## The first function creates a special vector that will allow
## us to cache the result of inverting a matrix: this function
## creates a special vector which is a list containing funtions to 
## set the value of the matrix, get the value of the matrix,
## set the inverse matrix, and get the inverse of the matrix

makeCacheMatrix <- function(matrixUninverted = matrix()) {
   # Value for storing the matrix invsere
   matrixInverse <- NULL

   # Funtion to set the value of the matrix to invert
   setMatrix <- function( y ) {
      matrixUninverted <<- y
      matrixInverse    <<- NULL
   }

   # Funciton to return the (uninverted) matrix
   getMatrix <- function () matrixUninverted

   # Function to set the inverse of the matrix
   setInverse <- function (inverse)  matrixInverse <<- inverse

   # Function to get the inverse of the matrix
   getInverse <- function ()  matrixInverse

   # Build the list to return
   list(set = setMatrix, get = getMatrix,
        setinverse = setInverse, getinverse = getInverse)
}

##
## Using the special "vector" created abouve, caluclate the inverse
## of the given matrix. However, it first checks to see if we have
## already done this calculation, and if we have return the cached result.
## If there is no cached vaule, it inverts the matrix, stores the
## value and returns the inverse
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversematrix <- x$getinverse()
    if ( !is.null(inversematrix) ) {
        message("Using cached value of inverse")
        return(inversematrix)
    }
    
    mtx <- x$get()
    inversematrix <- solve(mtx)
    x$setinverse(inversematrix)

    inversematrix
}

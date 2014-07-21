

## Put comments here that give an overall description of what your functions do
##
##      The following two functions work together to calculate, cache and serve
##	the inverse of an (assumed invertible) matrix.	


## Write a short comment describing this function
##
##      The first function provides data and functions to:
##	        - store or retrieve a matrix
##		- store or retrieve the inverse matrix
##	No function is provided here to calculate the inverse matrix.
##	These functions are returned by the function in a list, which the
##	cooperating function below may invoke.  This structure represents
##	a cache object.

makeCacheMatrix <- function (x = matrix()) {
  
        i <- NULL
  
        setmtx <- function (y) {
                x <<- y
                i <<- NULL
        }
  
        getmtx <- function () {
                x
        }
  
        setinv <- function (iota) {
                i <<- iota
        }
  
        getinv <- function () {
                i
        }
  
        list (setmtx = setmtx, getmtx = getmtx, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##
##      The second function calls functions above to:
##              - retrieve the matrix
##		- calculate the inverse using R function 'solve'
##		- store the inverse matrix
##		- return the inverse matrix
##	A subsequent query for the same matrix will:
##		- retrieve the inverse matrix from the cache object
##		- return the inverse matrix

cacheSolve <- function (v, ...) {
  
        iota <- v$getinv ()
        if (! is.null (iota)) {
                message ("getting cached data")
                return (iota)
        }
  
        data <- v$getmtx ()
        iota <- solve (data, ...)
  
        v$setinv (iota)
  
        iota
}

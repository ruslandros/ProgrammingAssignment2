## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix()
##      creates a special matrix object that can cache its its inverse
##      input  : x an invertible matrix
##      output : an inverse matrix of the original matrix
##

makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        set <- function(y) {
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x
        set_inverse <- function(inv_x) inverse_x <<- inv_x
        get_inverse <- function() inverse_x
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)   
}


## Write a short comment describing this function
##
## cacheSolve()
##      Computes the inverse of the special matrix returned by makeCacheMatrix().
##      

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inverse_x <- x$get_inverse()
        if(!is.null(inverse_x)) {
                message("getting cached data")
                return(inverse_x)
        }
        data <- x$get()
        inverse_x <- solve(data)
        x$set_inverse(inverse_x)
        inverse_x       
}

## Put comments here that give an overall description of what your
## functions do
#
## makeCacheMatrix() - creates a special matrix object with caching functionality
## cacheSolve() - computes the inverse of the special "matrix" returned by the function makeCacheMatrix(). If the inverse has already been calculated, cacheSolve() will retrieve the inverse from the cache
#
## assumption: the matrix supplied is always invertible
#
## test: a <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
##       cacheSolve(a)
#

## Write a short comment describing this function
##
## makeCacheMatrix()
##      creates a special matrix object that can cache its associated matrix
##

makeCacheMatrix <- function(x = matrix()) {

	# init cache to NULL
        cache_x <- NULL

	# called from external function
        set <- function(y) {
                x <<- y
                cache_x <<- NULL
        }

	# returns current matrix
        get <- function() x

	# explicitly cache matrix 'z'
        set_cache <- function(z) cache_x <<- z

	# returns what is in cache
        get_cache <- function() cache_x

	# list of items
        list(set = set, get = get,
             set_cache = set_cache,
             get_cache = get_cache)   
}


## Write a short comment describing this function
##
## cacheSolve()
## 	Computes the inverse of the special matrix returned by makeCacheMatrix().
##	If the inverse has already been calculated, retrieve from cache.
#      

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	# retrieve from cache
        inverse_x <- x$get_cache()
        if(!is.null(inverse_x)) {
                message("getting cached data")

		# if inverse has been calculated, returns it
                return(inverse_x)
        }

	# get current matrix to be calculated
        data <- x$get()

	# calculate inverse
        inverse_x <- solve(data)

	# store calculated inverse in cache
        x$set_cache(inverse_x)

        inverse_x       
}

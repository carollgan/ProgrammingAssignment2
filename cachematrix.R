## Functions that cache the inverse of a matrix.

## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always invertible.

## For square matrix use only (reason `solve` only handles square matrices )


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                	m <- NULL
                		set <- function(y) {
                				x <<- y
                				m <<- NULL
                		}
                		get <- function() x
                		setinverse <- function(solve) m <<- solve
                		getinverse <- function() m
                		list(set = set, get = get,
                			 setinverse = setinverse,
                			 getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
                m <- x$getinverse()
                        if(!is.null(m)) {
                                message("getting cached data")
                                return(m)
                        }
                        data <- x$get()
                        m <- solve(data, ...)
                        x$setinverse(m)
                        m
                }

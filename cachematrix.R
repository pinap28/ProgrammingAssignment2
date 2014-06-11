## A pair of functions that cache the inverse of a matrix.
## Functions:
## 1. makeCacheMatrix: creates a special "matrix" object 
##                     that can cache its inverse.
## 2. cacheSolve: computes the inverse of the special 
##                "matrix" returned by makeCacheMatrix above.

## The function makeCacheMatrix creates a list containing a
## function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the special
## "matrix" created with the makeCache Matrix function.
## It first check if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skip calculation.
## Otherwise it computes the inverse and set the value on the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m)
        x$setinverse(i)
        i
}

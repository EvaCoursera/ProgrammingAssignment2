## These functions make caching of the inverse of a matrix possible. The matrix 
## is stored in special cacheMatrix that remembers the inverse when it is 
## already calculated and it tracks whether the matrix has been changed.
## This special matrix-object is made with the function called makeCacheMatrix.
## The actual calculation is done with the function called cacheSolve.


## function: makeCacheMatrix (x: a numeric matrix)
## This function creates a special "matrix" object that can cache its inverse.
## 4 functions can be applied to this object: set and get to set/get the matrix
## stored in the object. As well as setinverse and getinverse to set/get the
## inverse matrix corresponding to the matrix. However, when the inverse was not 
## yet calculated, getinverse will return NULL.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## function: cacheSolve (x: CacheMatrix object made with makeCacheMatrix)
## This function computes the inverse of the special matrix object. It checks 
## first whether an inverse for the matrix was already calculated. If not, it 
## will calculate the inverse and store it in the special matrix object.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

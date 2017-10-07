## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather
## than compute it repeatedly. The two functions below cache the 
## inverse of a matrix.

## This function creates a special "matrix" object that can 
## cache its inverse. It is assumed that the matrix supplied 
## is always invertible.
## The function creates a special "matrix", which is really 
## a list containing function to:
## 1. set the value of the matrix
## 2. get the value of the matrix (content of matrix)
## 3. set the value of the inverse
## 4. get the value of the inverse

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


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.
## It is assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

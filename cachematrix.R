## Calculating the Inverse of A Matrix
## The Matrix That can cache its inverse

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  get <- function() a
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Inverse of matrix from above, If already created displays from cache

cacheSolve <- function(a, ...) {
        ## Returning a matrix that is the inverse of 'a'
  inv <- a$getInverse()
  if (!is.null(inv)) {
    message("getting cache data")
    return(inv)
  }
  mat <- a$get()
  inv <- solve(mat, ...)
  a$setInverse(inv)
  inv
  }


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #inverse matrix
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x #get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse #set the value of the inverse
  getinverse <- function() inv #get the value of the inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("from cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


B <- matrix(c(1,1,1,2),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1) 

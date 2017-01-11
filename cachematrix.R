## The function 'makeCacheMatrix' creates a list, containing functions to
## 1. set value of matrix
## 2. get value of matrix
## 3. set the value of inversed matrix
## 4. get the value of inversed matrix
## 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) s <<- inv
  getinverse <- function() s
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function 'cacheSolve' calculates the inversion of the special "matrix" 
## created with the function 'makeCacheMatrix'. However, it first checks to see 
## if the solution has already been calculated. If so, it gets the inversed matrix from 
## the cache and skips the computation. Otherwise, it calculates the inversed matrix and 
## sets the calculated value in the cache via the (inner)'setinverse' function
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if (!is.null(s)){
    message("getting cached data")
    return (s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}

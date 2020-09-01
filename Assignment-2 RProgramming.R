
## The matrix inversion is a process in which you catch the
## inverse instead of typing it repeatedly. 
## MakesCacheMatrix function is used to create a special matrix, 
##so it can later inverse with cacheSolve function


makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

###CacheSolve tests the function by computing the inverse created
###above with makeCacheMatrix. Therefore, it should retrieve inverse
###without changing the matrix. 
cacheSolve <- function(x, ...) {
  ### Inverse  of x 
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
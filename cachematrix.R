##The first Function which is MakeCacheMatrix creates a special matrix and caches its inverse, The seconde Function which is cacheSolve calculates the inverse of the matrix created with the first Function ,if the inverse has already been calculated then it gets it from the cache without doing the computation ,Otherwise it calculates it (the inverse) and sets it's value in the cache via the setinverse Function


## This function (makeCacheMatrix) creates a special matrix which is really a list containing a function to 1-set the value of the matrix ,2-get the value of the matrix ,3-set the value of the inverse ,4-get the value of the inverse 
	
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



##This function calculates the inverse of the special matrix created with the above function .However it first checks to see if the inverse has already been calculated if so ,it gets the inverse from the cache and skips the computation .Otherwise ,it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}        

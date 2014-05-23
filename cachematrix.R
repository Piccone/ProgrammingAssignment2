## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL #create the object that will hold the cached inverse value
  #set function enables comparison between current matrix/inverse values and any previously calculated ones
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  get <- function() x #  returns the actual matrix value on which inverse will be calculated
  setinverse <- function(solve) inv <<- solve #assigns the calculated inverse value to the object inv
  getinverse <- function() inv #returns either a precalculated inverse value or null if not precalculated
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  #connects the functions to the "column names" of the special object returned by makeCacheMatrix
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix()
## If the inverse has already been calculated and is unchanged, retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() #retrieve the value held in the object that caches a previously calculated inverse

  #if it returned a previously calculated value, return it and end the function
  if(!is.null(inv)) { 
    message("getting cached data")
    return(inv)
  }
  
  #If it did't return a previously calculated value , calculate it here, cache it and return it:
  
  data <- x$get() #matrix on which inverse will be calculated
  inv <- solve(data, ...) #calculate the inverse of that matrix
  x$setinverse(inv) #cache the calculated inverse value
  inv #return the calculated inverse value
  
}

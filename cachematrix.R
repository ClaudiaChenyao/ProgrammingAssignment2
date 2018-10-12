## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set the value of the vector
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  #get the value of the vector
  get <- function() x
  
  #set the value of inverse
  setInv <- function(inverse) m <<- inverse
  
  #get the value of inverse
  getInv <- function() m
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #get the value of inverse
  m <- x$getInv()
  
  #if calculated the inverse value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #else
  data <- x$get()
  m <- solve(data, ...)
  
  #set the value of inverse
  x$setInv(m)
  
  #return
  m
}

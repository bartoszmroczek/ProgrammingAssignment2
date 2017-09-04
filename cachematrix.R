## function makeCacheMatrix is used to create an object, which used as an argument for function cacheSolve
## will return an inverse of a matrix 
## (first time function cache solve will do the inverse and then store it in the object returned by function makeCacheMatrix, 
##if used again, function cache solve will use the previously generated result)

## function makeCacheMatrix returns a list of functions set,get,seting & getinv, and has a variable inv used to store the cached inverse of a matrix x

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinv<- function(inverse) {inv<<-inverse}
  getinv <- function() {inv}
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
    
  )
}


## function cacheSolve checks if the cached inverse of a matrix already exists. If it exists, the inverse is returned.
## If not, the function generates inversed matrix and caches it using method setinv

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
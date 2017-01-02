## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  library('MASS')
  InvMatrix <- NULL
  
  set <- function(y){
    x <<- y
    InvMatrix <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(ginv) InvMatrix <<- ginv
  getInv <- function() InvMatrix
  
  list(set = setInv, get = get,setInv = setInv,getInv = getInv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  library('MASS')
  
  ## Return a matrix that is the inverse of 'x'
  InvMatrix <- x$getInv()
  
  if (!is.null(InvMatrix)){
    message("getting cached data")
    return(InvMatrix)
  }
  
  data <- x$get()
  
  InvMatrix <- ginv(data)
  
  x$setInv(InvMatrix)
  InvMatrix
  
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function creates an object with simple 4 piece interface for storing a matrix
# and its reverse matrix to enable its caching
# please be carefull in using the setInv method as (accidental) putting wrong value may lead to misleading results
makeCacheMatrix <- function(mat = matrix()) {
  invMat <- NULL
  set <- function(y) {
           mat <<- y
           invMat <<- NULL
         }
  get <- function() mat
  setInv <- function(aInvMat) invMat <<- aInvMat
  getInv <- function() invMat
  list(get = get, set = set, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
  #check if we have it cached
  invMat <- mat$getInv()
  if(!is.null(invMat)) {
    message("Returning cached inverse...")
    return(invMat)
  } 
  #calculate inverse, cache it and return
  invMat <- solve(mat$get(), ...)
  mat$setInv(invMat)
  invMat
}
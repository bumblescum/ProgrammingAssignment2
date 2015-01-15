## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  im <- NULL
  set <- function(y){
    
    x<<-y
    im <<-NULL
  }
  
  get<- function () x
  setmatrix<-function(matrix) im <<- solve(matrix)
  getmatrix <-function() im
  list (set= set, get = get,setmatrix = setmatrix, getmatrix = getmatrix)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getmatrix()
  
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im<-solve(data, ...)
  x$setmatrix(im)
  im
}
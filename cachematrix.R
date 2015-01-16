## Using these two functions together allows for a user to cache 
##the result of inverting a matrix 

## makeCacheMatrix is a set of functions that allows a user to set or get
## a special matrix, while also inverting the matrix.  

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


## cacheSolve takes as an argument a special matrix created using
##MakeCacheMatrix(). This function returns the inverse of a matrix. 
##If the calculation has already been made, then the cached value is used. 
##otherwise the inverse is calculated and cached for future use.

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
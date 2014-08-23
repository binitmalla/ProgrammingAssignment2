## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix that allows to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  #Set value of inverse to NULL
  inverse<-NULL
  
  #Set defines a vector to set x to vector y and reset inverse to NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  
  #Returns vector x
  get<-function() x
  
  #Defines a function to set inverse
  set_inverse<-function(solve) inverse<<- solve
  
  #Returns inverse
  get_inverse<-function() inverse
  
  #Defines a list of functions
  list(set=set, get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by makeCacheMatrix function and retrieves inverese from Cache if it's already been calculated, otherwise calculates the inverse
cacheSolve <- function(x, ...) {
  
  inverse<-x$get_inverse()
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(inverse)){
    message("Retrieving from cache")
    return(inverse)
  }else{
    inverse<-solve(x$get(), ...)
    x$set_inverse(inverse)
    return(inverse)
  }
        
}

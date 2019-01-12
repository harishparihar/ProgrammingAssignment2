## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {      ## set value of matrix
    x<<-y
    m<<-NULL
  }
  get<-function() x       ## get value of matrix
  setInverse<-function(inverse) m<<-inverse      ## set value of inverse matrix
  getInverse<-function() m                      ## get value of inverse matrix
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## computes inverse of cache matrix returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)  ## solve function to get solution in form of inverse matrix
  x$setInverse(m)
  m
}

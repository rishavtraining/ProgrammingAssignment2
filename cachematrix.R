## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  ##defining argument with default mode of matrix
  inv<-NULL   ##initialise inv as null
  set<-function(y)   ##define set function to assign new
  {
    x<<-y
    inv<<-NULL ##if there is new matri,reset inv to null  
  }
  get<-function()x   ##get returns value of matrix argument
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv))
  {
    print("getting data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setinverse(inv)
  inv
}

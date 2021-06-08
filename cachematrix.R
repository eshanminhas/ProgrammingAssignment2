##makeCacheMatrix() creates a matrix with cacheable inverse. It has functions to set 
## and get the matrix object and the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ##Initialize cache vector for Inverse
  
  set <- function(mat)
  {
    x <<- mat
    inv <<- NULL
    ##set value of the inverse vector and matrix in this environment
  }
  get<- function() 
  {
    x             ##get the matrix
  }         
  setInevrse <- function(inverse) 
  {
    inv <<- inverse      ##set and cache the inverse of the Matrix
  }  
  getInverse <- function()
  {
    inv                 ##get the inverse
  }  
  
  list(set = set, get = get,
       setInevrse = setInevrse,
       getInverse = getInverse)    ##Return the list with all get set functions
}


##cacheSolve() to get the cached inverse and if not then solve the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  ##get the inverse from the argument matrix
  
  #check if the inverse is present
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
    
    ##if inverse present in cache then return inverse and exit
  }
  
  matr <- x$get()               #if no inverse then get the matrix x
  inver <- solve(matr, ...)      ##solve the matrix for inverse
  x$setInevrse(inver)                  ##set the inverse in the matrix 
  inver                             ##return the inverse of x
}

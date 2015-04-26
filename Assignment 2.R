#Course: R-Programming Coursera
#Assignment: 2
#Due Date: April 26th, 2015

#Purpose: This pair of functions caches the inverse of a given matrix. 
#         This eliminates unecessary calculations, as the value is stored. 

#Part 1
#'makeCacheMatrix' creates a special "matrix" object that can cache its inverse
#Args: An invertible matrix

makeCacheMatrix <- function(x=matrix()) {
      m <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      
       get <- function () x
       setInv <- function(solve) m <<- solve
       getInv <- function() m
       list ( set = set, get = get, setInv = setInv, getInv = getInv)}

#Part 2
#'cacheSolve' computes the inverse of the matrix returned in makeCacheMatrix.
#If the inverse has alredy been calculated, then the matrix will be retrieved
#from the cache. 
#Args: A special "matrix" created in makeCacheMatrix

cacheSolve <- function(x,...){
   m <- x$getInv()
   if(!is.null(m)){
      message("Getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data,...)
   x$setInv(m)
   m
}
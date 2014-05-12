### Introduction

#Taking the inverse of a matrix is typically a fast
#operation. However, for a very large matrix, it may take too long to
#compute the inverse, especially if it has to be computed repeatedly (e.g.
#in a loop). If the contents of a matrix are not changing, it makes
#sense to cache the inverse so that when we need it again, it
#can be looked up in the cache rather than recomputed.


#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to

#1.  set the matrix
#2.  get the matrix
#3.  set the inverse of the matrix
#4.  get the inverse of the matrix 

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



#The following function calculates the inverse of the special "matrix"
#created with the above function. However, it first checks to see if the
#inverse has already been calculated. If so, it get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the matrix and sets the value of the inverted matrix in the cache via the `setinverse`
#function.


cachesolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


##Usage
##Create matrix
    #myMat <- matrix(data = c(2,3,5,6,3,2,5,6,7), nrow = 3, ncol = 3)



##Create cache matrix 
    #myCacheMat <- makeCacheMatrix(myMat)


##Cache the inverted matrix
    #cachesolve(myCacheMat)



##Get cached inverted matrix
    #cachesolve(myCacheMat)


## This is the solution resolved of programming assignment 2 on the second of September 2023. The solution inverses 
## the matrix in the given way. The examples show that it really works in the requested way. 



## This function creates a list object that has places for matrix x and its inverse
makeCacheMatrix <- function(x = matrix()) {
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

## this function computes and inserts the inverse of x into the list created above if it is null.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}



# testing...
a <- makeCacheMatrix(matrix(1:4, 2, 2))
a$get() ##
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4

a$getinverse()
# NULL

# populate cache...
cacheSolve(a)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

a$getinverse()
# not null! 

# test if inverse is correct?
a$get() %*% a$getinverse() 
#[,1] [,2]
#[1,]    1    0
#[2,]    0    1

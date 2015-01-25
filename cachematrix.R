## Programming assignment 2:  Create function to inverse a matrix and store the value

## makeCacheMatrix:  store matrix and the inverse value of it

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL ##set m as a default to NULL
  set <- function(y) { ##embed function "set"
    x <<- y ##permanently stores x with value of y
    m <<- NULL ##permantnly stores m as value NULL
  }
  get <- function() x #sets the value of "get" to the function ran on x
  setinverse <- function(inverse) m <<- inverse ##Set the inverse value
  getinverse <- function() m ##call the inverse value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Solve for the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() ##call the inverse value that's been stored and store it as m
  if(!is.null(m)) { ##null check to see if there's a value already present
    message("getting cached data") 
    return(m)
  }
  data <- x$get() ##fetch the matrix
  m <- solve(data)  ##Solve for the matrix 
  x$setinverse(m)  ##Store the result
  m
}

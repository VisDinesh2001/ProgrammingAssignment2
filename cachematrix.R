## makeCacheMatrix stores an inputted matrix and its inverse in
## a list format. The list is a set of 4 functions named within the function

## Creates a special matrix to store the inverse
    ## Contains functions to set and get the matrix,
    ## as well as functions to set and get the inverse of the matrix.
    ## Calling the list 'x' by name allows execution of specific functions
      ## within the subenvironment of "makeCacheMatrix".

makeCacheMatrix <- function(x = matrix()) {
      m_inv <- NULL
      set <- function(y){
            x <<- y
            m_inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m_inv <<- inverse
      getinverse <- function() m_inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x', where 'x' is a matrix

cacheSolve <- function(x, ...) {
      m_inv <- x$getinverse()
      if (!is.null(m_inv)){   ##get cached data if m_inv is not NULL
            message("retreiving cached data")
            return (m_inv)
      }
      matrix <- x$get()   ## Retrieves the inputted matrix
      m_inv <- solve(matrix, ...)   ## Inverts the matrix, storing it in m_inv
      x$setinverse(m_inv)     ## Caches the inverted matrix in m_inv
      m_inv
}

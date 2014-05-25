## This function makeCacheMatrix creats a "matrix", which will do these things:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix using function solve
## 4. get the value of the inversed matrix


makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
                  x <<- y
                  m <<- NULL
          }
          get <- function() x
          setinversion <- function(solve) m <<- solve
          getinversion <- function() m
          list(set = set, get = get,
               setinversion = setinversion,
               getinversion = getinversion)

}


## This function will calculate the inversion of a given matrix. 
## It will first check if the inversed matrix has already been calculated.
## If so, it will get the inversed matrix and skip the calculation.
## If not, it calculate the inversion of the matrix and set the value of the solve in the cache via the setinversion function.
cacheSolve <- function(x, ...) {
             m <- x$getinversion()
             if(!is.null(m)) {
               message("getting cached data")
               return(m)
             }
             data <- x$get()
             m <- solve(data, ...)
             x$setinversion(m)
             m
        ## Return a matrix that is the inverse of 'x'
}

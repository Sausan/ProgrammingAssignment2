## This function calculates and caches the results of a matrix inverse
## We first provide an inversable matrix to the makeCacheMatrix to cache the inverse value, 
## Then we call the cacheSolve function to calculate the inverse value and return thr answer
## cacheSolve first checkes if the inverse value is cached, if so, it uses the cached value, else, it calculates the value and resturn it



## makeCacheMatrix generates a matrix that is cacheable

makeCacheMatrix <- function(x = matrix()) {
  ## First initiate the inverse matrix to NULL
  my_inverse <- NULL    

  ## define the caching functions set, get, set inverse and get inverse

  ## Assign new value for the matrix and initiate the inverse matrix with NULL
  set <- function(new_matrix) {
      x <<- new_matrix
      my_inverse <<- NULL
  }

  ## return the current value of the matrix
  get <- function() x

  ## save the matrix inverse value in the inverse matrix
  setinverse <- function(inverse) my_inverse <<- inverse

  ## return the inverse value
  getinverse <- function() my_inverse

  ## list of functions in makeCachMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve function first checks the inverse value to see if it is NULL. 
## if not, it returns the cahed value.
## if it is, it calculate the inverse value
## returne the inverse value

cacheSolve <- function(x, ...) {
    ## get the current value of the inverse matrix
    my_inverse <- x$getinverse()

    ## Check if the value is NULL
    if(!is.null(my_inverse)) {
       ## if not NULL, that means that we have calculated the inverse value before and it is cached
       message("getting cached data")

       ## return the current inverse value, and stop the function
       return(my_inverse)
    }

    ## if it is NULL, calculate the inverse value
    data <- x$get()
    my_inverse <- solve(data)
    x$setinverse(my_inverse)

    ## return the inverse value, and stop the function
    my_inverse
}




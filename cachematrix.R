## These functions are designed to present an understanding of Lexical Scoping within the R Programming language
## Combined, they will take an input matrix, calculate and return the Inverse of the Matrix
## When called for the same Matrix multiple times with the same instance, rather than re-calculating the result it will return the already cached result


## The makeCacheMatrix will calculate & store the Matrix ; or take an already solved matrix to cache

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
  # If an already calculated Inverse is provided to the $setMtrix subfuction, set the passed maxtrix to the x variable and set cache variable c as NULL
    doSet <- function(b) {
      x <<- b
      c <<- NULL
    }
    # If the $dGet subfunction is called, pass the value back
    doGet <- function() x
    # If the $setMtrix subfunction is called, then calculate the Inverse and save to cache variable c
    setMtrix <- function(solve) c <<- solve
    # if the $getMtrix subfunction is called, return the cached variable c
    getMtrix <- function() c
    list (doSet = doSet, doGet = doGet,
          setMtrix = setMtrix,
          getMtrix = getMtrix )
}


## The cacheSolve variable will take a matrix, and either return a cached Inverse matrix, or calculate and cache if it's a new one

cacheSolve <- function(x, ...) {
    ## get the existing Cached Inverse matrix (which will be NULL if it hasn't already been calcualted in this instance)
    mt<- x$getMtrix()
    ## Check if the returned matrix is NULL or not - and if it is not null, advised that the Cached matrix is being returned as the value & return the cached value
    if(!is.null(mt)) {
        message("Obtaining Cached Matix")
      return(mt)
    }
    ## If the Cached Matrix is NULL (so not previously calculated) get the Matrix value
    mcm <- x$doGet()
    ## Perform the SOLVE to get the Inverse Matrix
    mt<-solve(mcm,...)
    ## Pass the calculated Inverse Matrix to be cached
    x$setMtrix(mt)
    ## output the now calculated value
    return(mt)
}

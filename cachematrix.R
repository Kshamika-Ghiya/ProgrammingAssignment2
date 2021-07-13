## makeCasheMarix creates a special matrix object, 
## which cashes the inverse, i.e. invertible square matrix
## gives the inverse matrix as output 

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
        x <<- y
        a <<- NULL
        }
        get <- function() x
        setInv <- function(Inv)
                a <<- Inv
        getInv <- function() a
        list(set = set, get = get, setInv = setInv,
             getInv = getInv)

}


## cacheSolve computes the inverse.

cacheSolve <- function(x, ...) {
        a = x$getInv()
        if(!is.null(a)) {
                message("Cashed data is being fetched!")
                return(a)
        }
        ## Return a matrix that is the inverse of 'x'
        data = x$get()
        a = Inv(data, ...)
        x$setInv(a)
        a
}

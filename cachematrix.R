#ProgrammingAssignment2

#Aim of this function is to cache the inverse of matrix.

#The function makeCacheMatrix creates the inverse of a spesific matrix.

makeCacheMatrix <- function( mtrx = matrix() ) {
inv <- NULL

set <- function( matrix ) {
        mtrx <<- matrix
        inv <<- NULL
}

get <- function() {
	mtrx
}

setInverse <- function(inverse) {
    inv <<- inverse
}

getInverse <- function() {
    inv
}

list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))

}



#The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not #changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
mtrx <- x$getInverse()

if( !is.null(mtrx) ) {
        message("getting cached data")
        return(mtrx)
}

data <- x$get()

mtrx <- solve(data) %*% data

x$setInverse(mtrx)

return(mtrx)
}

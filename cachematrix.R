## As a part of the course R programming, assignment 2:
# This function creates a special "matrix" object that can cache its inverse.

## Data for example/testing
# > mdat <- matrix(c(20,2,4,8,7,7,7,21,13), nrow = 3, ncol =3, byrow=T)
# > makeCacheMatrix(mdat) -> x
# > cacheSolve(x)
# [,1]        [,2]        [,3]
# [1,]  0.07427056 -0.07692308  0.01856764
# [2,]  0.07294430 -0.30769231  0.14323607
# [3,] -0.15782493  0.53846154 -0.16445623
# > cacheSolve(x)
# getting cached data
# [,1]        [,2]        [,3]
# [1,]  0.07427056 -0.07692308  0.01856764
# [2,]  0.07294430 -0.30769231  0.14323607
# [3,] -0.15782493  0.53846154 -0.16445623

##########################################################################
## makeCacheMatrix; based on the example "Caching the Mean of a Vector",creates
# a special "vector", which is really a list containing a functionto
#      1. set the value of the vector
#      2. get the value of the vector
#      3. set the value of the mean
#      4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setM <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getM <- function() x
        setMinv <- function(solve) inv <<- solve
        getMinv <- function() inv
        list(setM = setM, getM = getM, setMinv = setMinv,getMinv = getMinv)
}

##########################################################################
## cacheSolve function; calculates the mean of the special "vector"
# created with the above function. However, it first checks to see if
# the mean has already been calculated. If so, it gets the mean from 
# the cache and skips the computation. Otherwise, it calculates the mean
# of the data and sets the value of the mean in the cache via the setmean
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inv <- x$getMinv()
                if (!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                
                matrix <-x$getM()
                inv <-solve(matrix, ...)
                x$setMinv(inv)
                inv
                }
##########################################################################
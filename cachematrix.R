## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## Creating a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}





> ## Testing the function with different scenarios.
> setwd("D:/RProjects") ## to set the working directory, it is my R working directory.
> source("cachematrix.R") ## store the previous functions with the name of cachematrix.R and put is in your working directory.


> my_matrix <- makeCacheMatrix(matrix(seq(2, 8, 2), 2, 2)) # to assign values to the original matrix.
> my_matrix$get() ## to show the original matrix.
     [,1] [,2]
[1,]    2    6
[2,]    4    8

>  my_matrix$getInverse() ## the inverse matrix has not been set yet.
NULL

> cacheSolve(my_matrix) ## getting cached data of the inversed matrix.
     [,1]  [,2]
[1,] -1.0  0.75
[2,]  0.5 -0.25

> my_matrix$getInverse() ## now the inverse matrix has values of inverting the original matrix
     [,1]  [,2]
[1,] -1.0  0.75
[2,]  0.5 -0.25


> my_matrix <- makeCacheMatrix(matrix(seq(1, 7, 2), 2, 2)) # to assign new values to the original matrix.

> my_matrix$get() ## to show the new matrix.
     [,1] [,2]
[1,]    1    5
[2,]    3    7

> my_matrix$getInverse() ## the cached values of the new inverse has been cleared.
NULL

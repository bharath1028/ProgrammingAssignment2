## This program allows us to cache the inverse of a matrix
## rather than having to compute it every time. This saves
## us from having to run intensive operations, especially for
## large matrices.

## makeCacheMatrix:
## This function takes in a matrix as an argument and returns
## a list of functions to:
## 1) Set the matrix
## 2) Get the matrix
## 3) Set the value of the inverse
## 4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	## Store the cached inverse matrix
    inv <- NULL
    
    ## Setter for matrix
	set <- function(y) {
	    x <<- y
	    inv <<- NULL
	}
	
    ## Getter for matrix
	get <- function() x
    
    ## Setter for inverse matrix
	setInverse <- function(inverse) inv <<- inverse

    ## Getter for inverse matrix
	getInverse <- function() inv

    ## Returns the matrix with the above functions
	list(set = set,
	     get = get, 
             setInverse = setInverse, 
	     getInverse = getInverse)
}


## This function computes the inverse of the matrix.
## If the matrix inverse has already been computed,
## it returns the cached solution.
cacheSolve <- function(x, ...) {
	inv  <- x$getInverse()
    
    ## If inverse already calculated, get cached solution
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
    
    ## Otherwise calculate the inverse and set it
	data <- x$get()
	inv <- solve(data)
	x$setInverse(inv)

	inv
}

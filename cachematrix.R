## makeCacheMatrix defines functions that enable the value of a matrix to be cached

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
    	x <<- y
    	m <<- NULL
  	}
  
	get <- function(){x}
	set_matrix <- function(matrix_value){m = matrix_value}
	get_matrix <- function(){m}
	list(set = set, get = get, set_matrix = set_matrix, get_matrix 	= get_matrix)
}


## cacheSolve returns a matrix that is the inverse of 'x'
## cacheSolve checks for a cached version of the inverse and returns the cached value if it exisits
## if a cached value does not exisit cacheSolve calculates the inverse of 'x'

cacheSolve <- function(x, ...) {
	m <- x$get_matrix()
	if(!is.null(m)){
		message{"getting cached data"}
	}
	else{
		m <- solve(x)
		x$set_matrix(m)
	}
	return(m)
}
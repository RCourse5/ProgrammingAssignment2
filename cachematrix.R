

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    
    set_matrix <- function(matrix) {
        x <<- matrix
        inverse <<- NULL
    }

    get_matrix <- function() {
        x
    }

    set_cached_inverse <- function(i) {
        inverse <<- i
    }

    get_cached_inverse <- function() {
        inverse
    }

    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_cached_inverse = set_cached_inverse,
         get_cached_inverse = get_cached_inverse)
}


cacheSolve <- function(x, ...) {

    cached_inverse <- x$get_cached_inverse()

    if (!is.null(cached_inverse)) {
        message("found cached inverse")
        return(cached_inverse)
    }

    matrix_content <- x$get_matrix()

    inverse <- solve(matrix_content) %*% matrix_content

    x$set_cached_inverse(inverse)

    inverse
}
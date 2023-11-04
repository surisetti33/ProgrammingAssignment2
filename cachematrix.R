## makeVector function creates a list with methods to set and get a vector and its mean.
makeVector <- function(x = numeric()) {
    m <- NULL  # Initialize a variable for mean.
    set <- function(y) {  # Method to set the vector.
        x <<- y
        m <<- NULL  # Reset the mean when the vector is updated.
    }
    get <- function() x  # Method to get the vector.
    setmean <- function(mean) m <<- mean  # Method to set the mean.
    getmean <- function() m  # Method to get the mean.
    list(set = set, get = get, setmean = setmean, getmean = getmean)  # Return a list of methods.
}

## cachemean function calculates and caches the mean of a vector.
cachemean <- function(x, ...) {
    m <- x$getmean()  # Attempt to get the cached mean.
    if (!is.null(m)) {
        message("getting cached data")  # Display a message if the mean is cached.
        return(m)  # Return the cached mean.
    }
    data <- x$get()  # Get the vector data.
    m <- mean(data, ...)  # Calculate the mean with optional arguments.
    x$setmean(m)  # Cache the calculated mean.
    m  # Return the calculated mean.
}

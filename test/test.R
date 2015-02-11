myfunction <- function() {
	x = rnorm(100)
	mean(x)
}

second <- function(x) {
	x + rnorm(length(x))
}

dputeach <- function(...) {
 	v = c(...)
	for(x in v) { dput(x) }
}
dputeach(1,2,3,4,5)


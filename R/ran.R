"ran_gamma" <- function(r, a, b=1, length=NA)
{
	a <- as.numeric(a)
	b <- as.numeric(b)
	length <- as.integer(length)

	if(length(a) != 1) {
		if(!is.na(length)) warning("ignoring argument length")
		if(length(b) != 1 && length(b) != length(a)) stop("a and b must be the same length")
		.Call("ran_gamma", r, a, b, 1, PACKAGE = "gsl")
	} else if(length(b) != 1) {
		if(!is.na(length)) warning("ignoring argument length")
		.Call("ran_gamma", r, a, b, 1, PACKAGE = "gsl")
	} else {
		if(is.na(length)) length = 1
		.Call("ran_gamma", r, a, b, length, PACKAGE = "gsl")
	}
}
"ran_gaussian" <- function(r, sigma, length=NA)
{
	sigma <- as.numeric(sigma)
	length <- as.integer(length)

	if(length(sigma) != 1 && !is.na(length)) warning("ignoring argument length")
	if(is.na(length)) length = 1
	.Call("ran_gaussian", r, sigma, length, PACKAGE = "gsl")
}

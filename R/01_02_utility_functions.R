# ---------------------------------------------------------------------------------- #
#####                     Some basic custom utility functions                    #####
# ---------------------------------------------------------------------------------- #

library(magrittr) # The only library that I truly need to load (in order to be able to use pipes).

### ____________________
#' Find binary variables
#'
#' @description Automatically detect binary (boolean) variables, even if there is NAs in the data.
#' @param x Any variable (vector, columns etc.) of any length.
#'
#' @return Logical (i.e. a TRUE or FALSE answer).
#' @export
#' @importFrom stats na.omit
#'
#' @examples
#' xx <- c(0, 0, 1, 0, NA)
#' is_binary(xx) # TRUE
is_binary <- function(x) {
  x0 <- na.omit(x)
  length(unique(x0)) %in% 1:2 && all(x0 %in% 0:1)
} # Asks: is the length of the unique elements of x0 (excluding NAs) is 1 or 2 AND all either 0 or 1?





### ____________________________________________
#' Convert factor values as exact numeric values
#'
#' @description Automatically convert the input `x (factor)` into a `numeric` vector while keeping the
#' exact value of `x`.
#' @details  In \strong{R}, \emph{numeric factors} should be coded as `character` but it is rarely the
#' case because most functions actually prefer `factors`. It's one of the shortcomings of \strong{R}.
#' Consequently, there is no \strong{R} function that converts a \emph{numeric factor} (e.g. a binary
#' variable with only zeros and ones) into a `numeric` vector while keeping the correct value (if
#' you use `as.numeric(myfactor)`, you will get the underlying level codes, not the values as
#' numbers), hence the usefulness of `as.numfactor`.
#'
#' @param x A `factor` variable (may contain NAs).
#'
#' @return A `numeric` vector.
#' @export
#'
#' @examples
#' f <- as.factor(cars$speed)
#' as.numeric(f) # Converts the values into level ranks.
#' as.numfactor(f) # Works!
as.numfactor <- function(x){as.numeric(levels(x))[x]}

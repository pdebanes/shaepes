


#two small functions
not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

all_na <- function(x) all(is.na(x))

not_all_zero <- function(x) {
  any(!is.na(x) & x != 0)
}
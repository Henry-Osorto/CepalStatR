# Internal null-coalescing operator
#
# Returns `y` when `x` is `NULL`; otherwise returns `x`.
#
# @param x Primary value.
# @param y Replacement value used when `x` is `NULL`.
#
# @return Either `x` or `y`.
# @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Walrus operator
#'
#' See \code{rlang::\link[rlang:colon-equals]{\%:\%=}} for details.
#'
#' @name :=
#' @rdname colon-equals
#' @keywords internal
#' @export
#' @importFrom rlang `:=`
#' @usage lhs := rhs
#' @param lhs A name or a string.
#' @param rhs A value.
#' @return A named value.
NULL

#' Bang-bang operator
#'
#' See \code{rlang::\link[rlang:bang-bang]{\!\!}} for details.
#'
#' @name !!
#' @rdname bang-bang
#' @export
#' @importFrom rlang `!!`
#' @usage !!(x)
#' @param x A value.
#' @return The value of `x`.
NULL

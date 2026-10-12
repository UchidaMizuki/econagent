#' Constant elasticity of transformation (CET) function
#'
#' @param substitution A scalar numeric of substitution parameter. Must be
#' greater than `1`, so that the transformation surface is convex.
#' @param elasticity_of_transformation A positive scalar numeric of elasticity
#' of transformation.
#' @param homogeneity A scalar numeric of degree of homogeneity. By default,
#' `1`. When `homothetic = TRUE`, `homogeneity` must be equal to `1`.
#' @param efficiency A scalar numeric of efficiency parameter.
#' @param weights A numeric vector of weights.
#' @param homothetic A logical scalar. By default, `homogeneity == 1`.
#'
#' @return A `util_cet` object.
#'
#' @name util_cet
NULL

#' @rdname util_cet
#' @export
util_cet <- function(
  substitution,
  homogeneity = 1,
  efficiency = NA_real_,
  weights = double(),
  homothetic = homogeneity == 1
) {
  check_substitution_cet(substitution)

  util_ces_impl(
    substitution,
    homogeneity = homogeneity,
    efficiency = efficiency,
    weights = weights,
    homothetic = homothetic,
    class = "util_cet"
  )
}

#' @rdname util_cet
#' @export
util_cet2 <- function(
  elasticity_of_transformation,
  homogeneity = 1,
  efficiency = NA_real_,
  weights = double(),
  homothetic = homogeneity == 1
) {
  util_cet(
    substitution = (elasticity_of_transformation + 1) /
      elasticity_of_transformation,
    homogeneity = homogeneity,
    efficiency = efficiency,
    weights = weights,
    homothetic = homothetic
  )
}

#' @export
type_sum.util_cet <- function(x) {
  "CET"
}

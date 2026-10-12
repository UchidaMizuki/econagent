#' Constant elasticity of transformation (CET) function
#'
#' @param substitution A scalar numeric of substitution parameter. Must be
#' greater than `1`, so that the transformation surface is convex.
#' @param elasticity_of_transformation A positive scalar numeric of elasticity
#' of transformation.
#' @param homogeneity A scalar numeric of degree of homogeneity. By default,
#' `1`. Only linearly homogeneous functions (`homogeneity = 1`) can be
#' calibrated with [util_calibrate()].
#' @param efficiency A scalar numeric of efficiency parameter.
#' @param weights A numeric vector of weights.
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
  weights = double()
) {
  check_substitution_cet(substitution)

  util_ces_impl(
    substitution,
    homogeneity = homogeneity,
    efficiency = efficiency,
    weights = weights,
    class = "util_cet"
  )
}

#' @rdname util_cet
#' @export
util_cet2 <- function(
  elasticity_of_transformation,
  homogeneity = 1,
  efficiency = NA_real_,
  weights = double()
) {
  util_cet(
    substitution = (elasticity_of_transformation + 1) /
      elasticity_of_transformation,
    homogeneity = homogeneity,
    efficiency = efficiency,
    weights = weights
  )
}

#' @export
type_sum.util_cet <- function(x) {
  "CET"
}

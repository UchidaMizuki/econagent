#' Constant elasticity of transformation (CET) function
#'
#' @param substitution A scalar numeric of substitution parameter. Typically
#' greater than `1`, so that the transformation surface is convex.
#' @param elasticity_of_transformation A scalar numeric of elasticity of
#' transformation.
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
  f <- util_ces(
    substitution,
    homogeneity = homogeneity,
    efficiency = efficiency,
    weights = weights,
    homothetic = homothetic
  )
  class(f)[[1]] <- "util_cet"
  f
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
util_gradient.util_cet <- util_gradient.util_ces

#' @export
util_calibrate.util_cet <- util_calibrate.util_ces

#' @export
util_demand_marshallian.util_cet <- util_demand_marshallian.util_ces

#' @export
obj_sum.util_cet <- obj_sum.util_ces

#' @export
type_sum.util_cet <- function(x) {
  "CET"
}

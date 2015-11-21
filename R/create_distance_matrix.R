#' Title
#'
#' @param pairwise_dist a list of pairwise comparisons from create_pairwise_comparisons
#' @param diag_val value of the diagonal, defaults to 0 (0 distance)
#'
#' @return a distance matrix
#' @export
#' @examples
#' matrix <- replicate(20, rnorm(10))
#' create_distance_matrix(create_pairwise_comparisons(matrix))
create_distance_matrix <- function(pairwise_dist, diag_val = 0) {
  list_samples <- unique(c(as.character(pairwise_dist$x), as.character(pairwise_dist$y)))
  pairwise_dist.rev <- transform(pairwise_dist, x = y, y = x, dist = dist)
  pairwise_dist.same <- data.frame(x = list_samples, y = list_samples, dist = diag_val)
  pairwise_dist <- unique(rbind(pairwise_dist.same, pairwise_dist, pairwise_dist.rev))
  x <- data.matrix(tidyr::spread(pairwise_dist, key = y, value = dist)[-1])
  return(as.dist(x))
}

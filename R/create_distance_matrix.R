#' Title
#'
#' @param pairwise a list of pairwise comparisons from create_pairwise_comparisons
#' @param ncores number of cores to parallelize this, defaults to single-core
#' @param diag_val value of the diagonal, defaults to 0 (0 distance)
#' @param distfun a function used to calculate distance between any pairwise comparisons. Defaults to sqrt(sum((x1 - x2) ^ 2)) (euclidian distance)
#'
#' @return a distance matrix
#' @export
#' @examples
#' matrix <- replicate(20, rnorm(10))
#' create_distance_matrix(create_pairwise_comparisons(matrix))
create_distance_matrix <- function(pairwise, ncores = 1, diag_val = 0, distfun = function(x1, x2) sqrt(sum((x1 - x2) ^ 2))) {
  if (!(is.list(pairwise))) stop("'pairwise' must be a list of data.frames or matrices")
  if (ncores > parallel::detectCores()) {
    stop(paste("Warning! You specified", ncores, "cores, but we only detected", parallel::detectCores(), "cores."))
  }
  total <- length(pairwise)
  total_batch <- divide_cores(total, ncores = ncores)
  pairwise_dist <- foreach::foreach(i=1:nrow(total_batch), .combine = rbind) %dopar% {
    start <- total_batch[i,1]
    end <- total_batch[i,2]
    merge_bind = NULL
    for (f in start:end) {
      tmp <- pairwise[[f]]
      diff.temp <- data.frame(
        x = colnames(tmp)[1],
        y = colnames(tmp)[2],
        dist = distfun(tmp[,1], tmp[,2])
      )
      merge_bind <- rbind(merge_bind, diff.temp)
    }
    return(merge_bind)
  }
  list_samples <- unique(c(as.character(pairwise_dist$x), as.character(pairwise_dist$y)))
  pairwise_dist.rev <- transform(pairwise_dist, x = y, y = x, dist = dist)
  pairwise_dist.same <- data.frame(x = list_samples, y = list_samples, dist = diag_val)
  pairwise_dist <- unique(rbind(pairwise_dist.same, pairwise_dist, pairwise_dist.rev))
  x <- data.matrix(tidyr::spread(pairwise_dist, key = y, value = dist)[-1])
  return(as.dist(x))
}

#' Create pairwise comparisons by row or column
#'
#' @param matrix a matrix or data.frame containing samples and observations
#' @param ncores number of cores to parallelize by. Defaults to single-core
#' @param by either "row" or "column", to specify if rows or columns are samples. Defaults to "row"
#'
#' @return a list of data.frames
#' @importFrom foreach "%dopar%"
#' @export
#' @examples
#' matrix <- replicate(20, rnorm(10))
#' create_pairwise_comparisons(matrix, ncores = 1) #not allowed to have parallel examples

create_pairwise_comparisons <- function(matrix, ncores = 1, by = "row", distfun = function(x1, x2) sqrt(sum((x1 - x2) ^ 2))){
  if (!(is.matrix(matrix) | is.data.frame(matrix))) stop("'matrix' must be a matrix or data frame")
  if (ncores > parallel::detectCores()) {
    stop(paste("Warning! You specified", ncores, "cores, but we only detected", parallel::detectCores(), "cores."))
  }

  # Generate combinations
  if (by == "row") comb <- combn(nrow(matrix),2)
  if (by == "column") comb <- combn(ncol(matrix),2)

  # Parallelize the dopar function by splitting the workload into batches for each core, since foreach is only good when number of jobs roughly equals number of cores
  task_list <- divide_cores(ncol(comb), ncores = ncores)
  # Combine
  pairwise <- foreach::foreach(i=1:nrow(task_list), .combine = rbind) %dopar% {
    start <- task_list[i,1]
    end <- task_list[i,2]
    merge_bind=vector("list", end-start+1)
    for (f in start:end) {
      one <- comb[1,f]
      two <- comb[2,f]
      if (by == "row") {
        sample1 <- matrix[one,]
        sample2 <- matrix[two,]
      }
      if (by == "column") {
        sample1 <- matrix[,one]
        sample2 <- matrix[,two]
      }
      diff_temp <- data.frame(
        x = one,
        y = two,
        dist = distfun(sample1, sample2)
      )
      merge_bind[[f-start+1]] <- diff_temp
    }
    data.table::rbindlist(merge_bind)
  }
  return(as.data.frame(pairwise))
}

#' Title
#'
#' @param total a number
#' @param ncores number of cores. If unspecified, will use all cores on machine
#'
#' @return data frame for use in foreach
#' @export
#' @examples
#' divide_cores(total = 1000)
divide_cores <- function(total, ncores = 1) {
  if (!(is.numeric(total) | is.integer(total))) stop("'total' must be a ONE number")
  if (ncores == 1) {
    return(data.frame(from = 1, to = total))
  } else {
    doMC::registerDoMC(ncores)
    batch = total %/% ncores
    from = seq(from = 1, to = total-batch, by = batch)[1:ncores-1]
    to = c(seq(from = batch, to = total, by = batch), total)[1:length(from)]
    total_batch <- data.frame(cbind(from, to))
    # Add on the remainder
    total_batch <- rbind(total_batch, data.frame(
      from=max(total_batch$to)+1,
      to=total)
    )
    doMC::registerDoMC(ncores)
    return(total_batch)
  }
}

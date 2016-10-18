#' Compute the snow reservoir
#'
#' Compute the snow reservoir
#' @param snow snow
#' @keywords snow
#' @export
#' @examples
#' \dontrun{
#' reservoir()
#' }
reservoir <-function(snow){

  #SWE pr. elevation zone
  swe_h <- snow$sca*snow$spd

  #mean catchment SWE ,must multiply with SCA to have arealvalues
  snomag <- mean(swe_h)

  middelsca <- sum(snow$sca)/length(snow$sca)

  snofritt <- 1-middelsca

  res <- list(snomag = snomag,
              swe_h =swe_h,
              middelsca = middelsca,
              snofritt = snofritt)
  return(res)
}

#' Initialize information related to snow
#'
#' Initialize the information related to snow
#' @param method method for the initialization, "load", "source", "manual"
#' @param path directory where to get the files, in used when method is "load" or "source"
#' @param snomag snow mag, NEED MORE EXPLANATION
#' @param swe_h swe_h, NEED MORE EXPLANATION
#' @param middelsca averaged sca
#' @param snofritt snowfree, NEED MORE EXPLANATION
#' @keywords snow
#' @export
#' @examples
#' \dontrun{
#' init.snowReservoir()
#' }
init.snowReservoir <-function(method=NULL,path=NULL,snomag=NULL,swe_h=NULL,middelsca=NULL,snofritt=NULL){

  snowReservoir <- switch(method,
    "manual"    = init.snowReservoir.manual(snomag=snomag,swe_h=swe_h,middelsca=middelsca,snofritt=snofritt),
    "load"      = init.snowReservoir.load(path=path),
    "source"    = init.snowReservoir.source(path=path),
    (message=paste0("Invalid method:", method,".")))

  return(snowReservoir)
}

init.snowReservoir.manual <- function(snomag,swe_h,middelsca,snofritt){
  res <- list(snomag=snomag,
               swe_h=swe_h,
               middelsca=middelsca,
               snofritt=snofritt)
  return(res)
}

init.snowReservoir.load <- function(path){
  load(paste0(path,"snowReservoir.rda"))
  return(snowReservoir)
}

init.snowReservoir.source <- function(path){
  source(paste0(path,"snowReservoir.R"),local=TRUE)
  return(snowReservoir)
}

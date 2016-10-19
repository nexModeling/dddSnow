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
#' init.snow()
#' }
init.snow <-function(method=NULL,path=NULL,snomag=NULL,swe_h=NULL,middelsca=NULL,snofritt=NULL){

  snow <- switch(method,
    "manual"    = init.manual(snomag=snomag,swe_h=swe_h,middelsca=middelsca,snofritt=snofritt),
    "load"      = init.load(path=path),
    "source"    = init.source(path=path),
    (message=paste0("Invalid method:", method,".")))

  return(snow)
}

init.manual <- function(snomag,swe_h,middelsca,snofritt){
  res <- list(snomag=snomag,
               swe_h=swe_h,
               middelsca=middelsca,
               snofritt=snofritt)
  return(res)
}

init.load <- function(path){
  load(paste0(path,"snowReservoir.rda"))
  return(snow)
}

init.source <- function(path){
  source(paste0(path,"snowReservoir.R"),local=TRUE)
  return(snow)
}

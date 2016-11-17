#' Initialize information related to snow
#'
#' Initialize the information related to snow
#' @param method method for the initialization, "load", "manual"
#' @param path directory where to get the files, in used when method is "load"
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
    (message=paste0("Invalid method:", method,".")))

  return(snowReservoir)
}

init.snowReservoir.manual <- function(snomag,swe_h,middelsca,snofritt){
  snowReservoir <- list(snomag=snomag,
               swe_h=swe_h,
               middelsca=middelsca,
               snofritt=snofritt)
  return(snowReservoir)
}

init.snowReservoir.load <- function(path){
  env <- environment()
  path <- normalizePath(file.path(path,"snowReservoir.rda"),mustWork = FALSE)
  load(path, envir=env)
  snowReservoir <- get("snowReservoir",envir = env)
  return(snowReservoir)
}

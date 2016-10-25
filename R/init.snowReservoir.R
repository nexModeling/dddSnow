#' Initialize information related to snow
#'
#' Initialize the information related to snow
#' @param method method for the initialization, "load", "manual"
#' @param path directory where to get the files, in used when method is "load"
#' @param snomag snow mag, NEED MORE EXPLANATION
#' @param swe_h swe_h, NEED MORE EXPLANATION
#' @param middelsca averaged sca
#' @param snofritt snowfree, NEED MORE EXPLANATION
#' @param SAVE Save the results, Boolean
#' @param pathResults Path of the results. By default: $HOME
#' @keywords snow
#' @export
#' @examples
#' \dontrun{
#' init.snowReservoir()
#' }
init.snowReservoir <-function(method=NULL,path=NULL,snomag=NULL,swe_h=NULL,middelsca=NULL,snofritt=NULL,SAVE=FALSE,pathResults="~/"){

  snowReservoir <- switch(method,
    "manual"    = init.snowReservoir.manual(snomag=snomag,swe_h=swe_h,middelsca=middelsca,snofritt=snofritt,SAVE=SAVE,pathResults=pathResults),
    "load"      = init.snowReservoir.load(path=path,SAVE=SAVE,pathResults=pathResults),
    (message=paste0("Invalid method:", method,".")))

  return(snowReservoir)
}

init.snowReservoir.manual <- function(snomag,swe_h,middelsca,snofritt,SAVE,pathResults){
  snowReservoir <- list(snomag=snomag,
               swe_h=swe_h,
               middelsca=middelsca,
               snofritt=snofritt)
  if (SAVE){
    pathInit <- paste0(pathResults,"init/")
    dir.create(pathInit, showWarnings = FALSE)
    do.call("save", list(obj="snowReservoir", file=paste0(pathInit,"snowReservoir.rda")))
  }
  return(snowReservoir)
}

init.snowReservoir.load <- function(path,SAVE,pathResults){
  load(paste0(path,"snowReservoir.rda"))
  if (SAVE){
    pathInit <- paste0(pathResults,"init/")
    dir.create(pathInit, showWarnings = FALSE)
    do.call("save", list(obj="snowReservoir", file=paste0(pathInit,"snowReservoir.rda")))
  }
  return(snowReservoir)
}

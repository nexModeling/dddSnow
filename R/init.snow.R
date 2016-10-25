#' Initialize information related to snow
#'
#' Initialize the information related to snow
#' @param method method for the initialization, "load", "source", "manual"
#' @param path directory where to get the files, in used when method is "load" or "source"
#' @param isoil precipitation and/or snowmelt from the elevation zones, vector(level zone)
#' @param gisoil glaciermelt from the elevation zones, vector(level zone)
#' @param sca snow coverage (NEED MORE EXPLANATION)
#' @param spd Snow Water Equivalent in BV (NEED MORE EXPLANATION)
#' @param wcd Free Water in BV (NEED MORE EXPLANATION)
#' @param nsno number of event (NEED MORE EXPLANATION)
#' @param alfa alfa parameter i nedbor gamma fordeling (NEED MORE EXPLANATION)
#' @param ny ny parameter i nedbor gamma fordeling (NEED MORE EXPLANATION)
#' @param snowfree percentage of snowfree area
#' @param SAVE Save the results, Boolean
#' @param pathResults Path of the results. By default: $HOME
#' @keywords snow
#' @export
#' @examples
#' \dontrun{
#' init.snow()
#' }
init.snow <-function(method=NULL,path=NULL,isoil=NULL,gisoil=NULL,spd=NULL,wcd=NULL,sca=NULL,nsno=NULL,alfa=NULL,ny=NULL,snowfree=NULL,SAVE=FALSE,pathResults="~/"){

  snow <- switch(method,
    "manual"    = init.snow.manual(isoil=isoil,gisoil=gisoil,spd=spd,wcd=wcd,sca=sca,nsno=nsno,alfa=alfa,ny=ny,snowfree=snowfree,SAVE=SAVE,pathResults=pathResults),
    "load"      = init.snow.load(path=path,SAVE=SAVE,pathResults=pathResults),
    (message=paste0("Invalid method:", method,".")))

  return(snow)
}

init.snow.manual <- function(isoil,gisoil,spd,wcd,sca,nsno,alfa,ny,snowfree,SAVE,pathResults){
   snow <- list(isoil=isoil,
               gisoil=gisoil,
               spd=spd,
               wcd=wcd,
               sca=sca,
               nsno=nsno,
               alfa=alfa,
               ny=ny,
               snowfree = snowfree)
    if (SAVE){
      pathInit <- paste0(pathResults,"init/")
      dir.create(pathInit, showWarnings = FALSE)
      do.call("save", list(obj="snow", file=paste0(pathInit,"snow.rda")))
    }
    return(snow)
}

init.snow.load <- function(path,SAVE,pathResults){
  load(paste0(path,"snow.rda"))
  if (SAVE){
    pathInit <- paste0(pathResults,"init/")
    dir.create(pathInit, showWarnings = FALSE)
    do.call("save", list(obj="snow", file=paste0(pathInit,"snow.rda")))
  }
  return(snow)
}

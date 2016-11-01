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
#' @keywords snow
#' @export
#' @examples
#' \dontrun{
#' init.snow()
#' }
init.snow <-function(method=NULL,path=NULL,isoil=NULL,gisoil=NULL,spd=NULL,wcd=NULL,sca=NULL,nsno=NULL,alfa=NULL,ny=NULL,snowfree=NULL){

  snow <- switch(method,
    "manual"    = init.snow.manual(isoil=isoil,gisoil=gisoil,spd=spd,wcd=wcd,sca=sca,nsno=nsno,alfa=alfa,ny=ny,snowfree=snowfree),
    "load"      = init.snow.load(path=path),
    (message=paste0("Invalid method:", method,".")))

  return(snow)
}

init.snow.manual <- function(isoil,gisoil,spd,wcd,sca,nsno,alfa,ny,snowfree){
   snow <- list(isoil=isoil,
               gisoil=gisoil,
               spd=spd,
               wcd=wcd,
               sca=sca,
               nsno=nsno,
               alfa=alfa,
               ny=ny,
               snowfree = snowfree)
    return(snow)
}

init.snow.load <- function(path){
  load(paste0(path,"snow.rda"))
  return(snow)
}

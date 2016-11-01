#' Update the information related to snow
#'
#' Update the information related to snow
#' @param htempX temperature for each elevation zone
#' @param hprecipX precipitation for each elevation zone
#' @param scaobX snow coverage observation
#' @param snowX snow
#' @param modelSnow
#'  list(nbLevelZone,unitsnow,n0,Ws,TS,CX,CFR,CGLAC,gca,UP)
#' @param modelPrecipLZ
#'  list(nbLevelZone,Plr,hfelt,midmetp)
#' @keywords snow
#' @export
#' @examples
#' \dontrun{
#' stateX.snow()
#' }
stateX.snow <-function(htempX,hprecipX,scaobX,snowX,modelSnow,modelPrecipLZ){


  hson <- modelSnow$nbLevelZone #elevation zones

  updateGisoil <- rep(NA,hson)
  updateIsoil  <- rep(NA,hson)
  updateSpd    <- rep(NA,hson)
  updateWcd    <- rep(NA,hson)
  updateSca    <- rep(NA,hson)
  updateNsno   <- rep(NA,hson)
  updateAlfa   <- rep(NA,hson)
  updateNy     <- rep(NA,hson)
  updateSnowfree <- rep(NA,hson)
  if (is.na(scaobX)){
     scaobX <- rep(NA,hson)
  }

  for(idim in 1:hson){

    tmp <- stateLZ(htemp=htempX[idim],hprecip=hprecipX[idim],scaob=scaobX[idim],
                        modelSnow=modelSnow,modelPrecipLZ=modelPrecipLZ,
                        gca = modelSnow$gca[idim],
                        sca=snowX$sca[idim],
                        spd=snowX$spd[idim],
                        wcd=snowX$wcd[idim],
                        nsno=snowX$nsno[idim],
                        alfa=snowX$alfa[idim],
                        ny=snowX$ny[idim])

    updateGisoil[idim] <- tmp$gisoil
    updateIsoil[idim]  <- tmp$isoil
    updateSpd[idim]    <- tmp$spd
    updateWcd[idim]    <- tmp$wcd
    updateSca[idim]    <- tmp$sca
    updateNsno[idim]   <- tmp$nsno
    updateAlfa[idim]   <- tmp$alfa
    updateNy[idim]     <- tmp$ny
    updateSnowfree[idim] <- tmp$snowfree
    rm(tmp)

  }

  snowUpdate <- snowX
  snowUpdate$gisoil <- updateGisoil
  snowUpdate$isoil  <- updateIsoil
  snowUpdate$spd    <- updateSpd
  snowUpdate$wcd    <- updateWcd
  snowUpdate$sca    <- updateSca
  snowUpdate$nsno   <- updateNsno
  snowUpdate$alfa   <- updateAlfa
  snowUpdate$ny     <- updateNy
  snowUpdate$snowfree <- updateSnowfree

  return(snowUpdate)
}

#' Update the information related to snow at one level zone
#'
#' Update the information related to snow at one level zone

#' @param htemp temperature for each elevation zone
#' @param hprecip precipitation for each elevation zone
#' @param scaob snow coverage observation
#' @param modelSnow list of parameters related to snow
#'  list(nbLevelZone,unitsnow,n0,Ws,TS,CX,CFR,CGLAC,gca,UP)
#' @param modelPrecipLZ list of parameters related to precipLZ
#'  list(nbLevelZone,Plr,hfelt,midmetp)
#' @param gca fraction of glacier pr elevation zone, fraction of glacier covered area pr elevation zone
#' @param sca snow coverage (NEED MORE EXPLANATION)
#' @param spd Snow Water Equivalent in BV (NEED MORE EXPLANATION)
#' @param wcd Free Water in BV (NEED MORE EXPLANATION)
#' @param nsno number of event (NEED MORE EXPLANATION)
#' @param alfa alfa parameter i nedbor gamma fordeling (NEED MORE EXPLANATION)
#' @param ny ny parameter i nedbor gamma fordeling (NEED MORE EXPLANATION)
#' @keywords snow
#' @export
#' @examples
#' \dontrun{
#' stateLZ()
#' }
stateLZ <-function(htemp,hprecip,scaob,modelSnow,modelPrecipLZ,
                        gca,sca,spd,wcd,nsno,alfa,ny) {

  # Precipitation as rain
  PR <-ifelse( (htemp>modelPrecipLZ$TX), hprecip*modelPrecipLZ$Pc,0)

  # Precipitation as snow
  PS <-ifelse( (htemp>modelPrecipLZ$TX), 0,hprecip*modelPrecipLZ$Sc)

  # snow melt, degreeday melting
  MW <- ifelse( (htemp<modelSnow$TS), modelSnow$CX*(htemp-modelSnow$TS)*modelSnow$CFR, modelSnow$CX*(htemp-modelSnow$TS))  #negative melt is turned into snow

  # glacier melt, degreeday melting
  MWGLAC <- ifelse( (htemp<modelSnow$TS), 0, modelSnow$CGLAC*(htemp-modelSnow$TS) )

  # snow distribution routine
  frasno <-snogamma(PRX=PR,PSX=PS,MWX=MW,scax=sca,scaobx=scaob,spdx=spd,
                         wcdx=wcd,prox=modelSnow$Ws,nsnox=nsno,alfax=alfa,
                         nyx=ny,alfa0x=modelPrecipLZ$a0,ny0x=modelSnow$n0,ac=modelPrecipLZ$a0,hc=modelPrecipLZ$d,
                         UP=modelSnow$UP)
                         
  update <- list(gisoil = MWGLAC,
                 isoil  = frasno$isoil,
                 spd    = ifelse( (frasno$spd<20000),frasno$spd,20000),
                 wcd    = frasno$wcd,
                 sca    = frasno$sca,
                 nsno   = frasno$nsno,
                 alfa   = frasno$alfadyn,
                 ny     = frasno$ny,
                 snowfree = ifelse((sca<gca),1,0)) #test for glaciermelt,  no melt if snowcovered, snowfree[idim] =0.0

  return(update)
}

#' Update dynamical values of nu and alfa
#'
#' The function \code{varc()} updates and returns dynamical values of
#' nu and alfa, of the snowroutine. The correlation coefficient is evaluated
#' as the sum of the correlation N*N, 2*N*u and u*u matrices
#' Should be a decreasing value with n, large n small c
#' All variables must be conditional (i.e. calculated on snowcovered area)
#' Note the distinction between conditional and unconditional units of melt
#' 18.03.2015 Recodet using only the correlation function. Alfanull ( sdalphax) and nynull( sdnux) are input
#' @param ppa SCA,relative reduction of SCA, previously accumulated units,current accu units and melt units
#' @param ac (previously alfanull) parameters of relationship mean standarddeviation of conditional precip.
#' @param drange parameters of relationship mean standarddeviation of conditional precip.
#' @param nudyn initial distr parameters, current distr parameters
#' @param alphadyn initial distr parameters, current distr parameters
#' @param sdnux  (ny0) initial distr parameters, current distr parameters
#' @param sdalphax  (alfa0) initial distr parameters, current distr parameters
#' @param nnn  SCA,relative reduction of SCA, previously accumulated units,current accu units and melt units
#' @param u  SCA,relative reduction of SCA, previously accumulated units,current accu units and melt units
#' @param n  SCA,relative reduction of SCA, previously accumulated units,current accu units and melt units
#' @param redsca  SCA,relative reduction of SCA, previously accumulated units,current accu units and melt units
#' @return a list of three updated values: nudyn,alphadyn,nnn
#' @keywords snow
#' @export
#' @examples
#' \dontrun{
#' varc()
#' }
varc <-function(ppa,ac,drange,nudyn,alphadyn,sdnux,sdalphax,nnn,u,n,redsca) {

   nnn <- round(nnn) #previously accumulated units
   n <- round(n)     # new accumulated units
   u <- round(u)     # melting units
   krysscorr <- 0.0

   #variance in: remember that nudyn inkludes nnn
   vardyn <- nudyn/alphadyn^2
   #correlation =1 for index (1,1)
   corrvec <-function(nnn,drange) # Correlation function
   {
     corrv <- exp(-nnn/drange)
     return(corrv)
   }

  # ACCUMULATION ####################
  if(n > 0)
  {
   #if nnn = 0 then the totalvariance is equal to autocovariance calculated for n (fresh start)
   #vardyn and kryssvar equals 0 and the mean value equals n*unitmean (unitmean = sdnux/sdalphax).
	   if (nnn==0)
	   {
       vartot <-n*(sdnux/sdalphax^2)*(1 + (n-1)*corrvec(n,drange))
  	   meantot <- n*(sdnux/sdalphax)
	   }

	   if(nnn > 0)# must scale the corrvec
     {
#      varoldcov <- (nnn+n)*(sdnux/sdalphax^2)*(1 + ((nnn+n)-1)*corrvec((nnn+n),drange)) # 19.03.2014
       varoldcov <- vardyn + n*(sdnux/sdalphax^2)*(1 + (n-1)*corrvec(n,drange)) # 08.01.2016  antar uavhengighet mellom gammelt og nytt, hensikten er a oke variansen
       # calculating the n matrix
  	   varnewcov <-n*(sdnux/sdalphax^2)*(1+(n-1)*corrvec(n,drange)) #denne er OK
       #total variance: (nnn+n)*ppa +n*(1-ppa)
	     vartot <- (varoldcov*ppa^2)+(varnewcov*(1-ppa)^2)#18.1 tar tatt vekk "i annen pa ppa'ene
       #new condtional mean for 100%coverage
       #Meantot kan ikke regnes ut som over for en oppdateringssituasjon. En slik akk.hendelsen forer ikke til 100% dekningsgrad!
       meantot <- ((ppa*(nnn+n))+((1-ppa)*n))*sdnux/sdalphax #For alle dekningsgrader
	   }
  } # for n >0

  # ABLATION ########################
  if(u > 0.0)
  {
    faktor <- (((nudyn*sdalphax^2)/(nnn*sdnux*alphadyn^2))+1+(nnn-1)*corrvec(nnn,drange))/(2*nnn)

    # We must ensure that u is conditional since u enters the routine as an unconditional value (melts over the entire area)
    meanubet <- (nnn-u)*(sdnux/sdalphax)
    meantot <- meanubet/(1-redsca) # Is the CONDITIONAL VALUE
    betu <- round((1-redsca)*nnn-(nnn-u)) # according to the Skaugen and Randen, 2013
    if(betu < 0)betu <- 0 # taking into account that new conditional mean is higher than previous conditional mean, can't have that
    autovar <- (sdnux/sdalphax^2)*(betu+betu*(betu-1)*corrvec(betu,drange))
    corrun <-faktor*(betu/nnn)
    kryssvar <- (sdnux/sdalphax^2)*(2*nnn*betu*corrun)
    if (betu == 0)
    {
        autovar <- 0
        kryssvar <- 0
    }
    vartot <- vardyn+autovar-kryssvar #NB Kryssvar should be reduced. Vartot is the CONDITIONAL VALUE

  } #for u > 0.0 END ABLATION

  #Calculating the updated parmeters for the spatial distribution
  #NB note that nudyn and alphadyn includes the nnn
  if(meantot==0)# the reservoir (nnn) and melt (u/betu) match eaxtly
  {
      nudyn <- sdnux      #This feature gives nnn as one, i.e. never zero, but that is taken care of in other subroutines
      alphadyn <- sdalphax
  } else
  {
     nudyn <- (meantot^2/vartot)
  	 alphadyn <- meantot/vartot  # Conditional distribution
  }
  if(vartot =="NaN")
  {
  	 print(paste("vartot=NaN (ny_varc) nudyn=",nudyn,"alphadyn=",alphadyn, "meantot=", meantot,"vartot=",vartot,"nnn=",nnn,"n=",n,"u=",u))
     pause ()
  }

  nnn <- round((nudyn/alphadyn)/(sdnux/sdalphax))#sikrer korrrespondanse mellom nnn og nydyn og alfadyn
  alphadyn <- nudyn/((sdnux/sdalphax)*nnn)

  #returnerer resultater
  resultvarc<-list(nudyn = nudyn,
                   alphadyn = alphadyn,
                   nnn = nnn)

  return(resultvarc)
}

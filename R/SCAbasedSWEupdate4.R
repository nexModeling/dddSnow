#Function for calculating dynamical melt and accumulation based on satellite-
#observed SCA.
#-------------------------------------
#Input parameters:
#ppa,scaob(): current modelled SCA, SatObserved SCA
#nudyn,alphadyn,nu0x,alpha0x: current dynamical parameters, initial parameters
#nnn,n,u: accumulated units, current accu and melt units
#Output parameters:
#ppa,nudyn,alphadyn: updated modelled SCA, updated dynamical parameters (VARC)
#nnn: updated accumulated units
#source("F:\\HB\\Prosjekter\\HbvSat\\MCMC_Kalibrering\\script\\functions\\ny_varc.R")

updateSWEfromSCA <- function(ppa,scaob,nudyn,alphadyn,alpha0x,nu0x,nsno,nnn,n,u,ac,hc)
{
antall <-round(nudyn/alphadyn)+500
diff <-vector("numeric",antall)
a <- vector("numeric",antall)
s <- vector("numeric",antall)
xkrit <-vector("integer",2)
ppaorig <- ppa

#-------------------------------------
#SatSCA < ModSCA
#Case of observed SCA being less than modelled SCA. Modelled SWE will be
#reduced.
#Note that nudyn is  NOT pr unit but for the whole shebang
#-------------------------------------
      print(paste("SatSCA != ModSCA (prior): spt=", nudyn/alphadyn, " ppa=",ppa," scaob",scaob," nudyn",round(nudyn,3)," alphadyn",round(alphadyn,3)," nnn",round(nnn,3)," nsno",round(nsno,3)))
#Setting nnn to the current accu dist - eg. nsno(i) in case of no melt or accumulation prior to the adjustment
      if (n == 0 || u == 0)
      {
        nnn <- round(nsno)
      }
        #Here we may have to nullify the snow reservoir if scaob(i) < 0.01 (WAIT)

#Initializing parameters
      newsca <- 0.0
      redsca <- 0.0
      crosspos <- 0
      xxsfac <- 1.15
      limit <- round(nudyn/alphadyn)+500 #max limit for the crossing point between acc & abl distr
#      antall <- 1000
      if(scaob < ppa) rat <- scaob/ppa #ratio of observed & current modelled SCA
      if (scaob > ppa) rat <- ppa/scaob
#Starting iteration newsca = 1-gamma against satSCA/modSCA
      xx <- nudyn
      a[1:limit] <- dgamma((1:limit),xx,alphadyn) # pdf akkumulasjonsfordeling
      xxs <-xx/xxsfac
      #alphaxxs <- 1.1*alphadyn
      alphaxxs <- xxsfac*alphadyn

      while( (newsca <= rat) && (xxs/alphaxxs > 1.0)) #siste term er for at vi ikke skal itererer oss vekk for smaa verdier
      {
       s[1:limit] <- dgamma((1:limit),xxs,alphaxxs) # pdf smeltefordeling i mm
        diff[1:limit] <- a[1:limit]-s[1:limit]
#Finding the crossingpoint
        crosspos <- which(diff > 0)
        xkrit[1:2] <- 0
        xkrit[2] <- crosspos[1] #i mm
#print(paste("xkrit[2]=", xkrit[2]))
        if (length(crosspos) < 1) xkrit[2] <- 1
        if (xkrit[2] == 0) stop("dette gaar ikke")
#Start exact method
        pa <-round(pgamma(xkrit[2],xx,alphadyn),5) #i mm
        ps <-round(pgamma(xkrit[2],xxs,alphaxxs),5)# i mm
        redsca <- pa + (1-ps)
        newsca <- 1-redsca      #Actual SCA is ppa <- ppa *newsca
#Updated mean for the changed SCA
        spt <- (1/(1-redsca))*(nnn-round(xxs/alphaxxs*10))*(nu0x/alpha0x)# Nytt betinget middel likn. 12, dvs middel for den delen av feltet som har snoe. stp ganges med 10 siden step er i mm
       # alphaxxs <- alphaxxs+0.001
       alphaxxs <- xxsfac*alphaxxs
        xxs <- xxs/xxsfac
      } #endif newsca < rat

if(scaob < ppa)# SWE is reduced, u > 0. Output is conditionsl mean SCA is the observed
{
 u <- round(xxs/alphaxxs*10)
 fra_varc <- varc(1.0,ac,hc,nudyn,alphadyn,nu0x,alpha0x,nnn,u,0,redsca) #
 xxny <- fra_varc$nudyn
 alphany <- fra_varc$alphadyn
#print(paste("satSCA < modSCA (prior): spt=",nnn/10," SCA=",ppa, sep=""))
print(paste("satSCA < modSCA (posterior): spt=",round(xxny/alphany,3)," SCA= ",scaob,sep=""))
}

if(scaob > ppa) # SWE is increased  n > 0. Output is conditional mean, SCA is the observed
{
 n <- round(xxs/alphaxxs*10)
 #fra_varc <- varc((ppa/scaob),ac,hc,nudyn,alphadyn,nu0x,alpha0x,nnn,0,n, redsca) #redsca ikke relevant her. ppa/scaob er den relative forskjellen mellom dekninggrad foer og etter n akkumuleringer SCA etter er 1.0
 fra_varc <- varc(1.0,ac,hc,nudyn,alphadyn,nu0x,alpha0x,nnn,0,n,redsca) #
 xxny <- fra_varc$nudyn
 alphany <- fra_varc$alphadyn
#print(paste("satSCA > modSCA (prior): spt=",nnn/10," SCA=",ppa,sep=""))
print(paste("satSCA > modSCA (posterior): spt=",round(xxny/alphany,3)," SCA= ",scaob,"n = ",round(n),sep=""))
}

ppa <- scaob
nudyn <- xxny
alphadyn <- alphany

nnn <- (nudyn/alphadyn)*(alpha0x/nu0x)#Oppdatert nnn i praksis: middel ganger 10
spt <- nudyn/alphadyn # opppdatert betinget middel verdi NB masse vann fordi SCA er stoerre

resultSCAupdate <- NULL
resultSCAupdate$nudyn <- nudyn
resultSCAupdate$alphadyn <- alphadyn
resultSCAupdate$ppa <- ppa
resultSCAupdate
} #end func updateSWEfromSCA

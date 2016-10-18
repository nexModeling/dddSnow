#' snogamma
#'
#' snogamma
#' NEED MORE EXPLANATION
#' WARNING:
#' Some input parameters described in the source file
#' are however not involved:
#' - cc   :temporal correlasjon mellom hendelser
#' - pro  :max prosent fritt vann i sno
#' - idim :no hoydesone'
#' HUSK AT SPD, WCD ER UBETINGEDE VERDIER. MA GANGES MED PPA FOR A FA AREALVERDIER!!!
#' og Output:
#' ISOIL            :runoff til grunn eller overflate avrenningn UBV
#' SPD  	      :swe BV
#' WCD              :fritt vann i snopakke BV
#' nsno             :antall hendelser
#' sca              :oppdatert snodekning
#' hc og ac bekriver sammenhengen mellom  areelt middel og standdardavvik av nedbor se BREMS
#' @param PRX nedbor som regn
#' @param PSX nedbor som sno
#' @param MWX potenial melt/refreeze(negative) mm
#' @param scax dekningsgrad (i hver hoydesone)(array(idim))
#' @param scaobx snow coverage observation
#' @param spdx Snow Water Equivalent in BV. NEED MORE EXPLANATION
#' @param wcdx Free Water in BV. NEED MORE EXPLANATION
#' @param prox Max Liquid water content in snow. Percentage of Snow Water Equivalent
#' @param nsnox number of event. NEED MORE EXPLANATION
#' @param alfax alfa parameter i nedbor gamma fordeling
#' @param nyx ny parameter i nedbor gamma fordeling
#' @param alfa0x alfa parameter i nedbor gamma fordeling
#' @param ny0x ny parameter i nedbor gamma fordeling
#' @param ac NEED MORE EXPLANATION
#' @param hc NEED MORE EXPLANATION
#' @param UP look at scaobx or not
#' @keywords snow
#' @export
#' @examples
#' \dontrun{
#' snogamma()
#'}

snogamma <-function(PRX,PSX,MWX,scax,scaobx,spdx,wcdx,prox,nsnox,alfax,nyx,alfa0x,ny0x,ac,hc,UP) {

 if( (scax*spdx < 0.1) && (PSX == 0.0)) {
    scax <-0.0
    ppa <-0.0
    spt <- 0.0
    isoil <-PRX
    nsnox <-0.0
    nn <- 0.0
    alfadyn <-alfa0x
    nydyn <- ny0x
    wct <-0.0
    xmw <-0.0
    if(UP ==1) {# vi oppdaterer fra satelittbilder
      if(scaobx > 0.03 && abs(ppa-scaobx) > 0.03) {
         if(ppa == 0) {
            ppa <- scaobx
            spt <- 5.0 # tillegg pga scaob > 0  Bor vel kalle varc
            wct <-0.25
            alfadyn <- alfa0x
           nydyn <- ny0x*(spt*(alfa0x/ny0x))
         }
         nn <- round(spt/(ny0x/alfa0x))
      }
    } # End oppdatering fra satelittbilder
 } else {#scax*spdx > 0.1 eller PSX == 0.0
    redsca <-0.0
    na <- 0.0              # antall enheter for akkumuleringshendelse
    u <-0.0               # antall enheter for smeltehendelse
    totps <-PSX            #nedbor som sno UBV
    spt <-spdx             #SWE BV disse holdes som BV
    wct <-wcdx             #fritt vann i snoen BV
    xmw <- MWX             #UBV smelting fra snorutina gjelder for omrader
                           #som har sno. Ma mult med ppa for a fa
                           #faktisk avrenning BV
    ppa <- scax            #reell dekningsgrad for feltet, ikke relativ til dekning!
    ppaold <- ppa
    nn <-round(nsnox)             #antall hendelser spt/(ny0/alfa)
                          # BV betyr betingede verdier (betinget av sno)
                          # UBV betyr ubetingede verdier (arealverdi)
    alfadyn <-  alfax     #Overforte variable
    nydyn <- nyx          #Overforte variable


    if(nn > 0.0) {
      alfadyn <-alfadyn
      nydyn <- round(nn) * nydyn # nydyn gar ut av rutine som pr enhet, men gar inn i rutine som: produkt et av nn* nydyn, her blir den jekket opp
    } else {
      nydyn <-ny0x
      alfadyn <- alfa0x
    }

#HUSK at spt er isdelen av SWE, mens wct er fritt vanndelen av SWE. Disse behandles separat

    totsn <-(spt+wct)# SWE og fritt vann i snoen BV far aktuell hendelse assosioert med scax

    #oppdatering av vanninnhold i sno
    if (xmw < 0.0) {# Gjenfrysing av fritt vann i sno
      totps <- PSX - xmw#gjenfrysing behandles som et snofall, skulle kanskje ikke bidra til endret fordeling, men, men
      wct <- wct + xmw  # reduserer vanninnholdet i snoen husk xmv er negativ
      if (wct < 0) wct <-0.0 #
      xmw <-0.0
    }

# spt er betingede verdier. For a fa arealverdier ma vi multiplisere med ppa
# Lager betinget arealvedi av totalt vann

#begynner na akkumulering og smelting med gamma fordeling
# AKKUMULERING
    if(totps > 0.1) {# totps er lik sno (pluss evt gjenfrysing) for hendelse
      na <- round(totps/(ny0x/alfa0x))        #number of units in accumulation event
      if(na == 0.0) na <-1.0 # round up if there is actually some snow

      if(nn == 0.0) {# no inital snow, i.e number of initial units equals zero
        ppa <- 1.0 # default setting avfter a snofall
        ppaold <- ppa

        if (na == 1.0) { # accumulation events equals zero
          alfadyn <- alfa0x
          nydyn <- ny0x
        } else { #na > 1.0
          fravarc <- varc(ppa,ac,hc,nydyn,alfadyn,ny0x,alfa0x,nn,u,na,redsca)
          alfadyn <- fravarc$alphadyn
          nydyn <- fravarc$nudyn
          nnvarc <- fravarc$nnn
          if(alfadyn < 0.0)print(paste("jeg er i snogamma 4.1 ppa=",ppa,ac,hc,nydyn,alfadyn,ny0x,alfa0x,nn,u,na, redsca, PSX, PRX))
          if(nydyn/alfadyn/na > 0.10000000001) {
            print(paste("diff mellom nnvarc og nn",nnvarc,nn, nnvarc-nn))
            print(paste("nn er 0 og na er >0, akkumulasjon",nydyn/alfadyn/n))
            #pause()
          }
        }
        spt <-  round(na)*(ny0x/alfa0x)# Conditional mean equals unconditional mean(ppa = 1.0) and is updatett due to snowfall (na >0, 1.0)

      } else {#nn and na > 0.0
        fravarc <- varc(ppa,ac,hc,nydyn,alfadyn,ny0x,alfa0x,nn,u,na,redsca)
        alfadyn <- fravarc$alphadyn
        nydyn <- fravarc$nudyn
        nnvarc <- fravarc$nnn
        nn <- round((nn*ppa))+ round(na) # # adjusting the mean for initial coverage < 1.0
        spt <- nn*(ny0x/alfa0x)# Conditional mean equals unconditional mean (ppa = 1.0) and is updatett due to snowfall (na >0, 1.0)
        ppa <-1.0
        ppaold <- ppa
        if(nydyn/alfadyn/nn >0.10000000001) {
          print(paste("diff mellom nnvarc og nn",nnvarc,nn, nnvarc-nn))
          print(paste("nn er >0 og na er >0, akkumulasjon",nydyn/alfadyn/nn))
          #pause()
        }
      } # slutt else nn > 0.0

      nn <- round(spt*alfa0x/ny0x) #oppdaterer etter akkumulasjon
      nsnox <- nn  #oppdaterer etter akkumulasjon
    } # if sentence for  totps > 0.1
    #for accumulation.  New coverage is set to 1.0  (wise?)

# SMELTING xmw
    if (xmw > 0.1) { # xmw reduserer SWE (is) bidrar til wct og avrenning

      u <-round(xmw/(ny0x/alfa0x)) #UBV skal smelte potensielt over hele feltet, men gjelder bare for snodekt areal
      # uu er potensiell smelting over hele snodekt areal
      # nsno() blir den nye n etter smeltehendelse NB betinget
      # nn  er den opprinnelige n far en smeltehendelse
      # Vi setter snomag == 0.0 hvis < 0,2 av hendelsesmean
      #setter u lik 1 hvis den er mindre enn 0.5

      if((nn - u) < 2) {

        nn <-0.0
        alfadyn <- alfa0x
        nydyn <- ny0x
        spt <-0.0
        wct <-0.0
        ppa <-0.0

      } else {# nn-u is greater than 2 units

         corrvec <-function(nnn,drange) {# Correlation function
            corrv <- exp(-nnn/drange)
            return(corrv)
         }

        vars <- (ny0x/alfa0x^2)*(u+u*(u-1)*corrvec(u,hc)) #varians av smelting
        ms <- u*(ny0x/alfa0x)                                #middel av smelting
        nys <- ms^2/vars #melting ny
        alfas <-ms/vars  #melt alfa
        alfaa <- alfadyn # accumulation alfa
        nya <- nydyn     #accumulation ny teste med nn*nydyn

        antall <-round(nya/alfaa) +1 #  Hvor lang vi trenger a ga  pa pdf'ene i mm
        diff <-vector("numeric",antall)
        a <- vector("numeric",antall)
        s <- vector("numeric",antall)
        xkrit <-vector("integer",2)

        s[1:antall] <- dgamma((1:antall),nys,alfas) # pdf smeltefordeling
        a[1:antall] <- dgamma((1:antall),nya,alfaa) # pdf akkumulasjonsfordeling
        diff[1:antall] <- a[1:antall]-s[1:antall]

        #find crossing
        krysspos <- which(diff > 0)
        xkrit[1:2] <-0
        xkrit[2] <-krysspos[1]
        if(length(krysspos)< 1)xkrit[2] <- 1
 	      if(xkrit[2]==0)stop("No crossing point!")
#start pa eksakt metode
        pa <- round(pgamma(xkrit[2],nya,alfaa),4)
        ps <-round(pgamma(xkrit[2],nys,alfas),4)
        redsca <- round((pa + (1-ps)),4)
#slutt pa eksakt metode
        if (redsca <= 0.0)redsca <- 0.0001
        if (redsca >=1.0)redsca <- 0.9999

        newsca <- 1-redsca      #reduksjon fra det som var for smelting. dvs at den reelle dekningen er ppa <- ppa *newsca
        spt <- (1/(1-redsca))*((nn-u)*(ny0x/alfa0x))# Nytt betinget middel, dvs middel for den delen av feltet som har sno. Som frode
        ppa <- ppaold*newsca     #oppdaterer reell dekningsgrad. NOTE that ppa is the coverage for the elevation zone
    # Det er to krav til u:
    # 1) den skal vaere sa stor at vi har mindre betinget middel etter smelting enn for (sikrer riktig form pa pdf'er)
    # 2) den skal ikke vaere sa stor at middelverdi blir negativ

        if(spt >= (nn*(ny0x/alfa0x))) { #betinget etter smelting kan ikke vaere mindre enn betinget far smelting
           u <- nn-(nn*(1-redsca))+1#=nnn*redsca +1
           spt <- (1/(1-redsca))*(nn-u)*(ny0x/alfa0x)# sikrer krav no 1
           if(spt>0) {
              fravarc <- varc(ppa,ac,hc,nydyn,alfadyn,ny0x,alfa0x,nn,u,na,redsca)
              alfadyn <- fravarc$alphadyn
              nydyn <- fravarc$nudyn
           } else {
              ppa  <-0.0 # skal sikre krav no.2
           }
        }

       #NOTE it might be a problem that the potential melting XMW can never be actual, even for the areas with snow
       # because the areas left snow free are areas with snow less than XMW
#       print(paste("newsca",newsca))

        if(ppa < 0.02) {#Ting skal settes lik null
           nn <-0.0
           alfadyn <- alfa0x
           nydyn <- ny0x
           spt <-0.0
           wct <-0.0
           ppa<-0.0
        } else {
           newnn <- spt/(ny0x/alfa0x) #accumulation updated after ablation
           nn <-nsnox # old accumulation
           #print(paste("snogamma_ 7", nydyn, alfadyn,nn,u, n, redsca,pa,ps ))
           fravarc <- varc(ppa,ac,hc,nydyn,alfadyn,ny0x,alfa0x,nn,u,na,redsca)
           alfadyn <- fravarc$alphadyn
           nydyn <- fravarc$nudyn
           #print(paste("snogamma_ 7_etter oppdatering", nydyn, alfadyn,nn,u,n, redsca,ac,hc))
           nsnox <- newnn  #oppdaterer units etter smelting
           nn <- newnn
        }  #for hvis ppa > 0.05

      } #if (nsnox-uu < 0.1)
    }    #if(xmw >0.1)

    # ABLASJON er ferdig

    sptgml <- spt     # dagen hydrologi beregnes ut i fra ikke oppdatet spt. Effekten av oppdatert begynner dagen etter
    ppagml <-ppa       # dagen hydrologi beregnes ut i fra ikke oppdatet spt. Effekten av oppdatert begynner dagen etter
    # Updates SWE and SCA if there are any discrepancies between SatSCA and ModSCA and if cloud coverage is less than 5%.
    nnn <- nn

   if(UP ==1) {# vi oppdaterer fra satelittbilder
      if(scaobx > 0.03 && abs(ppa-scaobx) > 0.1) {
         if (ppa > 0.0) {
            fraSCAupdate <- updateSWEfromSCA(ppa,scaobx,nydyn,alfadyn,alfa0x,ny0x,nsnox,nnn,na,u,ac,hc)
            alfadyn <- fraSCAupdate$alphadyn
            nydyn <- fraSCAupdate$nudyn
            spt <- fraSCAupdate$nudyn/fraSCAupdate$alphadyn # updated according to satellite derived SCA
            ppa <- fraSCAupdate$ppa  # updated according to satellite derived SCA
         }
         nn <- round(spt/(ny0x/alfa0x))
      }
   } # End oppdatering fra satelittbilder

   #      if (scaobx <= 0.03 && scaobx >= 0.0)# SCAobsx comes in as -9999/100
   #      {
   #        print(paste("scaob <= 0.03 Alt settes til 0 scaob =",scaobx))
   #        nn <- 0.0
   #        alfadyn <- alfa0x
   #        nydyn <- ny0x
   #        spt <- 0.0
   #       wct <- 0.0
   #       ppa <- 0.0
   #     }


   #Vanninnhold i sno
   wcmax <- sptgml*prox #pro er allerede i prosent. Potensielt max niva i nytt snoreservoir
   #hele xmv  er gatt til a reduserer spt dvs fjerne SWE gatt til avrenning

   wct <- wct + PRX + xmw # oppdatering av vanninnhold i sno Overskuddet av smeltingen wil da ga til avrenning
   if(wct > wcmax)wct <- wcmax
   if(wct < 0.0)wct <- 0.0

   isoil <- (scax*totsn)+PSX+PRX-(ppagml*(sptgml+wct))#This is moisture according to model and input NOT due to updating. The next  time step will take updating into account
   #isoil <- (scax*totsn)+PSX+PRX-(ppa*(spt+wct))
                 #pr og ps er allerede arealverdi
                 #spt og wct ma multipliseres med ny ppa , dvs ikke oppdatert men dagens meteorologi er taken into account
   if(isoil < 0.0001)isoil <- 0.0

} #initiell if setning som er if(sca*spd < 1.0 && PSX == 0.0)
##  END FIRST IF  ##

if(nn <= 0.0) {
   nydyn <- ny0x
} else {
  nydyn <- nydyn/nn # parameter exits as not for units
}
#print(paste("ut av snogamma 2:nydyn=",nydyn, "alfadyn:",alfadyn,"nn=", nn))
#variabler som skal overfores
spdx <-spt
wcdx <-wct
scax <-ppa
nsnox <-nn

#returnerer resultater
resultsno <- list(isoil = isoil,
                  spdx = spdx,
                  wcdx = wcdx,
                  scax = scax,
                  nsnox = nsnox,
                  alfadyn = alfadyn,
                  nydyn = nydyn)
return(resultsno)
}

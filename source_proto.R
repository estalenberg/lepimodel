
  #code#####
    #inputs####
    gmultmin= input$postureInput[1] #1.3
    gmultmax=input$postureInput[2]#3
    furdepth=input$furdInput 
    O2eff=0.2 
    furcond=input$furcInput
    q10=input$qInput
    mass=input$massInput/1000 #kg 
    speaka=(mass*1000)^0.668
    massmouse=(mass*1000)^0.675
    
    
    #for statistical models
    sexa=as.numeric(mins$sex[mins$cat==category])
    seasa=as.numeric(mins$seas[mins$cat==category])
    pintake=as.numeric(mins$pintake[mins$cat==category])
    
    #body temp
    ta=seq(-5,65,by=1)
    tbc=(36.00289 + (-0.18903*ta) + (-0.16238*sexa) + 0.00587*(ta^2) + (-1.22523*seasa) + (-0.00046*sexa*(ta^2)) + (0.0014*(ta^2)*seasa))
    tbcc=-35.06997 + 1*tbc # stat model must be run on full tb prediction because its based on real data, ellip model needs to have a minimum temp because otherwise it doesn't flatten out 
    
    rmr_m18=(1320.5947 + (-60.3524*ta) + (0.5884*(ta^2)) + (-6.989*speaka) + (42.3723*seasa) +
               (-45.5984*sexa) + (-16.4901*tbcc) + (-4.0853*ta*seasa) + (0.3571*ta*speaka) + 
               47.1475*seasa*tbcc)

    
    bmrmin_m18=min(rmr_m18)
    
    #for ellipsoid
    bmrmin=bmrmin_m18
    bmrw=bmrmin/3600*20.9116

    rmr_stat=rmr_m18 #ml/hr
    rmrw=rmr_stat/3600*20.9116 #rq 0.96 , 20.1 is rq 0.8 
    rmr_statkjd= (rmrw / 1000 * 3600)*24

    dt=cbind.data.frame(ta,rmr_m18,tbc)
    v0=bmrmin
    f2 <- approxfun(dt$rmr_m18, dt$ta)
    tamin<-as.numeric(f2(v0))
    
    f2 <- approxfun(dt$rmr_m18, dt$tbc)
    tbmin<-as.numeric(f2(v0))
    tbmin=tbmin
    tb= ifelse(ta <= tamin,tbmin,
               (36.00289 + (-0.18903*ta) + (-0.16238*sexa) + 0.00587*(ta^2) + 
                  (-1.22523*seasa) + (-0.00046*sexa*(ta^2)) + (0.0014*(ta^2)*seasa)))
    
    
    #posture=if ta is < tnz make it gmult min, if higher then gradually inc to gmult max
    x<-seq(tamin,max(ta),by=1)
    length(x)
    y<-seq(gmultmin,gmultmax,length.out=length(x))
    m1<-lm(y~x)
    coef<-(coefficients(m1))
    b=(coef[2])*ta+(coef[1])
    y <- ifelse((ta <tamin), gmultmin,  b)
    posture	= y #1.3 # Shape, ratio of long to short axis of a prolate ellipsoid
    #climate###
    
    airT	= ta# Air temperature (deg C)
    windspd	= rep(0.01,(length(ta)))# Wind speed (m/s)
    coreT=tb
    windspd	= windspd+input$windInput #0.01 seq(0, 10, 0)
    rh	= rep(input$rhInput,(length(ta)))# Relative humidity (%) seq(-30,100,30)
    
    
    mouseelephant <- bmrw#*(q10^((tb-tbmin)/10)) 
    mouseelephantmlh<-mouseelephant/ 20.9116 * 3600 
    sexseas=rep(as.character(mins$sexseas[mins$cat==category]),length(ta))
    
    pwet=input$wetInput/100
    #stat pred ewl####
    ewlp=exp(-2.0017 + (0.1885*airT))
    
    data<-(cbind.data.frame(sexseas,ewlp,airT,windspd,rh,coreT,rmr_stat,rmr_statkjd,rmrw,mouseelephantmlh,pwet,pintake))
  
    #new parameters####
    Q10=q10
    basmult=1#*input$basmultInput
    stress=0.6
    stat=2#*as.numeric(input$bmrInput)
    
    #function####changed to match risk app on 13/oct
    ellipsoid <- function(posture, mass, coreT,furdepth, furcond,
                          O2eff, stress, airT, windspd, rh, Q10, basmult, tbmin,stat) {
      if(stat<2){ # use mouse-elephant curve #this determines which bmr calculation to use
        mouseelephant <- 10^(-1.462 + 0.675 * log10(mass * 1000)) * basmult
      }else #use statistical calculation
      {mouseelephant <- mouseelephant*basmult} 
      
      posture[posture==1]<-1.01 # avoid divide by zero
      basal <- mouseelephant * Q10 ^ ((coreT - tbmin) / 10) # Q10 correction in code tbmin=37
      a_coef <- 0.6
      b_coef <- 0.5
      sp_heat_air <- 1005.8
      volume <- mass / 1000
      b <- ((3 * volume) / (4 * 3.14159 * posture))^0.333
      c <- b
      a <- b * posture
      k_body <- 0.5 + (6.14 * b) + 0.439
      numerator <- a^2*b^2*c^2
      denominator <- a^2 * b^2 + a^2 * c^2 + b^2 * c^2
      Fraction <- numerator/denominator
      Rbody <- Fraction / (2 * k_body * volume)
      ao <- b*posture + furdepth/1000
      bo <- b + furdepth/1000
      co <- c + furdepth/1000
      Ecc_outr <- sqrt(ao^2 - co^2) / ao
      Aouter <- 2 * pi * bo^2 + 2 * pi * ((ao*bo)/Ecc_outr) * asin(Ecc_outr)
      Rinsul <- (bo - b)/(furcond * Aouter)
      
      #dry air function####
      DRYAIR=function (db = db, bp = 0, alt = 0) {
        tstd = 273.15
        pstd = 101325
        patmos = pstd * ((1 - (0.0065 * alt/288))^(1/0.190284))
        bp = rep(bp, length(patmos))
        bp[bp <= 0] = patmos[bp <= 0]
        densty = bp/(287.04 * (db + tstd))
        visnot = 1.8325e-05
        tnot = 296.16
        c = 120
        visdyn = (visnot * ((tnot + c)/(db + tstd + c))) * (((db + 
                                                                tstd)/tnot)^1.5)
        viskin = visdyn/densty
        difvpr = 2.26e-05 * (((db + tstd)/tstd)^1.81) * (1e+05/bp)
        thcond = 0.02425 + (7.038e-05 * db)
        htovpr = 2501200 - 2378.7 * db
        tcoeff = 1/(db + tstd)
        ggroup = 0.0980616 * tcoeff/(viskin * viskin)
        bbemit = 5.670367e-08 * ((db + tstd)^4)
        emtmax = 0.002897/(db + tstd)
        return(list(patmos = patmos, densty = densty, visdyn = visdyn, 
                    viskin = viskin, difvpr = difvpr, thcond = thcond, htovpr = htovpr, 
                    tcoeff = tcoeff, ggroup = ggroup, bbemit = bbemit, emtmax = emtmax))
      }
      dryair<-(DRYAIR(db=airT))
      
      visc_air <- dryair$visdyn
      k_air <- dryair$thcond
      den_air <- dryair$densty
      volcheck <- (4/3) * 3.14159 * a * b * c
      CharDimens <- volcheck^0.333
      Eccentricity <- sqrt(a^2 - c^2) / a
      area <- 2 * pi * b^2 + 2 * pi * ((a * b) / Eccentricity) * asin(Eccentricity)
      Re_number <- den_air * windspd * CharDimens/visc_air
      Pr_number <- (visc_air * sp_heat_air) / k_air
      q3p_num <- 2 * area * k_body * k_air * (2 + a_coef * (Re_number^b_coef)*Pr_number^0.333)*(coreT - airT)
      q3p_denom <- 2 * k_body * CharDimens * volcheck + area * Fraction * k_air * (2 + (a_coef * Re_number^b_coef) * Pr_number^0.333)
      g_in_air <- q3p_num/q3p_denom
      skinT <- coreT - (g_in_air * Fraction) / (2 * k_body)
      Gr_number <- abs(((den_air^2)*(1/(airT+273.15))*9.80665*(CharDimens^3)*(skinT - airT))/(visc_air^2))
      Nufree <- 2 + 0.6 * ((Gr_number^0.25) * (Pr_number^0.333))
      Nuforced <- 0.37 * Re_number^0.6
      Nutotal <- (Nufree^3 + Nuforced^3)^(1/3)
      hc <- Nutotal * k_air/CharDimens
      Rconv <- 1 / (hc * Aouter)
      Rrad <- 1 / (4 * Aouter * 1 * 0.95 * (5.7 * 10^-8) * (airT + 273.15)^3)
      Rtotal <- Rbody + Rinsul + (Rconv*Rrad)/(Rconv + Rrad)
      upcrit <- coreT - (basal*stress*Rtotal)
      lowcrit <- coreT - basal * Rtotal
      Qgen <- (coreT - airT) / Rtotal
      QgenFinal <- Qgen
      # QgenFinal[QgenFinal<basal]<-basal
      QgenFinal[QgenFinal<basal]<-basal[QgenFinal<basal]
      mlO2ph <- QgenFinal / 20.9116 * 3600
      
      #vapour function####
      VAPPRS=function (db = db)   {
        t = db + 273.16
        loge = t
        loge[t <= 273.16] = -9.09718 * (273.16/t[t <= 273.16] - 1) - 
          3.56654 * log10(273.16/t[t <= 273.16]) + 0.876793 * (1 - 
                                                                 t[t <= 273.16]/273.16) + log10(6.1071)
        loge[t > 273.16] = -7.90298 * (373.16/t[t > 273.16] - 1) + 
          5.02808 * log10(373.16/t[t > 273.16]) - 1.3816e-07 * 
          (10^(11.344 * (1 - t[t > 273.16]/373.16)) - 1) + 0.0081328 * 
          (10^(-3.49149 * (373.16/t[t > 273.16] - 1)) - 1) + log10(1013.246)
        esat = (10^loge) * 100
        return(esat)
      }
      esat <- VAPPRS(skinT)
      #  esat <- VAPPRS(coreT)
      #wet air function####
      WETAIR=function (db = db, wb = db, rh = rh, dp = 999, bp = 101325)    {
        tk = db + 273.15
        esat = VAPPRS(db)
        if (dp < 999) {
          e = VAPPRS(dp)
          rh = (e/esat) * 100
        }
        else {
          if (min(rh) > -1) {
            e = esat * rh/100
          }
          else {
            wbd = db - wb
            wbsat = VAPPRS(wb)
            dltae = 0.00066 * (1 + 0.00115 * wb) * bp * wbd
            e = wbsat - dltae
            rh = (e/esat) * 100
          }
        }
        rw = ((0.62197 * 1.0053 * e)/(bp - 1.0053 * e))
        vd = e * 0.018016/(0.998 * 8.31434 * tk)
        tvir = tk * ((1 + rw/(18.016/28.966))/(1 + rw))
        tvinc = tvir - tk
        denair = 0.0034838 * bp/(0.999 * tvir)
        cp = (1004.84 + (rw * 1846.4))/(1 + rw)
        if (min(rh) <= 0) {
          wtrpot = -999
        }
        else {
          wtrpot = 461500 * tk * log(rh/100)
        }
        return(list(e = e, esat = esat, vd = vd, rw = rw, tvinc = tvinc, 
                    denair = denair, cp = cp, wtrpot = wtrpot, rh = rh))
      }
      WETAIR.rh=function (db = db, rh = rh, bp = 101325) 
      {
        tk = db + 273.15
        esat = VAPPRS(db)
        e = esat * rh/100
        rw = ((0.62197 * 1.0053 * e)/(bp - 1.0053 * e))
        vd = e * 0.018016/(0.998 * 8.31434 * tk)
        tvir = tk * ((1 + rw/(18.016/28.966))/(1 + rw))
        tvinc = tvir - tk
        denair = 0.0034838 * bp/(0.999 * tvir)
        cp = (1004.84 + (rw * 1846.4))/(1 + rw)
        wtrpot = 461500 * tk * log(rh/100)
        return(list(e = e, esat = esat, vd = vd, rw = rw, tvinc = tvinc, 
                    denair = denair, cp = cp, wtrpot = wtrpot, rh = rh))
      }
      
      Qresp_gph <- (mlO2ph / 0.2094 / O2eff) * (WETAIR.rh(db = coreT, rh = 100)$vd - WETAIR.rh(db = airT, rh = rh)$vd) / 1000
      conv_H2O_loss <- 2501200 - 2378.7 * airT
      Qresp_W <- ((Qresp_gph / 3600) * conv_H2O_loss) / 1000
      Qresp_kjph <- Qresp_W / 1000 * 3600
      
      fmr_kjh<-QgenFinal / 1000 * 3600
      
      PctBasal <- QgenFinal / basal * 100
      status<-Qgen
      status[status>basal]<--100000000
      status[status<stress * basal]<--300000000
      status[status<100000000*-1]<--200000000
      status[status==100000000*-1]<-1
      status[status==300000000*-1]<-3
      status[status==200000000*-1]<-2
      
      H2Oloss_W <- (Qgen * -1) + basal
      H2O_gph <- (((H2Oloss_W) * 1000) / conv_H2O_loss) * 3600
      H2O_gph[H2O_gph<0]<-0
      #wtr mult####
      #H2O_gph=H2O_gph#*wtrmult
      massph_percent<-H2O_gph
      massph_percent[massph_percent<0]<-0
      timetodeath<-massph_percent
      massph_percent[massph_percent!=0]<-((H2O_gph[massph_percent != 0] / 1000) / mass) * 100
      timetodeath[timetodeath!=0]<-1 / (H2O_gph[timetodeath!=0] / (mass * 0.15 * 1000))
      
      mouseelephantmlh=basal/ 20.9116 * 3600 
      
      #licking#### 
      wet=(min(area)*pwet) #m2 - 48cm2=12% of 'area' but wet area is 7.5% of sa with limbs so maybe reduce the amount wet that?  
      
      #probability of licking is:
      pL=0.0007*(ta^2)-(0.0156*ta)+0.1098
      pL=ifelse(pL>1,1,pL)
      
      #probability#####
      #need maybe another input for wet or licking i.e. prob or no prob
      wet=wet#*pL
      wetcm=wet*10000 # changes from 0.6 to 21cm - could make increase from bmr only 
      
      lick=wet*((furdepth/1000)*0.025)#m3 %5 of fur depth is about 1mm high, .25% is about 0.5mm
      lick=lick*1000000#g up to 1.03g
      lickW=((lick / 3600) * conv_H2O_loss) / 1000
      #max 0.689 W to licking waterg and 1.04g
      
      esat2=esat*0.00750062 #mmHg
      e=WETAIR(db=airT,rh=rh)$e #vap press air pascals
      e=e* 0.00750062 #mmHg
      ke=0.446*(windspd*60)^(0.634) #from Clifford et al 1959 
      qe=ke*(esat2-e) #kcal/h #heat loss from evaporation - higher in cold temps? vap press of air is higher in higher temps
      #this is true - basically ke is the conductance assuming the boundary layer  
      qe= (qe*1000)*0.00116298 #W/m2, heat loss from 1m2 100 
      wetLW=qe*wet # in watts/m2 #heat lost to licking - the maximum amount of heat that you can lose
      wetLW[wetLW<0] = 0 #can never gain heat from evaporation
      evapgph<-(((wetLW) * 1000) / conv_H2O_loss) * 3600 #this is water loss from licking assuming they only replace as much water as is lost by evaporation 
      
      #Licking
      lick[lick<0] = 0 #can never gain water to physical licking
      
      evapgph[evapgph<0] = 0 #can never gain water from evaporation 
      
      QgenL=Qgen+wetLW # increase heat loss by evap but this is crazy in the dry season, just not possible
      QgenFinalL <- QgenL
      #need to decrease basal by wetLW
      basalL=ifelse(basal>mouseelephant,basal-wetLW,basal)
      #QgenFinal[QgenFinal<basal]<-basal
      QgenFinalL[QgenFinalL<basalL]<-basalL[QgenFinalL<basalL]
      QgenFinalL=ifelse(QgenFinalL<mouseelephant,mouseelephant,QgenFinalL)
      
      mlO2phL <- QgenFinalL / 20.9116 * 3600 #rmr ml/h
      #q resp is calculated from total rmr so heat loss to resp is already considered, and maybe water loss? 
      Qresp_gphL <- (mlO2phL/0.2094/O2eff) * (WETAIR(db = coreT, rh = 100)$vd - WETAIR(db = airT, rh = rh)$vd)/1000 
      Qresp_WL <- ((Qresp_gphL / 3600) * conv_H2O_loss) / 1000
      Qresp_kjphL <- Qresp_WL / 1000 * 3600
      
      PctBasalL <- QgenFinalL / basal * 100
      statusL <- ifelse(QgenL > basal, 1, ifelse(QgenL < 0.6 * basal, 3, 2)) # 1 = cold, 2 = happy, 3 = stress, stress=0.6
      
      #but for water loss we need heat production to be reduced and with evaporative water loss, 
      # as heat loss no longer equal to heat production
      QgenLw=Qgen-wetLW #i.e. reduce heat production by evap
      H2Oloss_WL <- (QgenLw * -1) + basal #water loss is simply bmr minus heat production
      H2O_gphL <- (((H2Oloss_WL) * 1000) / conv_H2O_loss) * 3600
      H2O_gphL[H2O_gphL<0] = 0
      
      #here this is wtr with evap plus the additional loss when lick>evap
      wtr=ifelse(((lick-evapgph)>0),(H2O_gphL+(lick-evapgph)),H2O_gphL) #so if evap is more than licking then dont reduce water loss, only inc if lick is more than evap
      
      
      massph_percentL <- ifelse(wtr < 0, 0, ((wtr / 1000) / mass) * 100)
      timetodeathL <- ifelse(wtr < 0, NA, 1 / (wtr / (mass * 0.15 * 1000))) #0.15% of mass
      
      
      Qresp_gphL[Qresp_gphL<0] = 0
      wtr[wtr<0] = 0
      fmr_kjhL<-QgenFinalL / 1000 * 3600
      
      
      return(cbind.data.frame(UpperCritTemp=upcrit, LowerCritTemp=lowcrit,Tskin=skinT, 
                              lick=lick,wtr=wtr,evapgph=evapgph, Qresp_gph = Qresp_gph, Qresp_W = Qresp_W,
                              Qresp_kjph = Qresp_kjph, Qgen = Qgen, QgenFinal = QgenFinal, mlO2ph = mlO2ph,
                              PctBasal = PctBasal, status = status, H2Oloss_W = H2Oloss_W, H2O_gph = H2O_gph,
                              massph_percent = massph_percent, timetodeath = timetodeath,
                              Qresp_gphL=Qresp_gphL, 
                              Qresp_WL=Qresp_WL, Qresp_kjphL=Qresp_kjphL,  QgenL=QgenL, 
                              QgenFinalL=QgenFinalL, mlO2phL=mlO2phL, PctBasalL=PctBasalL, 
                              statusL=statusL, H2Oloss_WL=H2Oloss_WL, H2O_gphL=H2O_gphL, 
                              massph_percentL=massph_percentL, timetodeathL=timetodeathL,fmr_kjh=fmr_kjh,fmr_kjhL=fmr_kjhL 
      ))
    }
    
    run<-ellipsoid(posture, mass, coreT,furdepth, furcond,
                   O2eff, stress, airT, windspd, rh, Q10, basmult, tbmin, stat)
    
    allout<-cbind.data.frame(data,run)
    
    allout$MR_kjph <- allout$QgenFinal / 1000 * 3600
    allout$MR_kjphL <- allout$QgenFinalL / 1000 * 3600
    
    
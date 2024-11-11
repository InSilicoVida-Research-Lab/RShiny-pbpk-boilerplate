server <- function(input, output) {
  output$v <- renderPlot({

  library(deSolve)
  library(coda)
  library(rootSolve)
  library(purrr)
  library(tidyverse)
  library(dplyr)

  
  States = unlist(c(data.frame(
    Astomach = 0,
    Agut= 0,		   #amount of BPA gut
    Aliver= 0,		   #amount of BPA liver
    Abrain=0,          #amount of BPA in brain 
    Akidney= 0,        #amount of BPA in kidney
    Afilterate= 0,		       #amount of BPA in filterate
    Afat= 0,          #amount of BPA in fAT
    Askin= 0,         #amount of BPA in skin
    Agonads= 0,       #amount of BPA in gonads
    Arestbody= 0,     #amount of BPA rest of the body
    Aurine= 0,		  #amount of BPA in urine
    Aplasma= 0,       #amount of BPA in plasma 
    BPAG_gut=0,
    BPAG_prod_gut= 0,
    BPAG_prod_gut_delay= 0,
    BPAG_prod_liver= 0,
    BPAG_prod_liver_delay= 0,
    BPAG_delay_in= 0,
    ABPAG= 0,
    BPAG_urine= 0,
    BPAS_prod_liver= 0,
    ABPAS= 0,
    BPAS_urine= 0,
    AUC= 0,
    AUCBPAG= 0,
    AUCBPAS= 0,
    AUCliver= 0,
    AUCbrain= 0,
    AUCfat= 0,
    AUCkidney= 0,
    AUCskin= 0,
    AUCgonads= 0,
    AUCrestbody= 0
  )))
  States
  
  BW = 35.9242
  HT=139.776
  #constant Fraction of blood flows to organs (blood flow rate)
  QCC = 6.847167;                   #Total Cardiac blood output (L/h/kg)  #16
  HCT = 0.390838; 				 #hematocrit percentage
  FQliver = 0.2570004;		     #Fraction cardiac output going to liver 
  FQbrain = 0.117;     	            		            #Fraction cardiac output going to brain
  FQlung = 0.034; 			 #Fraction cardiac output going to lung
  FQkidney = 0.177; 		     #Fraction cardiac output going to kidney 
  FQfilterate = 0.035;         #Fraction cardiac output to the filtrate compartment (20%of kidney blood flow)            # in pregnancy use dynamic model                                                      
  FQfat =  0.052; 		     #Fraction cardiac output going to fat  
  FQgut = 0.181;  			 #Fraction cardiac output going to gut
  FQskin = 0.058;	             #Fraction cardiac output going to Skin
  FQgonads = 0.0002;           #fraction cardiac output going to gonads
  FQprostate = 0.0013 ; 		 #fraction cardiac output going to prostate(Sj?gren et al., 2014)#   FQprostate = 0.0013; (Inaba, 1992)         
  
  #constant organ volume as a fraction of total body weight
  Fliver = 0.02493843;		         #Fraction liver volume
  Fbrain =  0.035961;   				                        #Fraction brain volume
  
  Fkidney = 0.00495446;			 #Fraction kidney volume  
  Ffilterate = 0.0004;	     #Fraction filtrate compartment volume (10% of kidney volume)
  Ffat = 0.2182532;                #fractional volume of fat  
  Fgut = 0.016;  				 #fractional volume of gut , orginal
  Fskin = 0.04474755;              #Fraction skin volume
  Fgonads = 0.002700002;            #fractional volume of gonads
  
  Fplasma = 0.04378135; 	         #fractional volume of plasma
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Physicochemical parameter For BPA
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  MW = 232;
  fu = 1;     			             	            #Free fraction of BPA in plasma assumed to be 1.
  
  k_liver_plasma = 0.73;			                    #Liver/blood partition coefficient
  k_brain_plasma = 2.8; 			                    #brain/blood partition coefficient
  k_kidney_plasma = 0.858;			                #Kidney/blood partition coefficient
  k_fat_plasma =    5.0;			                        #Fat/blood partition coefficient
  k_skin_plasma = 5.7;       	          		        #Skin/blood partition coefficient                         
  k_gonads_plasma = 2.6;                              #Gonads/blood partition coefficient    
  k_restbody_plasma = 2.7;			                #Rest of the body/blood partition coefficient
  
  
  #########################################################################
  # unscaled BPA oral absorption and metabolic parameters 
  #suffix C meaning unscaled paramter
  #########################################################################
  GEC = 3.5;              							     #Gastric emptying time 1/h  
  k0C = 0;                                                 #oral uptake of BPA from the stomach into liver, set to 0   
  k1C = 9;           							             #oral uptake of BPA from the small intestine into liver     
  kGlin_BPAGC = 50;                                        #transport of BPAG into liver from enterocytes
  vmaxgut_gluC = 22750;       				 		     #nM/h/BW Glucuronidation of BPA in gut
  kmgut_glu = 58400;                                       #nM
  RAMS_gutC = 0;   									     #no data on sulphation in gut
  Fbpagliver = 0.9;           							 #fraction of bpag in the liver taken up into serum   
  Fbpasliver = 1;              							 #fraction of bpas in the liver taken up into serum  
  vmaxliver_gluC= 452823.68;						             #nM/h/BW Glucuronidation of BPA in liver 
  kmliver_glu = 45800;                                     #nM
  vmaxliver_sulfC = 18301.49;                 				 #nM/h/BW sulfation  of BPA in liver 
  kmliver_sulf = 10100;   
  
  #EHR and urinary excretion of BPAG
  kurineC = 0.07;	                     					#urinary elimination rate constant (L/h/kg)   for BPA                        #optimized
  kurinebpagC =  0.5;      						      	#urinary excretion rate of bpag   (L/h/kg)    optimized
  kurinebpasC =  0.027;       	            		        #urinary excretion rate of bpas    (L/h/kg)   #optimized
  EHRtime   = 0.1;                                        # (h) Time until EHR occurs
  EHRrateC = 0.2;                      		            #EHR OF BPAG  IF Time analysis there in case of yang etal  but data is working still as they mention zero EHR time in case of 0.02 value          Optimized
  kde = .35;                                              #deconjugation and conjugation reverisible process estimated (L/hr) assume to be equivalent to clearence. 
  kde_BPAG=0.30;                          #BPAG deconjugation; (L/hr)
  
  
  
  #initializing the parameters
  QCblood = QCC*BW 		 			               #Initial cardiac output for blood L/h  
  QCplasma =QCblood *(1-HCT)   					   #Adjust initial cardiac output for plasma flow  
  245.979*(1-0.390838)
  Qliver= FQliver* QCplasma   	 				   #Plasma flow to liver
  Qbrain= FQbrain* QCplasma   	 				   #Plasma flow to brain
  Qkidney= FQkidney*QCplasma 							 #Plasma flow to kidney
  Qfilterate= 0.2*Qkidney 									 #Plasma flow to filterate
  Qfat = FQfat*QCplasma                    #plasma flow to fat  
  Qgonads = FQgonads*QCplasma         		 #plasma flow to gonads   
  Qskin = FQskin*QCplasma          #plasma flow to prostate 
  Qgut = FQgut*QCplasma          #plasma flow to prostate
  
  Qrestbody = QCplasma  -(Qliver + Qbrain + Qkidney + Qfat + Qgonads + Qskin+Qgut)  #plasma flow to rest of the body
  
  vliver = Fliver * BW  									 #Liver Volume  
  vbrain = Fbrain*BW												 #volume of brain
  vkidney = Fkidney*BW										 #volume of kidney
  vfilterate = Ffilterate*BW
  vfat = Ffat*BW                   				 #Volume of  fat    
  vgonads = Fgonads * BW 	 							 	 #gonads Volume  
  vskin = Fskin*BW                 #Volume of  skin
  vgut = Fgut*BW 
  vplasma=Fplasma*BW
  vrestbody=(0.84*BW - (vliver+vkidney + vgut + vplasma +  vfat+   vbrain  +vskin + vgonads))
  
  
  #biochemical parameters
  GE = GEC/(BW^0.25);              							       #Gastric emptying time 1/h  (scaled with dividing with body weight)
  k1 = k1C/(BW^0.25);             							           #oral uptake of BPA from the small intestine into liver     (scaled with dividing with body weight)               (k1 optimized)
  kGlin_BPAG = kGlin_BPAGC/(BW^0.25);                                 #transport of BPAG into liver from enterocytes
  vmaxgut_glu = vmaxgut_gluC*(BW^0.75);        				 	   #nM/h/BW Glucuronidation of BPA in gut
  RAMS_gut = 0;  									                       #no data on sulphation in gut
  
  vmaxliver_glu= vmaxliver_gluC*(BW^0.75); 						   #nM/h/BW Glucuronidation of BPA in liver 
  vmaxliver_sulf = vmaxliver_sulfC*(BW^0.75);                 		   #nM/h/BW sulfation  of BPA in liver 
  #vdbpag = 0.0438*BW_preg;                                			   #fractional volume of distribution  of BPAG 
  #vdbpas = 0.0438*BW_preg;                                   		       #fractional volume of distribution of BPAS
  
  vdbpag = 0.0438*BW                 #fractional volume of distribution  of BPAG 
  vdbpas = 0.0438*BW                 #fractional volume of distribution of BPAS
  
  kurine = kurineC*(BW^0.75); 	  						               #urine elimination of BPA L/h/BW
  kurinebpag =  kurinebpagC*(BW^0.75);     						   #urinary excretion rate of bpag          #optimized
  kurinebpas =  kurinebpasC*(BW^0.75);    	            		       #urinary excretion rate of bpas          #optimized
  EHRrate = EHRrateC/(BW^0.25);                      		           #EHR OF BPAG  IF Time analysis there in case of yang etal  but data is working still as they mention zero EHR time in case of 0.02 value          Optimized
  Fbpagliver1 = 1- Fbpagliver;        						           #fraction of bpag subject to EHR   (met2 symbol in case of yang etal)
  Fbpasliver1 = 1- Fbpasliver;               				               #fraction of bpas subject to EHR  (met2s symbol in case of yang etal)
  
  
  #Time dependent EHR of BPA metabolites
  
  
  #######################################################################
  #dosing function
  #######################################################################
  
  anaus <- function(t,t0,t1)
  {
    y <- (tanh(100*(t-t0)) - tanh(100*(t-t1)))/2
    return(y)}
  
  
  #Compile parameters to be computed  in initialized
  para <- data.frame( 
    
    QCblood,
    QCplasma,
    Qliver,
    Qbrain,
    Qkidney,
    Qfat,
    Qskin,
    Qgonads,
    Qgut,
    Qrestbody,
    vliver,
    vbrain,
    vkidney,
    vfat,
    vskin,
    vgonads,
    vgut,
    vplasma,
    vrestbody,
    k_liver_plasma,        
    k_brain_plasma,          
    k_kidney_plasma,		
    k_fat_plasma, 	     
    k_gonads_plasma,	 
    k_skin_plasma, 		
    k_restbody_plasma,    
    GE,
    k1,
    kGlin_BPAG,
    vmaxgut_glu,
    RAMS_gut,
    vmaxliver_glu,
    kmgut_glu,
    Fbpagliver,
    Fbpasliver,
    kmliver_glu,
    vmaxliver_sulf,
    kmliver_sulf,
    kde,
    kde_BPAG,
    vdbpag,
    vdbpas,
    kurine,
    kurinebpag,
    kurinebpas,
    EHRrate,
    Fbpagliver1,
    Fbpasliver1
  )
  
  
  # #TO round the parameter value of the data frame create a function as data frame are non numeric in order to deal with that create a funciton such that it will round up the 
  # round_df = function (x, digits){
  #   numeric_columns = sapply(x,class) == 'numeric'
  #   x[numeric_columns] = round(x[numeric_columns], digits)
  #   x
  # }
  # para = round_df(para, 2)
  para
  
  PBTKmod <- function(para) {
    derivs <- function(t, y, para)
    {
      with (as.list(c(y, para)),
            {
              ##############dosing
              dose.O=input$dose
              uptake.O <- dose.O                    #amount of uptake
              period.O <- 1    #3/60                                 #uptake period
              koa      <- uptake.O/period.O                    #uptake rate
              t0=0
              t1 <- t0 + period.O
              OD <- data.frame(t0, t1)
              
              
              ANAUS.O   <- sum(apply(OD, 1, function(x) anaus(t, x[1], x[2])))
              #input2=input
              
              
              
              
              Input     <- koa*ANAUS.O                          # Dosing (oral)
              if(t<EHRtime){kentero=0}else{kentero=EHRrate};    
              
              cgut = Agut/vgut;         
              cplasma = Aplasma/vplasma;
              cliver = Aliver/vliver
              cbrain = Abrain/vbrain
              ckidney = Akidney/vkidney
              cfat = Afat/vfat
              cskin = Askin/vskin;
              cgonads = Agonads/vgonads
              cfilterate = Afilterate/vfilterate;
              crestbody = Arestbody/vrestbody
              CBPAG = (ABPAG/(vdbpag +1E-34));
              CBPAS = ABPAS/(vdbpas + 1E-34);                                            #concentration of bpas in the system
              RAM_gut = vmaxgut_glu*cgut/(cgut+kmgut_glu);                               # Glucorinadation in gut of BPA
              RAM = vmaxliver_glu*cliver*fu/(cliver*fu+kmliver_glu);                     # Glucorinadation of BPA in liver
              RAMS1 = vmaxliver_sulf*cliver*fu/(cliver*fu+kmliver_sulf); 	               # Sulfation of BPA in liver
              BPAG_delay  = BPAG_prod_liver_delay + BPAG_prod_gut_delay - BPAG_delay_in
              
              dAstomach = -GE*Astomach + (Input*BW*1000)/MW
              dAgut = GE*Astomach - RAM_gut - k1*Agut;                            
              dAliver = Qliver*(cplasma*fu-  cliver*(fu/k_liver_plasma)) + k1*Agut  - RAM - RAMS1; 
              dAbrain = Qbrain *(cplasma*fu - cbrain*(fu/k_brain_plasma));   
              dAkidney = Qkidney *(cplasma*fu - ckidney*(fu/k_kidney_plasma))- kurine* ckidney;
              dAfilterate = Qfilterate *(cplasma*fu - cfilterate*fu);    
              dAfat = Qfat *(cplasma*fu - cfat*(fu/k_fat_plasma));                        					#amount of chemical in fat tissue mg
              dAskin  = Qskin *(cplasma*fu - cskin*(fu/k_skin_plasma));    	
              dAgonads = Qgonads *(cplasma*fu - cgonads*(fu/k_gonads_plasma))      					 	# Amount of chemical in gonads
              dArestbody = Qrestbody *(cplasma*fu - crestbody*(fu/k_restbody_plasma));
              dAurine= kurine* ckidney;
              dAplasma = Qfat * cfat*(fu/k_fat_plasma)  + Qliver*cliver*(fu/ k_liver_plasma) + (Qbrain *cbrain*(fu/k_brain_plasma))+(Qkidney *ckidney*(fu/k_kidney_plasma))+(Qrestbody *crestbody*(fu/k_restbody_plasma)) - (QCplasma* cplasma*fu) +(Qskin * cskin*(fu/k_skin_plasma)) +        (Qgonads * cgonads*(fu/k_gonads_plasma))
              dBPAG_gut = RAM_gut - kGlin_BPAG*BPAG_gut
              dBPAG_prod_gut= Fbpagliver *kGlin_BPAG*BPAG_gut;     
              dBPAG_prod_gut_delay= Fbpagliver1*kGlin_BPAG*BPAG_gut;  
              dBPAG_prod_liver=Fbpagliver * RAM;     
              dBPAG_prod_liver_delay=  Fbpagliver1*RAM ;   
              dBPAG_delay_in=  BPAG_delay*EHRrate;   
              dABPAG=Fbpagliver * RAM + Fbpagliver *kGlin_BPAG*BPAG_gut + BPAG_delay*EHRrate- kurinebpag*CBPAG
              dBPAG_urine= kurinebpag*CBPAG
              dBPAS_prod_liver=Fbpasliver * RAMS1;
              dABPAS=   Fbpasliver*RAMS1  +Fbpasliver* RAMS_gut- kurinebpas * CBPAS;
              dBPAS_urine=  kurinebpas * CBPAS
              dAUC=cplasma;
              dAUCBPAG=CBPAG;
              dAUCBPAS=CBPAS;
              dAUCliver=cliver;
              dAUCbrain=cbrain;
              dAUCfat=cfat;
              dAUCkidney=ckidney;
              dAUCskin=cskin;
              dAUCgonads=cgonads;
              dAUCrestbody=crestbody;
              
              
              
              
              dydt = c(dAstomach, dAgut,dAliver,dAbrain,dAkidney,dAfilterate,dAfat,dAskin,dAgonads,
                       dArestbody,dAurine,dAplasma,dBPAG_gut,dBPAG_prod_gut,dBPAG_prod_gut_delay,dBPAG_prod_liver,dBPAG_prod_liver_delay,
                       dBPAG_delay_in,dABPAG,BPAG_urine,BPAS_prod_liver,ABPAS,BPAS_urine,AUC,AUCBPAG,
                       AUCBPAS,AUCliver,AUCbrain,AUCfat,AUCkidney,AUCskin,AUCgonads,AUCrestbody)
              conc <- c(cplasma=cplasma, cliver = cliver, cbrain = cbrain, ckidney = ckidney,cfat = cfat,
                        cskin=cskin,cgonads = cgonads,cfilterate=cfilterate, crestbody=crestbody,CBPAG=CBPAG,
                        CBPAS=CBPAS)
              
              res  <- list(dydt, conc)
              return(res)
              
            })}
    
    times= seq(0, 24, 1)
    
    return(ode(y=States, func=derivs, times=times, parms=para, method="lsoda")) 
  }
  
  v<-PBTKmod(para)
  z=as.data.frame(v)
  plot(z$time, z$cplasma, type="l")
  })
  }
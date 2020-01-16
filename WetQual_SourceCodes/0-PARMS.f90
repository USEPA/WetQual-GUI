!**********************************************************************************************************************
! This file includes commom prime and general parameters
! The variables used in main program body, should not be shared if not necessary
!**********************************************************************************************************************
!
MODULE Pprime   
!**********************************************************************************************************************
SAVE
    INTEGER i,z,t,l    
    INTEGER zi,kk,yapsim
end module  Pprime
!**********************************************************************************************************************

!**********************************************************************************************************************
MODULE parm  ! General parameters used in the model
!**********************************************************************************************************************
SAVE
CHARACTER *1000 text
integer, parameter :: up =1461000      !750000   dt can be 0.005  
!integer, parameter :: wkup =110
integer, parameter :: yrup =7305 !it was 734 and Mehdi changed it to 75000
!integer, parameter :: nwk =47
integer tsim ,dn(0:up),n   ! n = 734 in inputfiles
integer ndisdata

real RainDist(0:240,2),dis_percent,rainfalldist,pday

real fact,alfa_velr_o,alfa_velr_s,Ri,c_uw,tt1,bofixed,sumset(0:up)
real ZOnw(0:yrup), ZOnss(0:yrup),ZOnsf(0:yrup), ZNw(0:yrup),ZNs1(0:yrup)       
real ZNs2(0:yrup),ZNO3w(0:yrup), ZNO3s1(0:yrup),ZNO3s2(0:yrup),ZOw(0:yrup),Za(0:yrup)     
real Zb(0:yrup),ZPw(0:yrup),ZPs1(0:yrup),ZPs2(0:yrup),Zms(0:yrup),Zmw(0:yrup)   
real Beta1Nw,Beta1NO3w,Beta1Pw,pKc1c2,ntday,koko,kvkv,fNfN,vrvelr_o,vrvelr_s 
real porw

real S_fixed, kminw_fixed, kmin1s_fixed, knw_fixed,kns_fixed, kden_fixed, Ss_fixed, Sw_fixed, Dpw_fixed      
Real ZNset(0:yrup),ZNvol(0:yrup),ZNO3den(0:yrup),ZPwset(0:yrup),Zmwset(0:yrup),ZMNw(0:yrup),ZPdif(0:yrup),ZPmin(0:yrup),ZNdif(0:yrup),ZNO3dif(0:yrup)

!**********************************************************************************************************************
Real SumVwONw,SumVwNw,SumVwNO3w,SumVwOw,SumVwPw,SumVwmw,TotMNw,SumVw,SumVs,SumVs1,SumVs2,Sum1Vw,Sum1Vs,Sum1Vs1,Sum1Vs2,Sum2Vw,Sum2Vs,Sum2Vs1,Sum2Vs2
Real SumVwONss,SumVwNs1,SumVwNO3s1,SumVwa,SumVwPs1,SumVwms,SumVwONsf,SumVwNs2,SumVwNO3s2,SumVwb,SumVwPs2
Real SumVwNset,SumVwNvol,SumVwNO3den,SumVwPwset,SumVwmwset,SumVwPdif,SumVwPmin,SumVwNdif,SumVwNO3dif
!Real Y1_Nset,Y1_Nvol,Y1_NO3den,Y1_Ndif,Y1_NO3dif,Y1_Pwset,Y1_Pdif,Y1_Pmin,Y1_mwset,Y1_MNw,Y2_Nset,Y2_Nvol,Y2_NO3den,Y2_Ndif,Y2_NO3dif,Y2_Pwset,Y2_Pdif,Y2_Pmin,Y2_mwset,Y2_MNw,Pwout_last(10)
!Real Y1_ONw,Y1_ONsf,Y1_ONss,Y1_Nw,Y1_Ns1,Y1_Ns2,Y1_Ow,Y1_NO3w,Y1_NO3s1,Y1_NO3s2,Y1_Pw,Y1_Ps1,Y1_Ps2,Y1_a,Y1_b,Y1_mw,Y1_ms,YY1_ONw,YY1_Nw,YY1_NO3w,YY1_Pw,YY1_mw
!Real Y2_ONw,Y2_ONsf,Y2_ONss,Y2_Nw,Y2_Ns1,Y2_Ns2,Y2_Ow,Y2_NO3w,Y2_NO3s1,Y2_NO3s2,Y2_Pw,Y2_Ps1,Y2_Ps2,Y2_a,Y2_b,Y2_mw,Y2_ms,YY2_ONw,YY2_Nw,YY2_NO3w,YY2_Pw,YY2_mw

!Concentrations  
Real ONw(0:up),ONsf(0:up),ONss(0:up),Nw(0:up),Ns1(0:up),Ns2(0:up),Ow(0:up)
Real NO3w(0:up),NO3s1(0:up),NO3s2(0:up),L1(0:up),Pw(0:up),Ps1(0:up),Ps2(0:up),a(0:up), b(0:up), mw(0:up),ms(0:up)  
Real Nset(0:up),Nvol(0:up),NO3den(0:up),Pwset(0:up),mwset(0:up),MNw(0:up),Pdif(0:up),Pmin(0:up),Ndif(0:up),NO3dif(0:up)

!Ground water parameters                                                               
Real gw_NO3w,gw_NO3s1,gw_NO3s2,gw_Nw,gw_NS1,gw_Ns2                                     
!Fixed or initial parameters
Real dt,ronn,rond,roc,apn,sims,w
Real fNw,fNs1,fNs2,fNO3w,fNO3s1, fNO3s2
!Generated or calculated  parameters
Real Kminw ,Kga0, Kgb0 ,Kns,rows,Kden,vels_o,vels_s,velb,velr_o,velr_s, Kga(0:up),Kgb(0:up)
Real S,ana,Kda,Kdb,frap,Kim,Kmin1s,Kmin2s,fslw,Knw,kv,TN, SumV 
Real SumR0,Sigma,lamda,Tsr1,R(0:up), ro,Coef0,Coef1, L2fix
Real KgaTIa,KgbTIb,pi,fN,C1,C2,pK,pH
Real alpha1,alpha2,Kd, ip(0:up), Qa(0:up), Qn(0:up), epslon, Sumkga,Sumkgb
Real lat, ksa, ksb, Ran1, Ran2, Ran3, fw, Mu_T, Temp1, Temp2
Real Vps, kw, Kstar, Kp1,Fds,Fds1,Fss1, Fss, Fdw, Fsw, apa,Sp,  p, Kp2, Ks1, Ks2, gw_Pw, P_w, Ps_1, Ps_2, gw_Ps1, gw_Ps2, Dpw
Real theta,por,Is,beta0,beta1,beta2,fNup,SOD, Kda_fixed, Kdb_fixed
Real rchl,Ss,omega,L2,d_bound,Dstar,DOw,Daw,Dnw,tor,ko,Sw,Osat,Kea,Keb,Kep,Qg_star

!¥¥¥¥¥¥  Aµir  ¥¥¥¥¥¥
Real DOCw(0:up),LPOCw(0:up),RPOCw(0:up), TOCw(0:up),DOCs1(0:up),LPOCs1(0:up),RPOCs1(0:up),DOCs2(0:up),LPOCs2(0:up),RPOCs2(0:up),Os1(0:up),CH4w(0:up),CH4s1(0:up),CH4s2(0:up),CH4gw(0:up)
Real SumVwLPOCw, SumVwRPOCw, SumVwDOCw,SumVwTOCw, SumVs1LPOCs1, SumVs1RPOCs1, SumVs1DOCs1,SumVs2LPOCs2, SumVs2RPOCs2,  SumVs2DOCs2 , Sumf1, Sumf2
Real sumVwCH4w,sumVs1CH4s1,sumVs2CH4s2
Real ZDOCw(0:yrup),ZLPOCw(0:yrup),ZRPOCw(0:yrup),ZTOCw(0:yrup),ZDOCs1(0:yrup),ZLPOCs1(0:yrup),ZRPOCs1(0:yrup),ZDOCs2(0:yrup), ZLPOCs2(0:yrup)
Real ZCH4w(0:yrup),ZCH4s1(0:yrup),ZCH4s2(0:yrup) 
Real ZRPOCs2(0:yrup),ZOs1(0:yrup), ZVw(0:yrup),Zf1(0:yrup),Zf2(0:yrup),ZVs1(0:yrup),ZVs2(0:yrup),ZVs(0:yrup)

Real LPOCset(0:up), RPOCset(0:up),Coutw(0:up),DOCdif(0:up),DOCden(0:up),DOCresp(0:up),DOCmeth(0:up),PlantC(0:up) ,methane(0:up)!for mass balance
Real SumLPOCset , SumRPOCset ,SumCoutw ,SumDOCdif ,SumDOCden ,SumDOCresp,SumDOCmeth ,SumPlantC , summethane !for mass balance
Real ZLPOCset(0:yrup) , ZRPOCset(0:yrup) ,ZCoutw(0:yrup) ,ZDOCdif(0:yrup) ,ZDOCden(0:yrup) ,ZDOCresp(0:yrup),ZDOCmeth(0:yrup),ZPlantC(0:yrup) , Zmethane(0:up)  !for mass balance
!REAL Y12_LPOCset,Y12_RPOCset,Y12_Coutw,Y12_DOCdif ,Y12_DOCresp,Y12_DOCden ,Y12_DOCmeth, YY12_LPOCw, YY12_RPOCw, YY12_DOCw , Y12_PlantC , Y12_methane
REAL DOClast,LPOClast,RPOClast,DOClastY1,LPOClastY1,RPOClastY1

!Real Y1_DOCw ,Y1_LPOCw,Y1_RPOCw, Y1_TOCw,Y1_DOCs1,Y1_LPOCs1,Y1_RPOCs1,Y1_DOCs2,Y1_LPOCs2,Y1_RPOCs2 ,Y1_CH4w ,Y1_CH4s1 ,Y1_CH4s2
!Real Y2_DOCw ,Y2_LPOCw,Y2_RPOCw, Y2_TOCw,Y2_DOCs1,Y2_LPOCs1,Y2_RPOCs1,Y2_DOCs2,Y2_LPOCs2,Y2_RPOCs2 ,Y2_CH4w ,Y2_CH4s1 ,Y2_CH4s2
!REAL Y1_LPOCset,Y1_RPOCset,Y1_Coutw,Y1_DOCdif ,Y1_DOCresp,Y1_DOCden ,Y1_DOCmeth,  Y1_PlantC , Y1_methane
!REAL Y2_LPOCset,Y2_RPOCset,Y2_Coutw,Y2_DOCdif ,Y2_DOCresp,Y2_DOCden ,Y2_DOCmeth,  Y2_PlantC , Y2_methane
!REAL  YY1_LPOCw , YY1_RPOCw, YY1_DOCw ,YY2_LPOCw , YY2_RPOCw,  YY2_DOCw 
REAL CH4diff , Ch4plantS1, Ch4plantS2

!Time dependent parameters
Real ONin(0:up),Nwin(0:up),Ng(0:up),D(0:up),NO3in(0:up),mwin(0:up)
Real Pin(0:up),Pg(0:up),NO3g(0:up),Vs(0:up),owin(0:up)                      !phosphorus variables
Real Vw(0:up), Vs1(0:up),Vs2(0:up), Vb(0:up),Vbw(0:up),Nair(0:up),NO3air(0:up)
Real Qin(0:up),Qout(0:up),ET(0:up),Qg(0:up),Area(0:up),H(0:up),H1(0:up),H2(0:up),temp(0:up),Uw(0:up)
Real DOCin(0:up),LPOCin(0:up),RPOCin(0:up),DOCg(0:up), TOCin(0:up),DOCatm(0:up),TOCgw(0:up)
Real aca,FaDOC,FbDOC,kLPOc,kRPOC,KsatO,K1DOC,KinO,KN,k2DOC,BDOC1,k3DOC,FaLPOC,FbLPOC,BDOC0
Real vsLPOC,vrLPOC,FaRPOC,FbRPOC,BDOC2,KinN,k4DOC,vsRPOC,vrRPOC, cp1, cp2, cp3,vbLPOC,vbRPOC,vbDOC
Real Dcw , f1, f2 ,amc
Real kLPOC_fixed , kRPOC_fixed, KsatO_fixed, KinO_fixed,KN_fixed, KinN_fixed,K1DOC_fixed, k2DOC_fixed,k3DOC_fixed,k4DOC_fixed
Real fbw , fbs
Real Beta1cw, Beta1Ch4w	! for mass balance
Real Cair, Dch4w, k1CH4, k2CH4, ScCH4,kHCH4, JCH4 ,Spp , PPs1 , PPs2 , Ebuls1 , Ebuls2 , Lamdar, Rveg, DCh4air , Jebuls1, Jebuls2
Real k1CH4_fixed   , k2CH4_fixed 

INTEGER InitialCTest   !for testing initial carbon concentrations
INTEGER subhourlyprint , simno !prints subhourly results of the first run
INTEGER dailyprint , simnom    !prints daily results of the first run
INTEGER Implicitmethod ! for running the model with Implicit method
INTEGER inputuncertainty ! for running the model with input uncertainty of DOC inflow
INTEGER printdailyresultsNP ! for printing N and P daily results
INTEGER printdailyresultsCarbon ! for printing C daily results
Real cpp1, cpp2, cpp3 ! same as cp1, cp2 and cp3

!Characters for opening text files
Character(100) fixedparams, initialconc, hydro_climparams, hydrologparams,timedepparms, generatedparms,alldateweek,dateofweek, input_control
Character (100) dateofday, fixparmscarbon, initialcarbon,timedepparmcarbon,generatedparmcarbon 

END MODULE parm
!**********************************************************************************************************************

Subroutine Yr1Yr2_part1

Use parm

Use Pprime


!**********************************************************************************************************************
		!First and second year average concentrations
!**********************************************************************************************************************
    
  
     if (int((i+1)*dt)<371) then        ! Yr 1 average concentration
        Y1_ONw   =Y1_ONw  +Vw(i)*ONw(i+1)       ;Y1_Ow    =Y1_Ow   +Vw(i)*Ow(i+1)        ;Y1_Ps2   =Y1_Ps2  +Vs2(i)*Ps2(i+1)      ;
        Y1_ONsf  =Y1_ONsf +Vs(i)*ONsf(i+1)      ;Y1_NO3w  =Y1_NO3w +Vw(i)*NO3w(i+1)      ;Y1_a     =Y1_a    +Vw(i)*a(i+1)         ;
        Y1_ONss  =Y1_ONss +Vs(i)*ONss(i+1)      ;Y1_NO3s1 =Y1_NO3s1+Vs1(i)*NO3s1(i+1)    ;Y1_b     =Y1_b    +Vw(i)*b(i+1)         ;
        Y1_Nw    =Y1_Nw   +Vw(i)*Nw(i+1)        ;Y1_NO3s2 =Y1_NO3s2+Vs2(i)*NO3s2(i+1)    ;Y1_mw    =Y1_mw   +Vw(i)*mw(i+1)        ;
        Y1_Ns1   =Y1_Ns1  +Vs1(i)*Ns1(i+1)      ;Y1_Pw    =Y1_Pw   +Vw(i)*Pw(i+1)        ;Y1_ms    =Y1_ms   +Vs(i)*ms(i+1)        ;
        Y1_Ns2   =Y1_Ns2  +Vs2(i)*Ns2(i+1)      ;Y1_Ps1   =Y1_Ps1  +Vs1(i)*Ps1(i+1)      ;
                                                 
        Sum1Vw = Sum1Vw + Vw(i)        
        Sum1Vs = Sum1Vs + Vs(i)        
        Sum1Vs1= Sum1Vs1+ Vs1(i)
        Sum1Vs2= Sum1Vs2+ Vs2(i)

        Y1_DOCw     = Y1_DOCw  + Vw(i)*DOCw(i+1)    ; Y1_DOCs1    = Y1_DOCs1 + Vs1(i)*DOCs1(i+1)     ;Y1_DOCs2    = Y1_DOCs2+ Vs2(i)*DOCs2(i+1)    
        Y1_LPOCw    = Y1_LPOCw + Vw(i)*LPOCw(i+1)   ; Y1_LPOCs1   = Y1_LPOCs1+ Vs1(i)*LPOCs1(i+1)    ;Y1_LPOCs2   = Y1_LPOCs2+ Vs2(i)*LPOCs2(i+1) 
        Y1_RPOCw    = Y1_RPOCw + Vw(i)*RPOCw(i+1)   ; Y1_RPOCs1   = Y1_RPOCs1+ Vs1(i)*RPOCs1(i+1)    ;Y1_RPOCs2   = Y1_RPOCs2+ Vs2(i)*RPOCs2(i+1)  
        Y1_Ch4w     = Y1_CH4w  + Vw(i)*CH4w(i+1)    ; Y1_Ch4s1    = Y1_CH4s1  + Vs1(i)*CH4s1(i+1)   ;Y1_Ch4s2    = Y1_CH4s2  + Vs2(i)*CH4s2(i+1)     
        Y1_TOCw     = Y1_TOCw  + Vw(i)*TOCw(i+1)    ; 
        
        else        ! Yr 2 average concentration
        Sum2Vw = Sum2Vw + Vw(i)
        Sum2Vs = Sum2Vs + Vs(i)
        Sum2Vs1= Sum2Vs1+ Vs1(i)
        Sum2Vs2= Sum2Vs2+ Vs2(i)
    
        Y2_ONw   =Y2_ONw  +Vw(i)*ONw(i+1)       ;Y2_NO3w  =Y2_NO3w +Vw(i)*NO3w(i+1)      ;Y2_b     =Y2_b    +Vw(i)*b(i+1)         ;
        Y2_ONsf  =Y2_ONsf +Vs(i)*ONsf(i+1)      ;Y2_NO3s1 =Y2_NO3s1+Vs1(i)*NO3s1(i+1)    ;Y2_mw    =Y2_mw   +Vw(i)*mw(i+1)        ;
        Y2_ONss  =Y2_ONss +Vs(i)*ONss(i+1)      ;Y2_NO3s2 =Y2_NO3s2+Vs2(i)*NO3s2(i+1)    ;Y2_ms    =Y2_ms   +Vs(i)*ms(i+1)
        Y2_Nw    =Y2_Nw   +Vw(i)*Nw(i+1)        ;Y2_Pw    =Y2_Pw   +Vw(i)*Pw(i+1)        ;
        Y2_Ns1   =Y2_Ns1  +Vs1(i)*Ns1(i+1)      ;Y2_Ps1   =Y2_Ps1  +Vs1(i)*Ps1(i+1)      ;
        Y2_Ns2   =Y2_Ns2  +Vs2(i)*Ns2(i+1)      ;Y2_Ps2   =Y2_Ps2  +Vs2(i)*Ps2(i+1)      ;
        Y2_Ow    =Y2_Ow   +Vw(i)*Ow(i+1)        ;Y2_a     =Y2_a    +Vw(i)*a(i+1)         ;
                                                 
        Y2_DOCw     = Y2_DOCw  + Vw(i)*DOCw(i+1)    ;Y2_DOCs1    = Y2_DOCs1 + Vs1(i)*DOCs1(i+1)  ;Y2_DOCs2    = Y2_DOCs2+ Vs2(i)*DOCs2(i+1)   ;
        Y2_LPOCw    = Y2_LPOCw + Vw(i)*LPOCw(i+1)   ;Y2_LPOCs1   = Y2_LPOCs1+ Vs1(i)*LPOCs1(i+1) ;Y2_LPOCs2   = Y2_LPOCs2+ Vs2(i)*LPOCs2(i+1) ;
        Y2_RPOCw    = Y2_RPOCw + Vw(i)*RPOCw(i+1)   ;Y2_RPOCs1   = Y2_RPOCs1+ Vs1(i)*RPOCs1(i+1) ;Y2_RPOCs2   = Y2_RPOCs2+ Vs2(i)*RPOCs2(i+1) ;
        Y2_Ch4w     = Y2_CH4w  + Vw(i)*CH4w(i+1)    ;Y2_Ch4s1  = Y2_CH4s1  + Vs1(i)*CH4s1(i+1)  ;Y2_Ch4s2    = Y2_CH4s2  + Vs2(i)*CH4s2(i+1)     
        Y2_TOCw     = Y2_TOCw  + Vw(i)*TOCw(i+1)    ;
        
         END IF
           
           
 end subroutine
!------------------------------------------------------------------------------------------------------------
 
 
 
 
!------------------------------------------------------------------------------------------------------------
 Subroutine Y1Y2_part2
 Use parm
 Use pprime
 
 ! 2 yr average Mass balance еееееееееееееееееее jUST FOR Carbonееееееееееееееееееееееееееееее
            
           Y12_LPOCset = Y12_LPOCset  +  ZLPOCset(zi)
           Y12_RPOCset = Y12_RPOCset  +  ZRPOCset(zi)
           Y12_Coutw   = Y12_Coutw    +  ZCoutw(zi)  
           Y12_DOCdif  = Y12_DOCdif   +  ZDOCdif(zi) 
           Y12_DOCresp = Y12_DOCresp  +  ZDOCresp(zi)
           Y12_DOCden  = Y12_DOCden   +  ZDOCden(zi) 
           Y12_DOCmeth = Y12_DOCmeth  +  ZDOCmeth(zi)
           Y12_PlantC   = Y12_plantC    +  ZplantC(zi)
           Y12_methane = Y12_methane + Zmethane(zi) 
           
           YY12_LPOCw  = YY12_LPOCw +ZLPOCw(zi)*Qout(i+1)/1e6 !Mass of LPOC lost to outflow 
           YY12_RPOCw  = YY12_RPOCw +ZRPOCw(zi)*Qout(i+1)/1e6 !Mass of RPOC lost to outflow 
           YY12_DOCw   = YY12_DOCw  +ZDOCw(zi) *Qout(i+1)/1e6 !Mass of DPOC lost to outflow 
           
           DOClast     = ZDOCw(n)
           LPOClast    = ZLPOCw(n)
           RPOClast    = ZRPOCw(n)

! Y1 and Y2 mass balance averages    
     if (int((i+1)*dt)<371) then
            Y1_Nset   =Y1_Nset+ZNset(zi)
            Y1_Nvol   =Y1_Nvol+ZNvol(zi)
            Y1_NO3den =Y1_NO3den+ZNO3den(zi)
            Y1_Pwset  =Y1_Pwset+ZPwset(zi)
            Y1_mwset  =Y1_mwset+Zmwset(zi)
            Y1_MNw    =Y1_MNw+ZMNw(zi)
            Y1_Pdif   =Y1_Pdif+ZPdif(zi)
            Y1_Pmin   =Y1_Pmin+ZPmin(zi)
            Y1_Ndif   =Y1_Ndif+ZNdif(zi)
            Y1_NO3dif =Y1_NO3dif+ZNO3dif(zi)
!**********************************************************************    
            YY1_ONw   = YY1_ONw +ZOnw(zi)*Qout(i+1)/1e6     !Mass lost to outflow 
            YY1_Nw    = YY1_Nw +ZNw(zi)*Qout(i+1)/1e6       !Mass lost to outflow 
            YY1_NO3w  = YY1_NO3w+ZNO3w(zi)*Qout(i+1)/1e6    !Mass lost to outflow 
            YY1_Pw    = YY1_Pw +ZPw(zi)*Qout(i+1)/1e6       !Mass lost to outflow 
            YY1_mw    = YY1_mw + Zmw(zi)*Qout(i+1)/1e6      !Mass lost to outflow 
                                                             
            Y1_LPOCset = Y1_LPOCset  +  ZLPOCset(zi)
            Y1_RPOCset = Y1_RPOCset  +  ZRPOCset(zi)
            Y1_Coutw   = Y1_Coutw    +  ZCoutw(zi)  
            Y1_DOCdif  = Y1_DOCdif   +  ZDOCdif(zi) 
            Y1_DOCresp = Y1_DOCresp  +  ZDOCresp(zi)
            Y1_DOCden  = Y1_DOCden   +  ZDOCden(zi) 
            Y1_DOCmeth = Y1_DOCmeth  +  ZDOCmeth(zi)
            Y1_PlantC   = Y1_plantC  +  ZplantC(zi)
            Y1_methane = Y1_methane  + Zmethane(zi) 
            YY1_LPOCw  = YY1_LPOCw +ZLPOCw(zi)*Qout(i+1)/1e6 !Mass of LPOC lost to outflow 
            YY1_RPOCw  = YY1_RPOCw +ZRPOCw(zi)*Qout(i+1)/1e6 !Mass of RPOC lost to outflow 
            YY1_DOCw   = YY1_DOCw  +ZDOCw(zi) *Qout(i+1)/1e6 !Mass of DPOC lost to outflow  
            DOClastY1     = ZDOCw(371)
            LPOClastY1    = ZLPOCw(371)
            RPOClastY1    = ZRPOCw(371)
             

     else  !Yr 2 mass balance
!SInew*****************************************************************
            Y2_Nset   = Y2_Nset+ZNset(zi)
            Y2_Nvol   = Y2_Nvol+ZNvol(zi)
            Y2_NO3den = Y2_NO3den+ZNO3den(zi)
            Y2_Pwset  = Y2_Pwset+ZPwset(zi)
            Y2_mwset  = Y2_mwset+Zmwset(zi)
            Y2_MNw    = Y2_MNw+ZMNw(zi)
            Y2_Pdif   = Y2_Pdif+ZPdif(zi)
            Y2_Pmin   = Y2_Pmin+ZPmin(zi)
            Y2_Ndif   = Y2_Ndif+ZNdif(zi)
            Y2_NO3dif = Y2_NO3dif+ZNO3dif(zi)
!**********************************************************************    
            YY2_ONw   = YY2_ONw +ZOnw(zi)*Qout(i+1)/1e6
            YY2_Nw    = YY2_Nw +ZNw(zi)*Qout(i+1)/1e6
            YY2_NO3w  = YY2_NO3w+ZNO3w(zi)*Qout(i+1)/1e6
            YY2_Pw    = YY2_Pw +ZPw(zi)*Qout(i+1)/1e6
            YY2_mw    = YY2_mw + Zmw(zi)*Qout(i+1)/1e6


           Y2_LPOCset = Y2_LPOCset  +  ZLPOCset(zi)
           Y2_RPOCset = Y2_RPOCset  +  ZRPOCset(zi)
           Y2_Coutw   = Y2_Coutw    +  ZCoutw(zi)  
           Y2_DOCdif  = Y2_DOCdif   +  ZDOCdif(zi) 
           Y2_DOCresp = Y2_DOCresp  +  ZDOCresp(zi)
           Y2_DOCden  = Y2_DOCden   +  ZDOCden(zi) 
           Y2_DOCmeth = Y2_DOCmeth  +  ZDOCmeth(zi)
           Y2_PlantC   = Y2_plantC    +  ZplantC(zi)
           Y2_methane = Y2_methane + Zmethane(zi) 
           YY2_LPOCw  = YY2_LPOCw +ZLPOCw(zi)*Qout(i+1)/1e6 !Mass of LPOC lost to outflow 
           YY2_RPOCw  = YY2_RPOCw +ZRPOCw(zi)*Qout(i+1)/1e6 !Mass of RPOC lost to outflow 
           YY2_DOCw   = YY2_DOCw  +ZDOCw(zi) *Qout(i+1)/1e6 !Mass of DPOC lost to outflow 

        
     END IF 
     
     
End Subroutine
!------------------------------------------------------- ---------------- ---------------- ---------------- 

   
   
!----------------------------------------------------------- ---------------- ---------------- ----------------   
Subroutine Yr1Yr2_part3

Use parm
Use Pprime


!**********************************************************************************************************************
        Y1_ONw   =1e6*Y1_ONw  /Sum1Vw   ;   Y2_ONw   =1e6*Y2_ONw  /Sum2Vw       ;    Y1_DOCw  = Y1_DOCw  *1e6/sum1Vw        ;    Y2_DOCw  = Y2_DOCw  *1e6/sum1Vw
        Y1_ONsf  =1e6*Y1_ONsf /Sum1Vs   ;   Y2_ONsf  =1e6*Y2_ONsf /Sum2Vs       ;    Y1_LPOCw = Y1_LPOCw *1e6/sum1Vw        ;    Y2_LPOCw = Y2_LPOCw *1e6/sum1Vw
        Y1_ONss  =1e6*Y1_ONss /Sum1Vs   ;   Y2_ONss  =1e6*Y2_ONss /Sum2Vs       ;    Y1_RPOCw = Y1_RPOCw *1e6/sum1Vw        ;    Y2_RPOCw = Y2_RPOCw *1e6/sum1Vw
        Y1_Nw    =1e6*Y1_Nw   /Sum1Vw   ;   Y2_Nw    =1e6*Y2_Nw   /Sum2Vw       ;    Y1_TOCw  = Y1_TOCw  *1e6/sum1Vw        ;    Y2_TOCw  = Y2_TOCw  *1e6/sum1Vw
        Y1_Ns1   =1e6*Y1_Ns1  /Sum1Vs1  ;   Y2_Ns1   =1e6*Y2_Ns1  /Sum2Vs1      ;    Y1_DOCs1 = Y1_DOCs1 *1e6/sum1Vs1       ;    Y2_DOCs1 = Y2_DOCs1 *1e6/sum1Vs1
        Y1_Ns2   =1e6*Y1_Ns2  /Sum1Vs2  ;   Y2_Ns2   =1e6*Y2_Ns2  /Sum2Vs2      ;    Y1_LPOCs1= Y1_LPOCs1 *1e6/sum1Vs1      ;    Y2_LPOCs1= Y2_LPOCs1 *1e6/sum1Vs1
        Y1_Ow    =1e6*Y1_Ow   /Sum1Vw   ;   Y2_Ow    =1e6*Y2_Ow   /Sum2Vw       ;    Y1_RPOCs1= Y1_RPOCs1*1e6/sum1Vs1       ;    Y2_RPOCs1= Y2_RPOCs1*1e6/sum1Vs1
        Y1_NO3w  =1e6*Y1_NO3w /Sum1Vw   ;   Y2_NO3w  =1e6*Y2_NO3w /Sum2Vw       ;    Y1_DOCs2 = Y1_DOCs2 *1e6/sum1Vs2       ;    Y2_DOCs2 = Y2_DOCs2 *1e6/sum1Vs2
        Y1_NO3s1 =1e6*Y1_NO3s1/Sum1Vs1  ;   Y2_NO3s1 =1e6*Y2_NO3s1/Sum2Vs1      ;    Y1_LPOCs2= Y1_LPOCs2*1e6/sum1Vs2       ;    Y2_LPOCs2= Y2_LPOCs2*1e6/sum1Vs2
        Y1_NO3s2 =1e6*Y1_NO3s2/Sum1Vs2  ;   Y2_NO3s2 =1e6*Y2_NO3s2/Sum2Vs2      ;    Y1_RPOCs2= Y1_RPOCs2*1e6/sum1Vs2       ;    Y2_RPOCs2= Y2_RPOCs2*1e6/sum1Vs2
        Y1_Pw    =1e6*Y1_Pw   /Sum1Vw   ;   Y2_Pw    =1e6*Y2_Pw   /Sum2Vw       ;    Y1_CH4w  = Y1_CH4w  *1e6/sum1Vw        ;    Y1_CH4w  = Y1_CH4w  *1e6/sum2Vw        ;
        Y1_Ps1   =1e6*Y1_Ps1  /Sum1Vs1  ;   Y2_Ps1   =1e6*Y2_Ps1  /Sum2Vs1      ;    Y1_CH4s1 = Y1_CH4s1 *1e6/sum1Vs1       ;    Y2_CH4s1 = Y2_CH4s1 *1e6/sum2Vs1       ;
        Y1_Ps2   =1e6*Y1_Ps2  /Sum1Vs2  ;   Y2_Ps2   =1e6*Y2_Ps2  /Sum2Vs2      ;    Y1_CH4s2 = Y1_CH4s2 *1e6/sum1Vs2       ;    Y2_CH4s2 = Y2_CH4s2 *1e6/sum2Vs2       ;
        Y1_a     =Y1_a    /Sum1Vw       ;   Y2_a     =Y2_a    /Sum2Vw           ;
        Y1_b     =Y1_b    /Sum1Vw       ;   Y2_b     =Y2_b    /Sum2Vw           ;
        Y1_mw    =1e6*Y1_mw   /Sum1Vw   ;   Y2_mw    =1e6*Y2_mw   /Sum2Vw       ;
        Y1_ms    =Y1_ms   /Sum1Vs       ;   Y2_ms    =Y2_ms   /Sum2Vs           ;

!**************************************************************************************

End subroutine

     
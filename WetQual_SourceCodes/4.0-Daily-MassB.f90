!**********************************************************************************************************************
! This subroutine calculates daily mass balances of the outputs.
!**********************************************************************************************************************

!**********************************************************************************************************************
SUBROUTINE Massbalance
!**********************************************************************************************************************

USe parm; use pprime

!**********************************************************************************************************************
!          MassBalance Equations
!**********************************************************************************************************************
           ! Nirogen
           Nset(i+1)=porw*vels_o*Area(i)*ONw(i)-porw*velr_o*Area(i)*(ONsf(i)+ONss(i))
           Nvol(i+1)=porw*kv*Area(i)*(1-fN)*Nw(i)
           NO3den(i+1)=por*Vs2(i)*kden*NO3s2(i)
           Ndif(i+1)= Daw*beta1*Area(i)*(Nw(i)-Ns1(i))
           NO3dif(i+1)=Dnw*beta1*Area(i)*(NO3w(i)-NO3s1(i))
           MNw(i+1)=(Nw(i+1)+ONw(i+1)+NO3w(i+1))*Qout(i+1)           
          
           !Sediment
           mwset(i+1)=porw*vels_s*Area(i)*mw(i)-velr_s*Area(i)*ms(i)+velb*area(i)*ms(i)
           
           !phosphorus
           Pwset(i+1)=porw*vels_s*Area(i)*Fsw*mw(i)*Pw(i)-(1/(L1(i)+L2))*Area(i)*porw*velr_s*ms(i)*(L1(i)*Fss*Ps1(i)+L2*Fss1*Ps2(i)) 
           Pdif(i+1)=Dpw*beta1*Area(i)*(Fdw*Pw(i)-Fds*Ps1(i))
           Pmin(i+1)=Vw(i)*apn*kminw*Onw(i)

!еееееееееееее  A╡ir  Carbon Mass Balance ееееееееееееееееееееееее
           LPOCset(i+1)=porw*vels_o*Area(i)*LPOCw(i)-(1/(L1(i)+L2))*Area(i)*porw*velr_o*(L1(i)*LPOCs1(i)+L2*LPOCs2(i))! OC lost to settling
           RPOCset(i+1)=porw*vels_o*Area(i)*RPOCw(i)-(1/(L1(i)+L2))*Area(i)*porw*velr_o*(L1(i)*RPOCs1(i)+L2*RPOCs2(i))! OC lost to settling
           Coutw(i+1)=(LPOCw(i+1)+RPOCw(i+1)+DOCw(i+1))*Qout(i+1)                                ! OC lost to outflow
           DOCdif(i+1) =Dcw*beta1*Area(i)*(DOCw(i)-DOCs1(i))                                     ! OC lost to diffusion
           DOCresp(i+1)=(porw*Vw(i)*DOCw(i)+por*Vs1(i)*DOCs1(i))*Ow(i)/(Ow(i)+KsatO)*k1DOC       !DOC lost to aerobic respiration
           DOCden(i+1) =por*Vs2(i)*NO3s2(i)/(NO3s2(i)+KN)*k2DOC*DOCs2(i)                         !DOC lost to denitrification
           DOCmeth(i+1)=por*Vs2(i)*KinN/(NO3s2(i)+KinN)*k4DOC*DOCs2(i)                           !DOC lost to methanogenesis 
           PlantC (i+1)= aca*kda*a(i)+aca*kdb*b(i)                                               ! OC made by plant death in water
           methane (i+1)=  Ch4diff+Ch4plants1+Ch4plants2+Jebuls1+Jebuls2                         ! Methane emitted to atmosphere   

!**********************************************************************************************************************
END SUBROUTINE massbalance
!**********************************************************************************************************************

!**********************************************************************************************************************
SUBROUTINE Dailycalcs
!**********************************************************************************************************************
Use parm
Use Pprime
!**********************************************************************************************************************
      SumVwONw   = SumVwONw+Vw(i)*ONw(i+1)	 ;    SumVwONss  = SumVwONss+Vs(i)*ONss(i+1)    
      SumVwNw    = SumVwNw+Vw(i)*Nw(i+1)     ;    SumVwONsf  = SumVwONsf+Vs(i)*ONsf(i+1)    
      SumVwNO3w  = SumVwNO3w+Vw(i)*NO3w(i+1) ;    SumVwNs1   = SumVwNs1+Vs1(i)*Ns1(i+1)     
      SumVwOw    = SumVwOw+Vw(i)*Ow(i+1)     ;    SumVwNs2   = SumVwNs2+Vs2(i)*Ns2(i+1)     
      SumVwPw    = SumVwPw+Vw(i)*Pw(i+1)     ;    SumVwNO3s1 = SumVwNO3s1+Vs1(i)*NO3s1(i+1)
      SumVwmw    = SumVwmw+Vw(i)*mw(i+1)     ;    SumVwNO3s2 = SumVwNO3s2+Vs2(i)*NO3s2(i+1)
      SumVw      = SumVw + Vw(i)             ;    SumVwa     = SumVwa+Vw(i)*a(i+1)          
      SumVs      = SumVs + Vs(i)             ;    SumVwb     = SumVwb+Vw(i)*b(i+1)          
      SumVs1     = SumVs1+ Vs1(i)            ;    SumVwPs1   = SumVwPs1+Vs1(i)*Ps1(i+1)     
      SumVs2     = SumVs2+ Vs2(i)            ;    SumVwPs2   = SumVwPs2+Vs2(i)*Ps2(i+1)     
      SumVwms    = SumVwms+Vs(i)*ms(i+1)        
      
      !Mass balance  
      SumVwNset  = SumVwNset+Nset(i+1)       ;   SumVwPdif =SumVwPdif+Pdif(i+1)
      SumVwNvol  = SumVwNvol+Nvol(i+1)       ;   SumVwPmin =SumVwPmin+Pmin(i+1)
      SumVwNO3den= SumVwNO3den+NO3den(i+1)   ;   SumVwNdif =SumVwNdif+Ndif(i+1)
      SumVwPwset = SumVwPwset+Pwset(i+1)     ;   SumVwNO3dif =SumVwNO3dif+NO3dif(i+1)
      SumVwmwset = SumVwmwset+mwset(i+1)     ;   TotMNw     = TotMNw+MNw(i+1)  
             
    
 	  !еееееееееееее  A╡ir  ееееееееееееееееееееееее
	  SumVwLPOCw=SumVwLPOCw+Vw(i)*LPOCw(i+1)    ;    SumVs1LPOCs1=SumVs1LPOCs1+Vs1(i)*LPOCs1(i+1) ;SumVs2LPOCs2=SumVs2LPOCs2+Vs2(i)*LPOCs2(i+1)
	  SumVwRPOCw=SumVwRPOCw+Vw(i)*RPOCw(i+1)    ;    SumVs1RPOCs1=SumVs1RPOCs1+Vs1(i)*RPOCs1(i+1) ;SumVs2RPOCs2=SumVs2RPOCs2+Vs2(i)*RPOCs2(i+1)
	  SumVwDOCw=SumVwDOCw+Vw(i)*DOCw(i+1)       ;    SumVs1DOCs1=SumVs1DOCs1+Vs1(i)*DOCs1(i+1)    ;SumVs2DOCs2=SumVs2DOCs2+Vs2(i)*DOCs2(i+1)	 
	  SumVwCH4w=SumVwCH4w+Vw(i)*CH4w(i+1)       ;    SumVs1CH4s1=SumVs1CH4s1+Vs1(i)*Ch4s1(i+1)    ;SumVs2CH4s2=SumVs2CH4s2+Vs2(i)*Ch4s2(i+1)
	  SumVwTOCw=SumVwTOCw+Vw(i)*TOCw(i+1)       ;
      
       !mass balance                                  !parameters    
      SumLPOCset =  LPOCset(i+1)+ SumLPOCset    ;    sumf1=sumf1+f1
      SumRPOCset =  RPOCset(i+1)+ SumRPOCset    ;    sumf2=sumf2+f2
      SumCoutw   =  Coutw(i+1)  + SumCoutw      ;
      SumDOCdif  =  DOCdif(i+1) + SumDOCdif     ;
      SumDOCresp =  DOCresp(i+1)+ SumDOCresp    ;
      SumDOCden  =  DOCden(i+1) + SumDOCden 
      SumDOCmeth =  DOCmeth(i+1)+ SumDOCmeth
      SumplantC= PlantC(i+1) +sumplantC
      summethane=methane(i+1)+summethane
 
     
!**********************************************************************************************************************
!****                         load results to each individual variable  (make daily results)                                        *******
!**********************************************************************************************************************
           if(int((i+1)*dt+dt/2)==(i+1)*dt) Then
           zi=int((i+1)*dt+dt/2)                         
!**********************************************************************************************************************
            
            ZOnw(zi)=(SumVwONw/SumVw)*1e6            ;ZNset(zi)=(SumVwNset*dt)                 ;
            ZOnss(zi)=(SumVwONss/SumVs)*1e6          ;ZNvol(zi)=(SumVwNvol*dt)                 ;
            ZOnsf(zi)=(SumVwONsf/SumVs)*1e6          ;ZNO3den(zi)=(SumVwNO3den*dt)             ;
            ZNw(zi) =(SumVwNw/SumVw)*1e6             ;ZPwset(zi)=(SumVwPwset*dt)               ;
            ZNs1(zi) =(SumVwNs1/SumVs1)*1e6          ;Zmwset(zi)=(SumVwmwset*dt)               ;   
            ZNs2(zi) =(SumVwNs2/SumVs2)*1e6          ;ZMNw(zi)=TotMNw*dt                       ;
            ZNO3w(zi)=(SumVwNO3w/SumVw)*1e6          ;ZPdif(zi)=SumVwPdif*dt                   ;
            ZNO3s1(zi)=(SumVwNO3s1/SumVs1)*1e6       ;ZPmin(zi)=SumVwPmin*dt                   ; 
            ZNO3s2(zi)=(SumVwNO3s2/SumVs2)*1e6       ;ZNdif(zi)=SumVwNdif*dt                   ;
            Za(zi) =(SumVwa/SumVw)                   ;ZNO3dif(zi)=SumVwNO3dif*dt               ;
            Zb(zi) =(SumVwb/SumVw)                   
            ZOw(zi) =(SumVwOw/SumVw)*1e6            
            ZPw(zi) =(SumVwPw/SumVw)*1e6
            ZPs1(zi) =(SumVwPs1/SumVs1)*1e6
            ZPs2(zi) =(SumVwPs2/SumVs2)*1e6
            Zmw(zi) =(SumVwmw/SumVw)*1e6
            Zms(zi) =(SumVwms/SumVs)
            
            !ееееееее   A╡ir   еееееееееееее       Mass balance        еееееееееееееее     Other       ееееееееееееееееееее  
            ZLPOCw(zi)=SumVwLPOCw/SumVw*1e6        ; ZLPOCset(zi)=SumLPOCset  *dt  ;  ZVw(zi)=SumVw*dt/1e6	    !m3        
			ZRPOCw(zi)=SumVwRPOCw/SumVw*1e6        ; ZRPOCset(zi)=SumRPOCset  *dt  ;  Zf1(zi)=Sumf1*dt                                                 
			ZDOCw(zi)=SumVwDOCw/SumVw*1e6          ; ZCoutw(zi)  =SumCoutw    *dt  ;  Zf2(zi)=Sumf2*dt                      
			ZTOCw(zi)=SumVwTOCw/SumVw*1e6          ; ZDOCdif(zi) =SumDOCdif   *dt  ;  ZVs1(zi)=sumVs1*dt/1e6	!m3 
			ZCH4w(zi)=SumVwCH4w/SumVw*1e6          ; ZDOCresp(zi)=SumDOCresp  *dt  ;  ZVs2(zi)=sumVs2*dt/1e6	!m3 
			ZLPOCs1(zi)=SumVs1LPOCs1/SumVs1*1e6    ; ZDOCden(zi) =SumDOCden   *dt  ;  ZVs(zi)=sumVs*dt/1e6	    !m3 
			ZRPOCs1(zi)=SumVs1RPOCs1/SumVs1*1e6    ; ZDOCmeth(zi)=SumDOCmeth  *dt                                  
			ZDOCs1(zi)=SumVs1DOCs1/SumVs1*1e6      ; ZplantC(zi) =sumplantC   *dt                   
			ZCH4s1(zi)=SumVs1CH4s1/SumVs1*1e6      ; Zmethane(zi)=summethane  !   dt was already applied earlier. The units were gram
			ZLPOCs2(zi)=SumVs2LPOCs2/SumVs2*1e6                                   	
			ZRPOCs2(zi)=SumVs2RPOCs2/SumVs2*1e6      
			ZDOCs2(zi)=SumVs2DOCs2/SumVs2*1e6      
			ZCH4s2(zi)=SumVs2CH4s2/SumVs2*1e6
			

           ! Call Y1Y2_part2
            Call Equal0_Part2
    END IF
!**********************************************************************************************************************
 
!**********************************************************************************************************************
END SUBROUTINE Dailycalcs
!**********************************************************************************************************************
  
!**********************************************************************************************************************
! This subroutine simulates carbon cycle and removal in wetlands.
!**********************************************************************************************************************

!**********************************************************************************************************************
SUBROUTINE Carbon
!**********************************************************************************************************************
Use Parm

Use Pprime
!**********************************************************************************************************************

            
!еееееееееееееееееееееееееееееееееееееееееееееееееееее  A╡ir  ееееееееееееееееееееееееееееееееееееееееееееееееееееееее

													!Carbon cycle!
!---------------------------------------------------------------------------------------------------------------------------------------
vsLPOC = vels_o	; vsRPOC = vels_o	                ! some Parameters are equall to 
vbDOC  = velb	; vbLPOC = velb		; vbRPOC=velb 	! other ones used in N-P part											
vrLPOC = velr_o	; vrRPOC = velr_o                   !    
!   klpoc=kmin1s ;       kRPOC=kmin2s something to think about
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


fbs=1-fbw   ! plant aboveground and belowground biomass portions
            ! Maynard et al (2011) estimated shoot:root ratio of 0.23+-0.15 (doi:10.5194/bg-8-3391-2011)
f1= L1(i)/(L1(i)+L2)		
f2=L2/(L1(i)+L2)

! Input uncertainty is turned on or off. if 0, no input uncertainty, otherwise there will be TOC inflow uncertainty
    IF (inputuncertainty==0) then
        cp1=cpp1 ; cp2=cpp2 ; cp3=cpp3
    End IF


! ----------------------------------------------EQUATIONS FOR CARBON CYCLING---------------------------------

!                                                       WATER COLUMN

	!LPOC in water column
    
   ! LPOCin(i)= TOCin(i)* cp1

	LPOCw(i+1)=(1/(porw*Vw(i+1)+dt*(porw*Vw(i)*kLPOC+Qout(i)+porw*vsLPOC*Area(i))))*(porw*Vw(i)*LPOCw(i)+(dt)*(Qin(i)*LPOCin(i)+aca*kda*FaLPOC*a(i)&
	&+aca*kdb*fbw*FbLPOC*b(i)+vrLPOC*Area(i)*LPOCs1(i)))

	!RPOC in water column
	
!	RPOCin(i)= TOCin(i)* cp2
	
	RPOCw(i+1)= (1/(porw*Vw(i+1)+dt*(porw*Vw(i+1)*kRPOC+Qout(i)+porw*vsRPOC*Area(i))))*(porw*Vw(i)*RPOCw(i)+dt*(Qin(i)*RPOCin(i)+aca*kda*FaRPOC*a(i)&
	&+aca*kdb*fbw*FbRPOC*b(i)+vrRPOC*Area(i)*RPOCs1(i)))
	
	! DOC in water column
!	DOCin(i)= TOCin(i)* cp3	
	
	if (Ow(i)>0) then
	    DOCw(i+1)=1/( porw*Vw(i+1)+dt*( porw*Vw(i)*Ow(i)/(Ow(i)+KsatO)*k1DOC  &
	    & +porw*Vw(i)*KinO/(Ow(i)+KinO)*NO3w(i)/(NO3w(i)+KN)*k2DOC*0          &  ! No dinitrification in aerobic water
	    & +Qout(i)-Qg(i)*Coef1+Dcw*beta1* Area(i)                             &
	    & +porw*Vw(i)* KinO/(Ow(i)+KinO)*KinN/(KinN+No3w(i))*k3DOC*0        ))&  ! No Methanogenesis in aerobic water                
	    & *(porw*DOCw(i)*Vw(i) + dt* (Qin(i)*DOCin(i) +ip(i)*Area(i)*DOCatm(i)+ aca*kda*FaDOC*a(i) +&
	    & aca*kdb*fbw*FbDOC*b(i)+ porw*Vw(i)*kLPOC*LPOCw(i)+porw*Vw(i)*kRPOC*RPOCw(i)+ Qg(i)*DOCs1(i)*Coef0+ Dcw*beta1*Area(i)*DOCs1(i)))
	ELSE
	    DOCw(i+1)=1/( porw*Vw(i+1)+dt*(porw*Vw(i)*NO3w(i)/(NO3w(i)+KN)*k2DOC + Qout(i) - Qg(i)*Coef1 + Dcw*beta0* Area(i) + &
	    & porw*Vw(i)*KinN/(KinN+No3w(i))*k3DOC))* (porw*DOCw(i)*Vw(i) + dt* (Qin(i)*DOCin(i) +ip(i)*Area(i)*DOCatm(i)+ aca*kda*FaDOC*a(i) +&
	    & aca*kdb*fbw*FbDOC*b(i)+ porw*Vw(i)*kLPOC*LPOCw(i)+porw*Vw(i)*kRPOC*RPOCw(i)+ Qg(i)*DOCs2(i)*Coef0+ Dcw*beta0*Area(i)*DOCs2(i)))
	end if
	
	
	! TOC in water column
	
	TOCw(i+1)=DOCw(i+1)+RPOCw(i+1)+LPOCw(i+1)
	
	!-------------------------------------------------------------------------------------------------------------------------
	! Methane in Water Column !July 2012
	Ch4diff=0
	
	if (Ow(i)>0 ) then
	    CH4w(i+1)= 1/(porw*Vw(i+1)+dt*(JCH4*porw*Area(i)+Dch4w*beta1*porw*Area(i)+Qout(i)+Qg(i)*coef1+porw*Vw(i)*Ow(i)/(Ow(i)+KsatO)*k1CH4+  &
	    & porw*Vw(i)*KinO/(Ow(i)*KinO)*NO3w(i)/(NO3w(i)+KN)*k2CH4)) * &
	    & ( porw*Vw(i)*CH4w(i)+dt*(JCH4*porw*Area(i)*Cair + Dch4w*beta1*porw*area(i)*CH4s1(i)+Qg(i)*CH4s1(i)*coef0))
	
	else
	    CH4w(i+1)= 1/(porw*Vw(i+1)+dt*(JCH4*porw*Area(i)+Dch4w*beta0*porw*Area(i)+Qout(i)+Qg(i)*coef1+  &
	    & porw*Vw(i)*KinO/(Ow(i)*KinO)*NO3w(i)/(NO3w(i)+KN)*k2CH4)) * &
	    & ( porw*Vw(i)*CH4w(i)+dt*(JCH4*porw*Area(i)*Cair + Dch4w*beta0*porw*area(i)*CH4s1(i)+Qg(i)*CH4s2(i)*coef0))
	end if
	
    !Mass balance for Ch4
    If (Ch4w(i+1)<0) then
        Ch4w(i+1)=1e-8
        Ch4diff=	0
    else
        Ch4diff=JCH4*porw*Area(i)*(CH4w(i+1)-Cair)*dt ! CH4 diffused to atmosphere (grams)
    end if
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
!                                                      AEROBIC SEDIMENT LAYER	

	! Oxygen avaliability ar Aerobic sediment layer
	Os1(i)=Ow(i)/2	
	
	!LPOC in Aerobic sediment layer
	
	if (Os1(i)>0) then
	    LPOCs1(i+1)= 1/( Vs1(i) + dt*(Vs1(i)*kLPOC + porw*f1*vrLPOC*Area(i)+vbLPOC*Area(i) )  )* &
	    & (  Vs1(i)*LPOCs1(i) + dt*(aca*kdb*f1*fbs*FbLPOC*b(i) + f1*porw*vsLPOC*Area(i)* LPOCw(i) ) )
	Else
	    LPOCs1(i+1)=0
	end if
	
	!RPOC in aerobic sediment layer
	
	if (Os1(i)>0) then
	    RPOCs1(i+1)= 1/ (Vs1(i) + dt* (  Vs1(i)*kRPOC+ porw*f1*vrRPOC*Area(i)+vbRPOC*Area(i))) *&
	    & (Vs1(i)*RPOCs1(i)+dt*(fbs*f1*aca*kdb*FbRPOc*b(i)+f1*porw*vsRPOC*Area(i)*RPOCw(i)))  
	Else
	    RPOCs1(i+1)=0
	end if
	
	! DOC in aeroic sediment layer
	
	if (Os1(i)>0) then
	    DOCs1(i+1)= 1 / ( por*Vs1(i) + dt*(Dcw*beta1*Area(i)+Dcw*beta2*Area(i)+Qg(i)*Coef0-Qg(i)*Coef1+&
	    & por*Vs1(i)*Os1(i)/(Os1(i)+KsatO)*k1DOC+ por*vbDOC*Area(i))) * & 
	    & ( por*Vs1(i)*DOCs1(i) + dt*(fbs*f1*aca*kdb*FbDOC*b(i)+Vs1(i)*kLPOC*LPOCs1(i)+ Vs1(i)*kRPOC*RPOCs1(i)+ &
	    & Dcw*beta1*Area(i)*DOCw(i)+ Dcw*beta2*Area(i)*DOCs2(i) + Qg(i)* DOCs2(i)* coef0 -Qg(i)*DOCw(i)*Coef1 ))
	else
	    DOCs1(i+1)= 0
	end if
	
	! Methane in Aerobic sediment layer

    Jebuls1=0
    Ch4plants1=0
    
    IF (Os1(i)>0) then
	    Ch4s1(i+1)= 1/(por*Vs1(i)+dt*(Dch4w*(beta1+beta2)*Area(i)+Qg(i)*coef0-Qg(i)*coef1 +por*Vs1(i)*Os1(i)/(Os1(i)*Ksato)*k1CH4 &
	    & + lamdaR*f1*fbs*b(i)*Rveg*DCH4air))*(por*Vs1(i)*CH4s1(i)+dt*(Dch4w* beta1 *Area(i)*CH4w(i)+Dch4w* beta2 *Area(i)*CH4s2(i) +&
	    & Qg(i)*CH4s2(i)*coef0-Qg(i)*CH4w(i)*coef1+lamdaR*f1*fbs*b(i)*Rveg*DCH4air*Cair))
	Else
	    CH4s1(i+1)=0
	end if
	
	! do you mass balance here before the concentrations are brought down to balance dlevels
	! check for ebullition
    If (Ch4s1(i+1)<0) then
        Ch4s1(i+1)=1e-8 ;    Ch4plants1=	0
    else
        Ch4plants1=	lamdaR*f1*fbs*b(i)*Rveg*DCH4air*(CH4s1(i)-Cair)*dt	! plant mediated transfer in grams
    end if
    
	if (Ch4s1(i+1)> Ebuls1) then
	    Jebuls1=(Ch4s1(i+1)-Ebuls1)* Vs1(i)*por !ebullition in grams at this step
	    Ch4s1(i+1)= Ebuls1
	end if
	
	
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!                                                      ANAEROBIC SEDIMENT LAYER	
	
	!LPOC in anaerobic sediment layer
	
	If (Os1(i) >0) then
	    LPOCs2(i+1)= 1/( Vs2(i)+dt*(Vs2(i)*kLPOC+porw*f2*vrLPOC*Area(i)+vbLPOC*Area(i)))*(Vs2(i)*LPOCs2(i) + dt*( fbs*f2*aca*kdb*FbLPOC*b(i)+&
	    &f2*porw*vsLPOC*Area(i)* LPOCw(i) +vbLPOC*Area(i)*LPOCs1(i)))
	else
	    LPOCs2(i+1)= 1/( Vs2(i)+dt*(Vs2(i)*kLPOC+porw*vrLPOC*Area(i)+vbLPOC*Area(i))) * (Vs2(i)*LPOCs2(i) + dt*(  fbs*aca*kdb*FbLPOC*b(i)+&
	    &porw*vsLPOC*Area(i)* LPOCw(i)))
	end if


	!RPOC in anaerobic sediment layer
	if (Os1(i)>0) then
	    RPOCs2(i+1)= 1/( Vs2(i)+dt*(Vs2(i)*kRPOC+porw*f2*vrRPOC*Area(i)+vbRPOC*Area(i))) * (Vs2(i)*RPOCs2(i) + &
	    &dt*( fbs*f2*aca*kdb*FbRPOC*b(i)+f2*porw*vsRPOC*Area(i)*RPOCw(i)+vbRPOC*Area(i)*RPOCs1(i)))
	else
	    RPOCs2(i+1)= 1/( Vs2(i)+dt*(Vs2(i)*kRPOC+porw*vrRPOC*Area(i)+vbRPOC*Area(i))) * (Vs2(i)*RPOCs2(i) + dt*( fbs*aca*kdb*FbRPOC*b(i)+porw*vsRPOC*Area(i)*RPOCw(i)))
	end if


	!DOC in anaerobic sediment layer
	IF (Os1(i)>0) then
	    DOCs2(i+1) = 1/(por*Vs2(i) + dt*( Dcw*beta2*Area(i) + Qg(i)* Coef0 - Qg(i)*Coef1 +&
	    & por*vbDOC*Area(i)+&
	    & por*Vs2(i)*NO3s2(i)/(NO3s2(i)+KN)*k2DOC+ &
	    & por*Vs2(i)*KinN/(NO3s2(i)+KinN)*k4DOC))* &
	    & ( por*Vs2(i)*DOCs2(i)+ dt*(fbs*f2*aca*kdb*FbDOC*b(i)+Vs2(i)*kLPOC*LPOCs2(i)+ Vs2(i)*kRPOC*RPOCs2(i)+ &
	    & Dcw*beta2*Area(i)*DOCs1(i)+ Qg(i)* DOCg(i)* Coef0 -Qg(i)*DOCs1(i)*Coef1+por*vbDOC*Area(i)*DOCs1(i)))
	Else
	    DOCs2(i+1) = 1/(por*Vs2(i) + dt*( Dcw*beta2*Area(i) + Qg(i)* Coef0 - Qg(i)*Coef1 +&
	    & por*vbDOC*Area(i)+&
	    & por*Vs2(i)*NO3s2(i)/(NO3s2(i)+KN)*k2DOC+ &
	    & por*Vs2(i)*KinN/(NO3s2(i)+KinN)*k4DOC))* &
	    & ( por*Vs2(i)*DOCs2(i)+ dt*(fbs*aca*kdb*FbDOC*b(i)+Vs2(i)*kLPOC*LPOCs2(i)+ Vs2(i)*kRPOC*RPOCs2(i)+ &
	    & Dcw*beta2*Area(i)*DOCw(i)+ Qg(i)* DOCg(i)* Coef0 -Qg(i)*DOCw(i)*Coef1))
	end if
	
	
	!CH4 in anaerobic sediment layer
	Jebuls2=0
	Ch4plants2=0
	
	IF (Os1(i)>0) then
	    Ch4s2(i+1)= 1/(por*Vs2(i)+dt*(por*Vs2(i)*NO3s2(i)/(NO3s2(i)+KN)*k2CH4&
	    & + Qg(i)*(Coef0-Coef1)+Dch4w*beta2*Area(i)+lamdaR*f2*fbs*b(i)*Rveg*DCH4air))*(por*Vs2(i)*CH4s2(i)+dt*(&
	    & amc*por*Vs2(i)*KinN/(KinN+No3s2(i))*k4DOC*DOCs2(i)+Qg(i)*Ch4gw(i)*Coef0-Qg(i)*Ch4s1(i)*Coef1+Dch4w*beta2*Area(i)*Ch4s1(i) +&
	    &lamdaR*f2*fbs*b(i)*Rveg*DCH4air*Cair))
	Else
	    Ch4s2(i+1)= 1/(por*Vs2(i)+dt*(por*Vs2(i)*NO3s2(i)/(NO3s2(i)+KN)*k2CH4&
	    & + Qg(i)*(Coef0-Coef1)+Dch4w*beta2*Area(i)+lamdaR*f2*fbs*b(i)*Rveg))*(por*Vs2(i)*CH4s2(i)+dt*(&
	    & amc*por*Vs2(i)*KinN/(KinN+No3s2(i))*k4DOC*DOCs2(i)+Qg(i)*Ch4gw(i)*Coef0-Qg(i)*Ch4w(i)*Coef1+Dch4w*beta0*Area(i)*Ch4w(i) +&
	    &lamdaR*f2*fbs*b(i)*Rveg*DCH4air*Cair))
	END IF
	
	! do you mass balance here before the concentrations are brought down to balance dlevels
	! check for ebullition
    
    If (Ch4s2(i+1)<0) then
        Ch4s2(i+1)=1e-8
        Ch4plants2=	0
    else
        Ch4plants2=	lamdaR*f2*fbs*b(i)*Rveg*DCH4air*(CH4s2(i)-Cair)*dt ! plant mediated transfer in gr. 
	end if
	
	
	if (Ch4s2(i+1)> Ebuls2) then
	    Jebuls2=(Ch4s2(i+1)-Ebuls2)* Vs2(i)*por !ebullition in grams at this step
	    Ch4s2(i+1)= Ebuls2
	ELSE
	    Jebuls2=0
	end if
	
!еееееееееееееееееееееееееееееееееееееееееееееееееееее  END OF CARBON EQUATIONS  ееееееееееееееееееееееееееееееееееееееееееееееееееееееее

!**********************************************************************************************************************
END SUBROUTINE
!**********************************************************************************************************************
	
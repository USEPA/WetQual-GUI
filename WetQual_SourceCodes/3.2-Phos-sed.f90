!**********************************************************************************************************************
! This subroutine simulates phosphorus and sediment processes in wetlands. 
!**********************************************************************************************************************

!**********************************************************************************************************************
SUBROUTINE PhosphorSed
!**********************************************************************************************************************

Use Parm ; Use Pprime 

!**********************************************************************************************************************
    If (Qg(i)<0) then
        gw_Pw=  Fdw*Qg(i)*Pw(i)
        gw_Ps1=-Fdw*Qg(i)*Pw(i)+Fds*Qg(i)*Ps1(i)
        gw_Ps2=-Fds*Qg(i)*Ps1(i)+Qg(i)*Fds1*Ps2(i)
    else
        gw_Pw=Fds*Qg(i)*Ps1(i)
        gw_Ps1=Fds1*Qg(i)*Ps2(i)-Fds*Qg(i)*Ps1(i)
        gw_Ps2=+Qg(i)*Fds1*Ps2(i)-Qg(i)*Pg(i)
    end if 
			            
! Pw water column    
if (Ow(i)>0) Then 
!**********************************************************************************************************************
!**********************************************************************************************************************
    velr_s=min(velr_s,(vels_s*mw(i)/ms(i)+1/(dt*porw)))     !last term is actaully Vs/Area(i)*depth, we assumed depth=1cm
    vrvelr_s=vrvelr_s+velr_s     
!**********************************************************************************************************************
    Pw(i+1)=(1/(Vw(i+1)+(dt)*(porw*vels_s*Area(i)*Fsw*mw(i)+Dpw*beta1*Area(i)*Fdw+Qout(i)+0*Coef0-Qg(i)*Fdw*Coef1)))*(Pw(i)*Vw(i)+(dt)*(Qin(i)*Pin(i)+&
    &(1/(L1(i)+L2))*Area(i)*porw*velr_s*ms(i)*(L1(i)*Fss*Ps1(i)+L2*Fss1*Ps2(i))+Qg(i)*Fds*Ps1(i)*Coef0+0*Coef1-apa*KgaTIa*a(i)+Vw(i)*apn*kminw*Onw(i)+Dpw*beta1*Area(i)*Fds*Ps1(i)))           
Else
    Pw(i+1)=(1/(Vw(i+1)+(dt)*(porw*vels_s*Area(i)*Fsw*mw(i)+Dpw*beta0*Area(i)*Fdw+ Qout(i)+0*Coef0-Qg(i)*Fdw*Coef1)))*(Pw(i)*Vw(i)+(dt)*(Qin(i)*Pin(i)+&
    &porw*velr_s*Area(i)*Fss*ms(i)*Ps2(i)+Qg(i)*Fds*Ps2(i)*Coef0-0*Coef1-apa*KgaTIa*a(i)+Vw(i)*apn*kminw*Onw(i)+Dpw*beta0*Area(i)*Fds*Ps2(i)))           
End if

!Ps1 sediment layer                      
if (Ow(i)>0) Then
!**********************************************************************************************************************
!**********************************************************************************************************************
     Ps1(i+1)=(1/(1+(dt/(Vs1(i)))*(f1*Area(i)*porw*velr_s*Fss*ms(i)+beta1*Dpw*Area(i)*Fds+Area(i)*Velb+beta2*Dpw*Area(i)*Fds+Qg(i)*Fds*Coef0&
     &-Qg(i)*Fds*Coef1)))*(Ps1(i)+ (dt/(Vs1(i)))*(f1*porw*vels_s*Area(i)*Fsw*mw(i)*Pw(i)+beta1*Dpw*Area(i)*Fdw*Pw(i)+Qg(i)*Fds*Ps2(i)*Coef0-Qg(i)&
     &*Fdw*Pw(i)*Coef1+beta2*Dpw*Area(i)*Fds1*Ps2(i)-apa*KgbTIb*b(i)+Vs1(i)*kmin1s*ONsf(i)*apn+Vs1(i)*kmin2s*ONss(i)*apn))   
Else
    Ps1(i+1)= 0
End if

!Ps2 Anaerobic layer 
if (Ow(i)>0) Then
!**********************************************************************************************************************
!**********************************************************************************************************************
     Ps2(i+1)=(1/(1+(dt/(Vs2(i)))*(f2*Area(i)*porw*velr_s*Fss1*ms(i)+Area(i)*Velb+beta2*Dpw*Area(i)*Fds1+Qg(i)*Fds1*Coef0-Qg(i)*Fds1*Coef1)))*&
     &(Ps2(i)+(dt/(Vs2(i)))*(f2*Area(i)*porw*vels_s*Fsw*mw(i)*Pw(i)+Vs2(i)*apn*kmin1s*ONsf(i)+ Vs2(i)*apn*kmin2s*ONss(i)+Qg(i)*Pg(i)*Coef0-Qg(i)&
     &*Fds*Ps1(i)*Coef1+Area(i)*Velb*Ps1(i)+Beta2*Dpw*Area(i)*Fds*Ps1(i)-apa*KgbTIb*b(i)))
Else
     Ps2(i+1)=(1/(1+(dt/(Vs2(i)))*(Area(i)*Velb+beta0*Area(i)*Fds1+Qg(i)*Fds1*Coef0-Qg(i)*Fds1*Coef1)))*(Ps2(i)+(dt/(Vs2(i)))*(Vs2(i)*apn*kmin1s*ONsf(i)+Qg(i)*Pg(i)&
     &*Coef0-Qg(i)*Fdw*Pw(i)*Coef1+ Vs2(i)*apn*kmin2s*ONss(i)+beta0*Area(i)*Fdw*Pw(i)-apa*KgbTIb*b(i)))
End if

!**********************************************************************************************************************
   mw(i+1)=(1/(porw*Vw(i+1)+(dt)*(vels_s*porw*Area(i)+Qout(i))))*(mw(i)*porw*Vw(i)+(dt)*(Qin(i)*mwin(i)+porw*velr_s*Area(i)*ms(i)))
!**********************************************************************************************************************

!**********************************************************************************************************************
END SUBROUTINE
!**********************************************************************************************************************

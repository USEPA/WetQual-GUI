!**********************************************************************************************************************
! This subroutine simulates nitrogen transformation and removal in three layers including water column, 
!                           aerobic and anaerobic soil layers.
!**********************************************************************************************************************

!**********************************************************************************************************************
SUBROUTINE Nitrogen
!**********************************************************************************************************************
! This subroutine calculates sediment oxygen demand (SOD) and oxygen in water column as well as mass of floating plant.
!**********************************************************************************************************************

!**********************************************************************************************************************
Use Parm ; Use Pprime 
!**********************************************************************************************************************
                !Organic Nitrogen (particulate Nitrogen) in Water Column  ONW
!**********************************************************************************************************************
    ONw(i+1)=(1/(porw*Vw(i+1)+dt*kminw*porw*Vw(i)+dt*vels_o*porw*Area(i)+dt*Qout(i)))*(porw*Vw(i)*ONw(i)+(dt)*(Qin(i)*ONin(i)+ana*kda*a(i)&
    &+ana*kdb*b(i)+porw*velr_o*Area(i)*(ONsf(i)+ONss(i))+Area(i)*S*fw))    
!**********************************************************************************************************************
       	   	    !labile Organic Nitrogen (particulate Nitrogen) in  Sediment Layer ONSr                             
!**********************************************************************************************************************
    ONsf(i+1)=(1/(1+dt*porw*velr_o*Area(i)/Vs(i)+dt*kmin1s+dt*velb*Area(i)/Vs(i)))*(ONsf(i)+(dt/Vs(i))*(frap*ana*kdb*b(i)+frap*porw*vels_o*Area(i)*ONw(i)+frap*(1-fw)*Area(i)*S))

!**********************************************************************************************************************

                !refractory Organic Nitrogen (particulate Nitrogen) in  Sediment Layer ONSs
!**********************************************************************************************************************
    ONss(i+1)=(1/(1+dt*porw*velr_o*Area(i)/Vs(i)+dt*kmin2s+dt*velb*Area(i)/Vs(i)))*(ONss(i)+(dt/Vs(i))*(fslw*ana*kdb*b(i)+fslw*porw*vels_o*Area(i)*ONw(i)+fslw*(1-fw)*Area(i)*S))              
!**********************************************************************************************************************
    If (Qg(i)<0) then
        gw_Nw=Qg(i)*Nw(i)
        gw_Ns1=-Qg(i)*Nw(i)+Qg(i)*Ns1(i)
        gw_Ns2=-Qg(i)*Ns1(i)+Qg(i)*Ns2(i)
    else
        gw_Nw=Qg(i)*Ns1(i)
        gw_Ns1=Qg(i)*Ns2(i)-Qg(i)*Ns1(i)
        gw_Ns2=Qg(i)*Ng(i)-Qg(i)*Ns2(i)
    end if 
!**********************************************************************************************************************
                !Amonia (NH4+NH3) in Water Column Nw                
!**********************************************************************************************************************
if (Ow(i)>0) Then                    
   Nw(i+1)=(1/(porw*Vw(i+1)+(dt)*(porw*Vw(i+1)*fN*knw*(1-exp(-0.6*Ow(i)*1e6))+Daw*beta1*Area(i)+kv*porw*Area(i)*(1-fN)+Qout(i)+0*Coef0&
   &-Qg(i)*Coef1)))*(porw*Vw(i)*Nw(i)+(dt)*(Qin(i)*Nwin(i)+ip(i)*Area(i)*Nair(i)+Daw*beta1*Area(i)*Ns1(i)+Qg(i)*Ns1(i)*Coef0-0*Coef1+&
   &porw*Vw(i)*Kminw*ONw(i)-fNw*ana*KgaTIa*a(i)+Area(i)*Qa(i)))                                             
Else
   Nw(i+1)=(1/(porw*Vw(i+1)+(dt)*(Daw*beta1*Area(i)+kv*porw*Area(i)*(1-fN)+Qout(i)+0*Coef0+Qg(i)*Coef1)))*(porw*Vw(i)*Nw(i)+(dt)*&
   &(Qin(i)*Nwin(i)+ip(i)*Area(i)*Nair(i)+Daw*beta1*Area(i)*Ns2(i)+Qg(i)*Ns2(i)*Coef0+0*Coef1+porw*Vw(i)*Kminw*ONw(i)-fNw*ana*KgaTIa*a(i)+Area(i)*Qa(i)))                       
End if
!**********************************************************************************************************************

!**********************************************************************************************************************
                !Amonia (NH4+NH3) in Aerobic Sediment Layer Ns1                
!**********************************************************************************************************************
if (Ow(i)>0) Then
   Ns1(i+1)=(1/(1+(dt/((por+rows*Kd*fN)*Vs1(i)))*(Daw*beta1*Area(i)+por*Area(i)*velb+por*Vs1(i)*fN*kns*(1-exp(-0.3*Ow(i)*1e6))+beta2*&
   &Daw*Area(i)+Qg(i)*Coef0-Qg(i)*Coef1)))*(Ns1(i)+(dt/((por+rows*Kd*fN)*Vs1(i)))*(Daw*beta1*Area(i)*(Nw(i))+Qg(i)*Ns2(i)*Coef0-Qg(i)*&
   &Nw(i)*Coef1-fNs1*ana*KgbTIb*b(i)+beta2*Daw*Area(i)*(Ns2(i))+Vs1(i)*kmin1s*ONsf(i)+Vs1(i)*kmin2s*ONss(i)))
Else
   Ns1(i+1)=0
End if
!**********************************************************************************************************************

!**********************************************************************************************************************
                !Amonia (NH4+NH3) in Anaerobic Sediment Layer  Ns2                
!**********************************************************************************************************************
if (Ow(i)>0) Then
   Ns2(i+1)=(1/(1+(dt/((por+rows*Kd*fN)*Vs2(i)))*(beta2*Daw*Area(i)+velb*por*Area(i)+Qg(i)*Coef0-Qg(i)*Coef1)))*(Ns2(i)+ (dt/((por+rows*Kd*fN)&
   &*Vs2(i)))*(beta2*Daw*Area(i)*Ns1(i)+velb*por*Area(i)*Ns1(i)+Qg(i)*Ng(i)*Coef0-Qg(i)*Ns1(i)*Coef1+Vs2(i)*kmin1s*ONsf(i)+Vs2(i)*kmin2s*ONss(i)-fns2*ana*KgbTIb*b(i)))                                                     
Else
   Ns2(i+1)=(1/(1+(dt/((por+rows*Kd*fN)*Vs2(i)))*(Daw*beta0*Area(i)+velb*por*Area(i)+Qg(i)*Coef0-Qg(i)*Coef1)))*(Ns2(i)+ (dt/(por+rows*Kd*fN)*Vs2(i))&
   &*(Daw*beta0*Area(i)*Nw(i)+Qg(i)*Ng(i)*Coef0-Qg(i)*Nw(i)*Coef1+Vs2(i)*kmin1s*ONsf(i)+Vs2(i)*kmin2s*ONss(i)-fns2*ana*kgbTIb*b(i)))                                    
End if
!**********************************************************************************************************************
    If (Qg(i)<0) then
        gw_NO3w=Qg(i)*NO3w(i)
        gw_NO3s1=-Qg(i)*NO3w(i)+Qg(i)*NO3s1(i)
        gw_NO3s2=-Qg(i)*NO3s1(i)+Qg(i)*NO3s2(i)
    else
        gw_NO3w=Qg(i)*NO3s1(i)
        gw_NO3s1=Qg(i)*NO3s2(i)-Qg(i)*NO3s1(i)
        gw_NO3s2=-Qg(i)*NO3s2(i)+ Qg(i)*NO3g(i)
    end if 
!**********************************************************************************************************************
    
!**********************************************************************************************************************
                !Nitrate (NO3) in Water Column NO3w
!**********************************************************************************************************************
if (Ow(i)>0) Then
    NO3w(i+1)=(1/(porw*Vw(i+1)+(dt)*(Dnw*beta1*Area(i)+Qout(i)+0*Coef0-Qg(i)*Coef1)))*(porw*Vw(i)*NO3w(i)+(dt)*(Qin(i)*NO3in(i)+ip(i)&
    &*Area(i)*NO3air(i)+ porw*Vw(i)*fN*knw*(1-exp(-0.6*Ow(i)*1e6))*Nw(i)+Dnw*beta1*Area(i)*NO3s1(i)+Qg(i)*NO3s1(i)*Coef0-&
    &0*Coef1-(fNO3w)*ana*KgaTIa*a(i)+Area(i)*Qn(i)))                
Else    			           
!**********************************************************************************************************************
     NO3w(i+1)=(1/(porw*Vw(i+1)+(dt)*(Dnw*beta0*Area(i)+porw*Vw(i)*NO3w(i)/(NO3w(i)+KN)*kden+Qout(i)+0*Coef0-Qg(i)*Coef1)))*(NO3w(i)*porw*Vw(i)+(dt)*(Qin(i)*NO3in(i)&
     &+ip(i)*Area(i)*NO3air(i)+Dnw*beta0*Area(i)*NO3s2(i)-(fNO3w)*ana*KgaTIa*a(i)+Area(i)*Qn(i)))
End if
!**********************************************************************************************************************

!**********************************************************************************************************************
                !Nitrate (NO3) in Aerobic Sediment Layer NO3s1
!**********************************************************************************************************************
if (Ow(i)>0) Then 
    NO3s1(i+1)=1/(por*Vs1(i)+dt*(Dnw*beta1*Area(i)+beta2*Dnw*Area(i)+velb*por*Area(i)+Qg(i)*Coef0-Qg(i)*Coef1)) &
    & *(NO3s1(i)*por*Vs1(i)+dt*(Dnw*beta1*Area(i)*NO3w(i)+Qg(i)*NO3s2(i)*Coef0-Qg(i)*NO3w(i)*Coef1+por*Vs1(i)*fN*kns*(1-exp(-0.3*Ow(i)*1e6))*Ns1(i)+beta2*Dnw*Area(i)*NO3s2(i)-fNO3s1*ana*KgbTIb*b(i)))
Else 
    NO3s1(i+1)=0
End if
!**********************************************************************************************************************

!**********************************************************************************************************************
                !Nitrate (NO3) in Anaerobic Sediment Layer  NO3s2                
!**********************************************************************************************************************
if (Ow(i)>0) Then                               
    NO3s2(i+1)=(1/(1+(dt/(por*Vs2(i)))*((beta2*Dnw*Area(i)+por*Vs2(i)*NO3s2(i)/(NO3s2(i)+KN)*kden+por*Area(i)*velb+Qg(i)*Coef0-Qg(i)*Coef1))))*(NO3s2(i)+(dt/(por*Vs2(i)))*&
    &(beta2*Dnw*Area(i)*NO3s1(i)+por*Area(i)*velb*NO3s1(i)+Qg(i)*NO3g(i)*Coef0-Qg(i)*NO3s1(i)*Coef1-fNO3s2*ana*KgbTIb*b(i)))                                 
Else                                                
    NO3s2(i+1)=(1/(1+(dt/(por*Vs2(i)))*((beta0*Dnw*Area(i)+por*Vs2(i)*NO3s2(i)/(NO3s2(i)+KN)*kden+por*Area(i)*velb+Qg(i)*Coef0-Qg(i)*Coef1))))*(NO3s2(i)+(dt/(por*Vs2(i)))*&
    &(Dnw*beta0*Area(i)*NO3w(i)+Qg(i)*NO3g(i)*Coef0-Qg(i)*NO3w(i)*Coef1-fNO3s2*ana*KgbTIb*b(i)))              
End if
!**********************************************************************************************************************
                
!**********************************************************************************************************************
END SUBROUTINE
!**********************************************************************************************************************

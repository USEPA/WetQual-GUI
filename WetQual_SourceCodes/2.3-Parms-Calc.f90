!**********************************************************************************************************************
! This subroutine reads some of the WetQual parameters which are considered to be random.
!
!N, P, TSS parameters (�14_generated_parameters.txt� file)
!Symbol in publications	Symbols in the model files	Definition, Units
!l2	    ::L2fix	    ::Thickness of anaerobic soil layer (cm)
!?	    ::theta	    ::Temperature coefficient in Arhenious equation
!Is	    ::Is	    ::Optimal light level (ly/day), range from about 100 to 400 ly/d (Chapra, 1997, p. 611)
!fN	    ::fNup	    ::Fraction of total ammonia nitrogen in ionized form
!kd	    ::Kd 	    ::Ammonium ion distribution coefficient (mL/g)
!kep	::kep	    ::Parameter required as input but not used in the model (1/m)
!kga	::kga0	    ::Growth rate of free-floating plant (1/day)
!kgb	::kgb0	    ::Growth rate of benthic and rooted plant (1/day)
!kmr	::kmin1s    ::First-order rapid mineralization rate in wetland soil (1/day)
!knw	::knw	    ::First-order nitrification rate in wetland free water (1/day)
!kmw	::kminw	    ::First-order mineralization rate in wetland free water (1/day)
!kns	::kns	    ::First-order nitrification rate in aerobic soil layer (1/day)
!kdn	::kden	    ::Denitrification rate in anaerobic soil layer (1/day)
!?s	    ::rows	    ::Wetland soil particle density (g/cm3)
!vso	::vels_o    ::Effective settling velocity (cm/day) for organic material
!vss	::vels_s    ::Effective settling velocity (cm/day) for sediment
!vb	    ::velb	    ::Effective burial velocity (cm/day)
!ana	::ana	    ::Gram of nitrogen per gram of chlorophyll-a in plant/algae (gN/gChl)
!rc,chl	::rChl	    ::Ratio of carbon mass to chlorophyll a mass in algae (gC/gChl)
!Ss	    ::Ss	    ::Oxygen removal rate per unit volume of aerobic layer by other processes (g/L/day)
!Sw	    ::Sw	    ::Volumetric oxygen consumption rate in water by other processes (gr/cm3/day)
!?	    ::c_uw	    ::Empirical parameter used for calculating volatilization mass transfer velocity kv
!fr	    ::frap	    ::Fraction of rapidly mineralizing particulate organic matter
!c1	    ::c1	    ::Used for calculating pK (Keq, equilibrium coefficient)
!c2	    ::c2	    ::Used for calculating pK (Keq, equilibrium coefficient)
!pH	    ::PH	    ::pH
!S	    ::S	        ::Rate of nitrogen fixation by microorganisms (mg-N/m3/hr)
!Kw	    ::Kw	    ::Phosphorus sorption coefficient in water (cm3/g)
!apa	::apa	    ::Ratio of phosphorus to Chlorophyll-a in algae (grP/grChl)
!Dpw	::Dpw	    ::Inorganic phosphorus free-water diffusion coefficient (cm2/day)
!Ksa	::Ksa	    ::Accounts for partitioning to phosphorus sorption site (cm3/g)
!Ksb	::Ksb	    ::Accounts for association with iron hydroxide precipitate (cm3/g)
!Ran1	::Ran1	    ::Random number used for calculating soil porosity (?) and free-water oxygen diffusion coefficient
!fw	    ::fW	    ::Fraction of nitrogen fixation in water
!fact	::fact	    ::Vertical diffusion magnification factor
!?ro	::alfa_velr_o::Coefficient for resuspension/recycling of organic material
!?rs	::alfa_velr_s::Coefficient for resuspension/recycling of sediment
!?w	    ::porw	    ::Effective porosity of wetland surface water
!
!Carbon parameters (�15_generated_parameters_carbon.txt� file)
!Symbol in publications	Symbols in the model files	Definition, Units
!aca	::aca	    ::ratio of carbon to chlorophyll-a in algae (gC/gChl)
!faD	::FaDOC	    ::Fraction of dissolved organic C produced by death/loss of free floating plants and attached algae (faL + faR + faD = 1) (Dimensionless)
!faL	::FaLPOC	::Fraction of labile particulate C produced by death/loss of free floating plants and attached algae (faL + faR + faD = 1) (Dimensionless)
!faR	::FaRPOC	::Fraction of refractory particulate C produced by death/loss of free floating plants and attached algae (faL + faR + faD = 1) (Dimensionless)
!fbD	::FbDOC	    ::Fraction of dissolved organic C produced by death/loss of rooted and benthic plants (fbL + fbR + fbD = 1)
!fbL	::FbLPOC	::Fraction of labile particulate C produced by death/loss of rooted and benthic plants (fbL + fbR + fbD = 1)
!fbR	::FbRPOC	::Fraction of refractory particulate C produced by death/loss of rooted and benthic plants (fbL + fbR + fbD = 1)
!kL 	::kLPOC	    ::first order hydrolysis rate of labile particulate organic carbon (1/day)
!kR 	::kRPOC	    ::First order hydrolysis rate of refractory particulate organic carbon (1/day)
!KO 	::KsatO	    ::Michaelis�Menten half saturation concentration of dissolved oxygen required for oxic respiration (mg/L)
!KinO	::KinO	    ::Michaelis�Menten oxygen inhabitation coefficient (mg/L)
!KN 	::KN	    ::Michaelis�Menten nitrate N half saturation concentration required for denitrification (mg/L)
!KinN	::KinN	    ::Michaelis�Menten nitrate-N inhibition coefficient (mg/L)
!k1D	::K1DOC	    ::maximum dissolved organic C utilization rate for aerobic respiration (1/day)
!k2D	::k2DOC	    ::maximum dissolved organic C utilization rate for denitrification (1/day)
!k3D	::k3DOC	    ::maximum dissolved organic C utilization rate for methanogenesis in anaerobic water (1/day)
!k4D	::k4DOC	    ::maximum dissolved organic C utilization rate for methanogenesis in anaerobic sediment (1/day)
!cp1	::cp1	    ::fraction of inflowing organic carbon (TOCin) in form of dissolved organic carbon (DOC)
!Cp2	::cp2	    ::fraction of inflowing organic carbon in form of labile particulate organic carbon (LPOC)
!Cp3	::cp3	    ::fraction of inflowing organic carbon in form of refractory particulate organic carbon (RPOC)
!fbw	::fbw	    ::fraction of rooted plant biomass above soil-water interface (1/day)
!k1M	::k1CH4	    ::maximum methane utilization rate for aerobic respiration (1/day)
!k2M	::k2CH4	    ::maximum methane utilization rate for denitrification (1/day)
!Rv	    ::Rveg	    ::root length density in soil (cm/gr) [L root/M chla]
!
!**********************************************************************************************************************

!**********************************************************************************************************************
SUBROUTINE ReadParms
!**********************************************************************************************************************

Use parm ; use Pprime
integer ii,j,k,ni
!**********************************************************************************************************************

!!TotMNw = 0;

    read (14,*) L2fix,theta,Is,fNup,kd,kep,kga0,kgb0,kmin1s,knw,kminw,kns,kden,rows,vels_o,vels_s,velb,ana,rChl,Ss,Sw,c_uw,frap,c1,c2,PH,S,Kw,apa,Dpw,Ksa,Ksb,Ran1,fW,fact,alfa_velr_o,alfa_velr_s,porw

    !�������  A�ir  ��������
     read (15,*) aca, FaDOC,FaLPOC,FaRPOC,FbDOC,FbLPOC,FbRPOC,kLPOC,kRPOC,KsatO,KinO,KN,KinN,k1DOC,k2DOC,k3DOC,k4DOC,cp1,cp2,cp3,fbw,k1ch4,k2ch4,Rveg
    !������������������������
!**********************************************************************************************************************


!**********************************************************************************************************************
            !This is the part we initialize fN values
            TN = Nw(0)+Ns1(0)+Ns2(0)+NO3w(0)+NO3s1(0)+NO3s2(0)
            fNw=(fNup*Nw(0))/TN
            fNs1=(fNup*Ns1(0))/TN
            fNs2=(fNup*Ns2(0))/TN
            fNO3w=(NO3w(0))/TN
            fNO3s1=(NO3s1(0))/TN
            fNO3s2=(NO3s2(0))/TN
!**********************************************************************************************************************

!**********************************************************************************************************************
    !In this part porosity is defined based on the L2.
    if(L2fix>10) Then
        por = 0.5+0.3*Ran1 !por is between U(0.5,0.8) here
    Else
        por=0.7+0.2*Ran1   !por is between U(0.7,0.9)here
    End if
    sumset(0)=0 !0.5*Area(0)*por/dt
    ms(0)=rows*(1-por)  !This is to initiliaze ms()

  !  Vb(0)=6586.8*1e6 ! This is to initiliaze Vb
!**********************************************************************************************************************

!**********************************************************************************************************************
   Is=Is*4.19          !to convert ly/day to J/(cm2day)
   pi=4*atan(1.0)
   S=S*1e-8     !S is generated in excel file with S*1e8 and here it is converted to original value
   fslw = 1-frap
!**********************************************************************************************************************
	! this part is used to calculate R(i) values for plant growth
!**********************************************************************************************************************
!**********************************************************************************************************************
        SumR0=0
        Sumkga=0
        Sumkgb=0
        do ii=1,366
            Sigma = Asin(0.4*sin(2*pi*(ii-82.0)/365.0))
            !Tsr1 = Acos(-tan(Sigma)*tan(lat))
            Tsr1 = Acos(-tan(Sigma)*tan(lat))/w
            ro = 1 + 0.033*cos(2*pi*ii/365.0)
            !R(i)= 30*ro*(Tsr1*Sin(Sigma)*Sin(lat)+cos(Sigma)*Cos(lat)*Sin(Tsr1))
            R(ii)= 30*ro*(w*Tsr1*Sin(Sigma)*Sin(lat)+cos(Sigma)*Cos(lat)*Sin(w*Tsr1))
            SumR0 = SumR0 + R(ii)
        End do
!**********************************************************************************************************************
!**********************************************************************************************************************
        ii=0
        ni=0
        do while(ii<(int(real(n)/dt+dt/2)+1))
          !do ii=0,int((n)/dt+0.01),int(1/dt+0.01)
            Ri=R(dn(int(ii*dt+dt/2)))
            kga(ii)=(365*Ri*kga0)/SumR0
            kgb(ii)=(365*Ri*kgb0)/SumR0
            Sumkga=Sumkga+kga(ii)
            Sumkgb=Sumkgb+kgb(ii)

           ! print*, ii,kga(ii),kgb(ii)
           !print*, ii,dn(int(ii*dt+dt/2)),kga(ii),kgb(ii),kga0,kgb0
            ni=ni+1
	        ii=int(ni/dt+dt/2)

        end do
!**********************************************************************************************************************
             Kda_fixed = Sumkga/n
             Kdb_fixed = Sumkgb/n
!**********************************************************************************************************************
            !Interpolation
        do ii=0,int((n)/dt+dt/2)
            j=int(int(ii*dt+dt/2)/dt+dt/2)   !lower bound of interpolation
 	        k= j+int(1/dt+dt/2)             !upper bound of interpolation
            if (ii /= j) then
                kga(ii)=kga(j)+(kga(k)-kga(j))*real(i-j)/real(k-j)
                kgb(ii)=kgb(j)+(kgb(k)-kgb(j))*real(i-j)/real(k-j)
            end if
        end do
!**********************************************************************************************************************
! 	        open (202,file="202.txt",status="unknown")
! 	    DO ii=0,int((n/dt-1)+dt/2)
! 	        write (202,"(I10,4F15.5)")ii,kga(ii),kgb(ii),R(ii),SumR0
!        end do
!**********************************************************************************************************************
!**********************************************************************************************************************
            S_fixed = S             ;   !���������������  A�ir  ����������������
            kminw_fixed = kminw     ;   kLPOC_fixed = kLPOC
            kmin1s_fixed = kmin1s   ;   kRPOC_fixed = kRPOC
            knw_fixed = knw         ;   KsatO_fixed= KsatO*1e-6		!mg/lit to gr/cm3
            kns_fixed = kns         ;   KinO_fixed=KinO*1e-6		!mg/lit to gr/cm3
            kden_fixed = kden       ;   KN_fixed=KN*1e-6			!mg/lit to gr/cm3
            Ss_fixed=Ss             ;   KinN_fixed=KinN*1e-6		!mg/lit to gr/cm3
            Sw_fixed=Sw             ;   K1DOC_fixed=K1DOC
            Dpw_fixed=Dpw           ;   k2DOC_fixed=k2DOC
                                        k3DOC_fixed= k3DOC
                                        k4DOC_fixed= k4DOC
                                        k1CH4_fixed=k1CH4
                                        k2CH4_fixed=k2CH4
                                        !���������������������������������������

!**********************************************************************************************************************
END SUBROUTINE
!**********************************************************************************************************************


!**********************************************************************************************************************
SUBROUTINE GeneralCalc1
!**********************************************************************************************************************

   Use Parm ; Use Pprime

!**********************************************************************************************************************
          velr_o=alfa_velr_o*((Qin(i)/(100*100*100))**1.1)/10000      !cm/day
          velr_s=alfa_velr_s*((Qin(i)/(100*100*100))**1.1)/10000      !cm/day

            !ms and velb is updated at each step
            ms(i+1)=rows*(1-por)
            velb=porw*vels_s*mw(i)/ms(i)-porw*velr_s
!**********************************************************************************************************************
            !General calculations of parameters
!**********************************************************************************************************************

            tor=(por**(4./3.)) ! used to be 1/3
            Mu_T = 0.0005*exp(-0.0762*temp(i))+0.0013*exp(-0.0177*temp(i))
            Dow = 0.864*(0.2604+0.006383*(temp(i)+273)/(Mu_T*1000))*(0.5+Ran1) !cm2/day
            Daw = 0.0864*(9.5+0.413*temp(i)) !cm2/day
            Dnw= 0.0864*(9.5+0.388*temp(i)) ! base temp is 25 degrees Dnw@ 25= 1.66
            Dpw = 0.0864*(3.3+0.181*temp(i)) !SI ! cm2/day

            !----- DOC Carbon diffusivity in water ------
            Dcw = 0.0864*(9.5+0.3319*temp(i))                                               ! !���������������  A�ir  ! base temp is 25 degrees Dnw@ 25= 1.538 cm2/day, OC diffusivity in water

            !----Methane arbon diffusivity in water and air------------------
            Dch4w = (0.9798+0.02986*temp(i)+0.0004381*temp(i)**2)*0.000000001*24*3600*10000 ! cm2/day from Wania et al., 2010 CH4 diffusivity in water
            DCH4air= 1.9*1E-5* ((temp(i)+273)/298)**1.82


            !-------- Methane diffusion to atmosphere-------------------
            ScCH4=1898.0-110.1*temp(i) +2.834*temp(i)**2.0-0.02791*temp(i)**3.0             ! Methane Schmidt number, from Wania et al 2010
            JCH4=  (0.17* Uw(i))*(  ScCH4/600.)**0.5 * 24                                   ! J= cm/day -- k600=(0.17* Uw(i)) is from Wanninkohof et al.(2010) and has a unit of cm/hr, the rest is from wania et al 2010                                    


            !Dch4w= 1.5*0.000000001*((temp(i)+273.1)/298)*24*3600*10000                     ! cm2/day from Tang et al., 2010    CH4 diffusivity in wate
            !JCH4 =(2.07+0.215*(0)**1.7)*36.0*((ScCH4/600.0)**(-0.5)) *24*3600*100          ! Jch4 is methane mass transfer rate cm/day. You can put wind speed at wetland level (m/s) instead of 0. Wania et al, 2010
                                                                                            !J from Wania tunes out as a big number when multiplied by 24*3600*100 has to change


            !----------------Ebullition concentration---------------------------
            Spp=0.05708-0.001545*temp(i)+0.0002068*temp(i)**2                               ! Wania et al.,2010- Bunsen solubility coefficient defined as volume of gas dissolved per volume of liquid at atmospheric pressure and a given temperature
            PPs1= 101.3*1000.+ H(i)/100*1000*9.81                                           ! pressure at aerobic sediment (pascal)
            PPs2= 101.3*1000.+ (H(i)+L2fix)/100.0*1000.0*9.81                               ! pressure at anaerobic sediment (pascal)
            Ebuls1=pps1/8.3145/(temp(i)+273.15)*Spp*16.04/1e9 * 0.15                      ! maximum solubility concentration of methane in aerobic sediment later (gr/cm3)
            Ebuls2=pps2/8.3145/(temp(i)+273.15)*Spp*16.04/1e9 *0.15                       ! maximum solubility concentration of methane in anaerobic sediment later (gr/cm3)


            !---- Cair Calculations-----------------------
            kHCH4 = (0.0014*exp(-1700.0*(1/(temp(i)+273.15)-1/298.0)))                      ! henry constant for ch4 (mol.m-3.atm-1) Tang et al 2010 
            Cair= 1.79*10**(-6) * kHCH4 * 16.04/1000                                        ! gr/cm3 from Wania et al 2010

            !---------------------------------

             kmin2s = kmin1s*0.1
            kda=kda_fixed*(1.02**(temp(i)-20)) !take theta=1.02
            kdb=kdb_fixed*(1.02**(temp(i)-20)) !take theta=1.02
            Osat=exp(-139.34411+1.575701e5/(temp(i)+273)-6.642308e7/(temp(i)+273)**2+1.2438e10/(temp(i)+273)**3-8.621949e11/(temp(i)+273)**4)
            Osat=Osat*1e-6 !to convert mg/lt to gr/cm3
            KgbTIb = kgb(i)*(1.02**(temp(i)-20))  !Kgb(i)*(theta**(temp(i)-20))*philb!take theta=1.02
            KgaTIa= kga(i)*(1.02**(temp(i)-20))  !kga(i)*(theta**(temp(i)-20))*phila!take theta=1.02


!**********************************************************************************************************************
!****                         Temperature adjustment                                                            *******
!**********************************************************************************************************************
            S=S_fixed*(theta**(temp(i)-20))
            kminw=kminw_fixed*(theta**(temp(i)-20))
            kmin1s=kmin1s_fixed*(theta**(temp(i)-20))
            kmin2s = kmin1s*0.1
            knw = knw_fixed *(theta**(temp(i)-20))
            kns = kns_fixed *(theta**(temp(i)-20))
            kden =  kden_fixed *(theta**(temp(i)-20))
            Ss = Ss_fixed*(theta**(temp(i)-20))
            Sw = Sw_fixed*(theta**(temp(i)-20))



            !���������������  A�ir  ����������������
            kRPOC = kRPOC_fixed*(theta**(temp(i)-20))
            KsatO= KsatO_fixed*(theta**(temp(i)-20))
            KinO=KinO_fixed*(theta**(temp(i)-20))
            KN=KN_fixed*(theta**(temp(i)-20))
            KinN=KinN_fixed*(theta**(temp(i)-20))
            K1DOC=K1DOC_fixed*(theta**(temp(i)-20))
            k2DOC=k2DOC_fixed*(theta**(temp(i)-20))
            k3DOC= k3DOC_fixed*(theta**(temp(i)-20))
            k4DOC= k4DOC_fixed*(theta**(temp(i)-20))
            k1CH4= k1CH4_fixed*(theta**(temp(i)-20))
            k2CH4= k2CH4_fixed*(theta**(temp(i)-20))
            !���������������������������������������


!**********************************************************************************************************************
!SInew*****************************************************************
        ko=(c_uw*Uw(i)*100)*(1.024**(temp(i)-20)) !cm/day                                !Broecker et al. (1978)
		kv=((1.17*c_uw)/(1+12.07*c_uw)*Uw(i)*100)*(1.024**(temp(i)-20))					 !cm/day

!       ko=100*0.0986*Uw(i)**1.64														 !Wanninkhof(1991)
!       ko=100*(0.728*Uw(i)**(1./2.) - 0.317*Uw(i) + 0.0372*Uw(i)**2)					 !Banks and Herrera (1977)
!**********************************************************************************************************************
!**********************************************************************************************************************
	    ! Aerobic/Anaerobic thickness
	       !d_bound=h(i)/2.0
	        omega=ronn*por*fN*Kns*Ns1(i)+rond*(Kmin1s*ONsf(i)+Kmin2s*ONss(i)) +Ss
            L1(i) = -por*tor*d_bound + sqrt(((por*tor*d_bound)**2)+(2*por*tor*DOw*ow(i))/omega)     ! was changed slightly from an earlier version     
            L2=L2fix-L1(i)  !L2 is found using L2fix=L1(i)+L2
            pK=C1+(C2/(temp(i)+273))
            fN=(10**(-pH))/((10**(-pH)+10**(-pK)))

        !     fN=(-log(pH))/((-log(pH)+10**(-pK)))
	    !     d_bound=h(i)/2.0
	    !     L1(i) = -0.5*por*tor*h(i)/2. + 0.5*sqrt(((por*tor*h(i)/2.)**2)+(4*por*tor*DOw*ow(i))/omega)
!**********************************************************************************************************************
!**********************************************************************************************************************

            beta0  = fact*2*porw*por*tor/(porw*L2+por*tor*h(i))
            beta1  = fact*2*porw*por*tor/(porw*L1(i)+por*tor*h(i))  !beta1=beta1*diffusion (see below equations)
            beta2  = fact*2*por*tor/(L1(i)+L2)

            Vs(i)=Area(i)*(L1(i)+L2)
            Vs1(i)=Area(i)*L1(i)
            Vs2(i)=Area(i)*L2  !L2= depth of anaerobic layer

!**********************************************************************************************************************
!**********************************************************************************************************************
    pKc1c2    = pKc1c2    +pK
    Beta1Nw   = Beta1Nw   +beta1*Daw
    Beta1NO3w = Beta1NO3w +beta1*Dnw
    Beta1Pw   = Beta1Pw   +beta1*Dpw

	Beta1Cw = Beta1Cw + beta1*Dcw        ! Amir
	Beta1Ch4w = Beta1Ch4w + beta1*DCh4w  ! Amir

    koko=koko+ko
    kvkv=kvkv+kv
    fNfN=fNfN+fN
!**********************************************************************************************************************
!**********************************************************************************************************************

            !These coefficients are determined for the ground water
            If (Qg(i)>0) then
                Coef0=1
                Coef1=0
            Else
                Coef0=0
                Coef1=1
            End if

            !These section is for the phosphorus
            Fdw=1/(1+Kw*mw(i))
            Fsw=Kw*Fdw

            Ks1= Ksa+Ksb*min(1.,Ow(i)/Osat)    !Ks2+Kstar*tanh(L1(i)/L2)
            Ks2=Ksa

            Fds=1/(por+(1-por)*rows*Ks1)
            Fss=Ks1*Fds1
            Fds1=1/(por+(1-por)*rows*Ks2)
            Fss1=Ks2*Fds1
            kp1= 2*(tor*Dpw*L1(i)+Dpw*d_bound)/(d_bound+L1(i))**2
            kp2= 2*tor*Dpw/(L1(i)+d_bound)

			velr_o=min(velr_o,(Vs(i)/(porw*Area(i)*dt)+vels_o*ONw(i)/(ONss(i)+ONsf(i))))
			vrvelr_o=vrvelr_o+velr_o
            !End general calculations
!**********************************************************************************************************************
END SUBROUTINE
!**********************************************************************************************************************



!**********************************************************************************************************************
 SUBROUTINE GeneralCalc2
!**********************************************************************************************************************
   Use Parm ; Use Pprime
!**********************************************************************************************************************
             !This is the part we update fN values
           TN = Nw(i+1)+Ns1(i+1)+Ns2(i+1)+NO3w(i+1)+NO3s1(i+1)+NO3s2(i+1)
           fNw=(fNup*Nw(i+1))/TN       ; fNO3w=(NO3w(i+1))/TN
           fNs1=(fNup*Ns1(i+1))/TN     ; fNO3s1=(NO3s1(i+1))/TN
           fNs2=(fNup*Ns2(i+1))/TN     ; fNO3s2=(NO3s2(i+1))/TN

!**********************************************************************************************************************
  END SUBROUTINE
!**********************************************************************************************************************

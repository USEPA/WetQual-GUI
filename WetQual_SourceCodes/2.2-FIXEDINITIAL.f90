!**********************************************************************************************************************
! This subroutine reads the basic model parameters, initial concentrations of nutrients,
!                       hydro-climate and input concentration time series.
!
!Basic Parameters (�10_basic_parameters.txt� file)
!Symbol	::Definition (Units)
!dt	    ::Time step of simulation (day)
!n	    ::Number of days of simulation
!ronn	::Gram of oxygen consumed per gram of total ammonium nitrogen nitrified (gO/gN)
!rond	::Gram of oxygen consumed per gram of organic nitrogen mineralized(gO/gN)
!roc	::Gram of oxygen produced per gram of organic carbon synthesized
!sims	::Number of Monte Carlo (MC) simulations
!fNw	::Fraction of mineral nitrogen plant uptake as nitrate-N in free water
!fNs1	::Fraction of mineral nitrogen plant uptake as nitrate-N in the aerobic layer
!fNs2	::Fraction of mineral nitrogen plant uptake as nitrate-N in the anaerobic layer
!fNO3w	::Fraction of mineral nitrogen plant uptake as nitrate-N in free water
!fNO3s1	::Fraction of mineral nitrogen plant uptake as nitrate-N in the aerobic layer
!fNO3s2	::Fraction of mineral nitrogen plant uptake as nitrate-N in the anaerobic layer
!w	    ::Angular velocity of earth (15�/h, or ?/12 rad/h)
!apn	::Phosphorus to nitrogen mass ratio produced by mineralization of particulate organic matter (POM)
!lat	::latitude in radians
!d_bound::the thickness of a laminar (diffusive) boundary layer situated on top of the soil-water interface (cm)
!amc	::Stoichiometric yield of Methane from the anaerobic decomposition of gram of organic carbon during methanogenesis (gr/gr)
!lamdaR	::Specific conductivity of root system (m root m-3 soil)
!dn	    ::Day number of the year (day), Julian day
!*******************************************************************
!*******************************************************************
!Initial Concentration (�11_initial_concentration.txt� file)
!Symbol	::Definition  (Units)
!Onw	::Particulate organic nitrogen concentration in free water (mg/L)
!Onss	::Concentration of refractory organic nitrogen in wetland soil (mg/L)
!Onsf	::Concentration of labile organic nitrogen in wetland soil (mg/L)
!Nw	    ::Total ammonia-nitrogen ([NH4+] + [NH3]) concentration in free water (mg/L)
!Ns1	::Total ammonia-nitrogen pore-water concentration in upper aerobic layer (mg/L)
!Ns2	::Total ammonia-nitrogen pore-water concentration in lower anaerobic layer (mg/L)
!NO3w	::Nitrate-nitrogen concentration in free water (mg/L)
!NO3s1	::Nitrate-nitrogen pore-water concentration in upper aerobic layer (mg/L)
!NO3s2	::Nitrate-nitrogen pore-water concentration in lower anaerobic layer (mg/L)
!a	    ::Mass of free floating plant (gr chlorophyll a)
!b	    ::Mass of rooted plants (gr chlorophyll a)
!mw	    ::Sediment concentration in free water (mg/L)
!ms	    ::Wetland soil bulk density (mg/L)
!Ow	    ::Oxygen concentration in free water (mg/L)
!Pw	    ::Total inorganic phosphorus concentration in free water (mg/L)
!Ps1	::Total phosphorus concentration in aerobic layer (mg/L)
!Ps2	::Total phosphorus concentration in anaerobic layer (mg/L)
!DOCw	::Concentrations of dissolved organic C in free water (mg/L)
!LPOCw	::Concentrations of labile (fast reacting) particulate organic C in free water (mg/L)
!RPOCw	::Concentrations of refractory (slow reacting) particulate organic C in free water (mg/L)
!DOCs1	::Pore water concentrations of DOC in aerobic sediment layer (mg/L)
!LPOCs1	::Pore water concentrations of LPOC in aerobic sediment layer (mg/L)
!RPOCs1	::Pore water concentrations of RPOC in aerobic sediment layer (mg/L)
!DOCs2	::Pore water concentrations of DOC in lower anaerobic sediment layer (mg/L)
!LPOCs2	::Pore water concentrations of LPOC in lower anaerobic sediment layer (mg/L)
!RPOCs2	::Pore water concentrations of RPOC in lower anaerobic sediment layer (mg/L)
!CH4w	::Methane concentration in free water (mg/L)
!CH4s1	::Methane concentration in aerobic sediment layer (mg/L)
!CH4s2	::Methane concentration in anaerobic sediment layer (mg/L)
!*******************************************************************
!*******************************************************************
!Hydro-Climate Parameters (�12_hydro_climate.txt� file)
!Symbol	::Definition, Units
!Qin	::Volumetric inflow rate (m3/day)
!Qout	::Wetland discharge (outflow) rate (m3/day)
!Vw	    ::Water volume of wetland surface water (m3)
!A	    ::wetland surface area (m2)
!ET	    ::Evapotranspiration rate (cm/day)
!ip	    ::Precipitation rate (cm/day)
!Qg	    ::Groundwater discharge (negative for infiltration) (m3/day)
!Uw	    ::Wind speed (m/s)
!Tair   ::Daily air temperature (�C)
!Twater	::Daily water temperature (�C)
!*******************************************************************
!*******************************************************************
!Input Concentrations (�13_input_concentrations.txt� file)
!Symbol	::Definition (Units)
!ONin	::Organic nitrogen concentration in incoming flow (mg/L)
!NO3in	::Nitrate-nitrogen concentration in incoming flow (mg/L)
!NWin	::Total ammonia-nitrogen ([NH4+] + [NH3]) concentration in incoming flow (mg/L)
!NO3g	::Nitrate-nitrogen concentration in groundwater discharge (mg/L)
!Ng	    ::Total ammonia-nitrogen concentration in groundwater discharge (mg/L)
!Owin	::Oxygen concentration in incoming flow (mg/L)
!PO4in	::Phosphate concentration in incoming flow (mg/L)
!Pg	    ::Total phosphorus concentration in groundwater discharge (mg/L)
!mwin	::Sediment concentration in incoming flow (mg/L)
!NH4air	::Ammonium concentration in precipitation (mg/L)
!NO3air	::Nitrate-nitrogen concentrations in precipitation (mg/L)
!Qa	    ::Dry depositional rates of total ammonia nitrogen (mg/m2/day)
!Qn	    ::Dry depositional rates of total nitrate-nitrogen (mg/m2/day)
!LPOCin	::Labile particulate organic carbon concentration in incoming flow (mg/L)
!RPOCin	::Refractory particulate organic carbon concentration in incoming flow (mg/L)
!DOCin	::Dissolved organic carbon concentration in incoming flow (mg/L)
!DOCatm	::Atmospheric deposition for total organic carbon (mg/m2/day)
!TOCgw	::Total organic carbon concentration in groundwater discharge (mg/L)


!**********************************************************************************************************************
subroutine FixedInitial
!**********************************************************************************************************************

USE parm
use pprime

IMPLICIT NONE

INTEGER :: ii,j,k,ni

real temp_air,raindist1
character *10  datedummy
!**********************************************************************************************************************
!****     !Basic or initial parameters are being read from the file                                  *******
!**********************************************************************************************************************
    open (10,file=fixedparams,status="old")
    read (10,*) text
    read (10,*) dt,n,ronn,rond,roc,sims
    !read (10,*) dt,n,ronn,rond,roc,lat,d_bound,sims
    !SI::,sims
    read (10,*) text
    !read (10,*) fNw,fNs1,fNs2,fNO3w,fNO3s1,fNO3s2,w,apn,lat,d_bound
    read (10,*) fNw,fNs1,fNs2,fNO3w,fNO3s1,fNO3s2

    !read (10,*) fNw,fNs1,fNs2,fNO3w,fNO3s1,fNO3s2,apn
    !������  A�ir  ���������
    read (10,*) text !units
    read (10,*) apn,lat,amc , lamdaR
    read (10,*) text
    do ii=0,n
        read (10,*) dn(ii)
    end do
    close(10)
!***********************************************************************
           w=0.2618        ! Angular velocity of earth (15(0C)/h, or pi/12 rad/h)
!***********************************************************************
           d_bound=0
!***********************************************************************

!**********************************************************************************************************************
            !This is to read the initial concentrations and then convert to g/cm3

    open (11,file=initialconc,status="old")
    read (11,*) text
    read (11,*) text
    read (11,*) ONw(0),ONsf(0),ONss(0),Nw(0),Ns1(0),Ns2(0),NO3w(0),NO3s1(0),NO3s2(0)
    read (11,*) text
    read (11,*) a(0),b(0)
    read (11,*) text
    read (11,*) mw(0), ms(0)
    read (11,*) text
    read (11,*) Ow(0),Pw(0),Ps1(0),Ps2(0)
    !������  A�ir  ������
    read (11,*) text
    read (11,*) text
    read (11,*) DOCw(0),LPOCw(0),RPOCw(0),DOCs1(0),LPOCs1(0),RPOCs1(0),DOCs2(0),LPOCs2(0),RPOCs2(0),Ch4w(0),Ch4s1(0),ch4s2(0)

    close(11)

!**********************************************************************************************************************
!****            open generated parameters files  ! SI:                                                                    *******
!**********************************************************************************************************************
    !This file includes the generated parameter sets
    open (14,file=generatedparms,status="old")
    read (14,*) text   ! SI: title was inserted to the file

    !���������������������  A�ir  ����������������������
    open (15,file=generatedparmcarbon,status="old")
    read (15,*) text
    read (15,*) text
    !���������������������������������������������������

!**********************************************************************************************************************
!****            Load initial parameters for new output files  ! SI:                                                                    *******
!**********************************************************************************************************************
           ZOnw(0)=Onw(0)           ; !���������������������  A�ir  ����������������������
           ZOnss(0)=Onss(0)         ; ZDOCw(0)=DOCw(0)
           ZOnsf(0)=Onsf(0)         ; ZLPOCw(0)=LPOCw(0)
           ZNw(0)=Nw(0)             ; ZRPOCw(0)=RPOCw(0)
           ZNs1(0)=Ns1(0)           ; ZTOCw(0)=ZDOCw(0)+ZLPOCw(0)+ZRPOCw(0)
           ZNs2(0)=Ns2(0)           ; ZDOCs1(0)=DOCs1(0)
           ZNO3w(0)=NO3w(0)         ; ZLPOCs1(0)=LPOCs1(0)
           ZNO3s1(0)=NO3s1(0)       ; ZRPOCs1(0)=RPOCs1(0)
           ZNO3s2(0)=NO3s2(0)       ; ZDOCs2(0)=DOCs2(0)
           ZOw(0)=Ow(0)             ; ZLPOCs2(0)=LPOCs2(0)
           Za(0)=a(0)               ; ZRPOCs2(0)=RPOCs2(0)
           Zb(0)=b(0)               ; ZCH4w(0)=CH4w(0)
           ZPw(0)=Pw(0)             ; ZCH4s1(0)=CH4s1(0)
           ZPs1(0)=Ps1(0)           ; ZCH4s2(0)=CH4s2(0)
           ZPs2(0)=Ps2(0)           ; ZVw(0)=Vw(0)/1e6   ! this line is just for an test output file. can be eliminated later
           Zms(0)=ms(0)
           Zmw(0)=mw(0)
!*************************************!���������������������������������������������������*********************************************************************************

            ONw(0)=ONw(0)*1e-6		!(mg/L to g/cm3 conversion)
            ONsf(0)= ONsf(0)* 1e-6	!(mg/L to g/cm3 conversion)
            ONss(0)= ONss(0)*1e-6	!(mg/L to g/cm3 conversion)
            Nw(0)=Nw(0)*1e-6		!(mg/L to g/cm3 conversion)
            Ns1(0)=Ns1(0)*1e-6		!(mg/L to g/cm3 conversion)
            Ns2(0)=Ns2(0)*1e-6		!(mg/L to g/cm3 conversion)
            NO3w(0)=NO3w(0)*1e-6	!(mg/L to g/cm3 conversion)
            NO3s1(0)=NO3s1(0)*1e-6  !(mg/L to g/cm3 conversion)
            NO3s2(0)=NO3s2(0)*1e-6  !(mg/L to g/cm3 conversion)
            Ow(0)=Ow(0)*1e-6		!(mg/L to g/cm3 conversion)
            Pw(0)=Pw(0)*1e-6
            Ps1(0)=Ps1(0)*1e-6
            Ps2(0)=Ps2(0)*1e-6
	        mw(0)=mw(0)*1e-6
	        ms(0)=ms(0)
            bofixed=b(0)

           	!���������������������  A�ir  ����������������������
			DOCw(0)=DOCw(0)*1e-6             !(mg/L to g/cm3 conversion)
			LPOCw(0)=LPOCw(0)*1e-6           !(mg/L to g/cm3 conversion)
			RPOCw(0)=RPOCw(0)*1e-6           !(mg/L to g/cm3 conversion)
			TOCw(0)=DOCw(0)+LPOCw(0)+RPOCw(0)!
			DOCs1(0)=DOCs1(0)*1e-6           !(mg/L to g/cm3 conversion)
			LPOCs1(0)=LPOCs1(0)*1e-6         !(mg/L to g/cm3 conversion)
			RPOCs1(0)=RPOCs1(0)*1e-6         !(mg/L to g/cm3 conversion)
			DOCs2(0)=DOCs2(0)*1e-6           !(mg/L to g/cm3 conversion)
			LPOCs2(0)=LPOCs2(0)*1e-6         !(mg/L to g/cm3 conversion)
			RPOCs2(0)=RPOCs2(0)*1e-6         !(mg/L to g/cm3 conversion)
			CH4w(0)=CH4w(0)*1e-6
			CH4s1(0)=CH4s1(0)*1e-6
			CH4s2(0)=CH4s2(0)*1e-6
			!���������������������������������������������������

!**********************************************************************************************************************
        ! Reading reservour and time dependent parameters (hydrologic_parameters)
!**********************************************************************************************************************
    open (12,file=hydro_climparams,status="old")
    read (12,*) text   ! SI: title was inserted to the file
    read (12,*) text   ! SI: title was inserted to the file
    ii=0
    ni=0
    do while(ii<(int((n)/dt+dt/2)+1))
    !do i=0,int((n)/dt+0.01),int(1/dt+0.01)
        !read (12,*) Qin(ii),Qout(ii),Vw(ii),Area(ii),ET(ii),ip(ii),Qg(ii),Uw(ii)temp(ii)
        read (12,'(A10)',advance='no') datedummy
        read (12,*)Qin(ii),Qout(ii),Vw(ii),Area(ii),ET(ii),ip(ii),Qg(ii),Uw(ii),temp_air,temp(ii)
        H(ii)=Vw(ii)/Area(ii)

        ni=ni+1
        ii=int(ni/dt+dt/2)
    end do
    CLOSE(12)
!**********************************************************************************************************************
!**********************************************************************************************************************
    open (13,file=timedepparms,status="old")
    read (13,*) text   ! SI: title was inserted to the file
    read (13,*) text   ! SI: title was inserted to the file
        ii=0
        ni=0
    do while(ii<(int(real(n)/dt+dt/2)+1))
    !do i=0,int((n)/dt+0.01) ,int(1/dt+0.01)

        read (13,'(A10)',advance='no') datedummy
        read (13,*) ONin(ii),NO3in(ii),Nwin(ii),NO3g(ii),Ng(ii),Owin(ii),Pin(ii),Pg(ii), mwin(ii),Nair(ii),NO3Air(ii),Qa(ii),Qn(ii), LPOCin(ii) , RPOCin(ii) , DOCin(ii), DOCatm(ii),TOCgw(ii)

        ni=ni+1
        ii=int(ni/dt+dt/2)
    end do
    CLOSE(13)
!**********************************************************************************************************************
!**********************************************************************************************************************
    !This is the part we do unit conversion
    ii=0
    ni=0
    do while(ii<(int(real(n)/dt+dt/2)+1))
    !do ii=0,int((n)/dt+0.01),int(1/dt+0.01) ! 0 to 73400 step 100
        !hydro-climate
        Qin(ii)=Qin(ii)*100*100*100   !(m3/day to cm3/day conversion)
        Qout(ii)=Qout(ii)*100*100*100 !(m3/day to cm3/day conversion)

        Area(ii)=Area(ii)*10000       !(m2 to cm2 conversion)
        Vw(ii)=Vw(ii)*100*100*100     !(m3 to cm3 conversion)
        H(ii)=(H(ii))*100             !(m to cm conversion)

        ET(ii)=ET(ii)*Area(ii)         !(cm/day to cm3/day)
        Qg(ii)=Qg(ii)*100*100*100     !(m3/day to cm3/day conversion)

        !input concentrations
        ONin(ii)=ONin(ii)* 1e-6       !(mg/L to g/cm3 conversion)
        NO3in(ii)=NO3in(ii)* 1e-6     !(mg/L to g/cm3 conversion)
        Nwin(ii)=Nwin(ii)* 1e-6       !(mg/L to g/cm3 conversion)
        NO3g(ii)=NO3g(ii)* 1e-6       !(mg/L to g/cm3 conversion)
        Ng(ii)=Ng(ii)* 1e-6           !(mg/L to g/cm3 conversion)
        Owin(ii)=Owin(ii)*1e-6
        Pin(ii)=Pin(ii)*1e-6
        Pg(ii)=Pg(ii)*1e-6
        mwin(ii)=mwin(ii)*1e-6
        Nair(ii)=Nair(ii)* 1e-6           !SI
        NO3Air(ii)=NO3Air(ii)* 1e-6       !SI

        !���������������������  A�ir  ����������������������
		!	TOCin(ii)=TOCin(ii)*1e-6		 !(mg/L to g/cm3 conversion)
        LPOCin(ii)=LPOCin(ii)*1e-6		 !(mg/L to g/cm3 conversion)
        RPOCin(ii)=RPOCin(ii)*1e-6		 !(mg/L to g/cm3 conversion)
        DOCin(ii)=DOCin(ii)*1e-6		 !(mg/L to g/cm3 conversion)

        DOCatm(ii)=DOCatm(ii)*1e-6    !(mg/L to g/cm3 conversion)
        TOCgw(ii)=TOCgw(ii)*1e-6		!(mg/L to g/cm3 conversion)

        !���������������������������������������������������
        ni=ni+1
        ii=int(real(ni)/dt+dt/2)
!**********************************************************************************************************************
    end do
!**********************************************************************************************************************
    call SCS
!******************************************************************************
    raindist1=0

!**********************************************************************************************************************
    !Interpolation
    do ii=0,int(real(n)/dt+dt/2) !,int(1/dt+0.01)!,int(1./dt+.00000001) !,int(1./delt+.00000001)+.000001
!**********************************************************************************************************************
                j=int(int(real(ii)*dt+dt/2)/dt+dt/2)   !lower bound of interpolation
 	            k= int((int(real(ii)*dt+dt/2)+1)/dt+dt/2)            !j+int(1/dt+dt/2)             !upper bound of interpolation

!**********************************************************************************************************************
            dis_percent= real(ii)/real(int(1/dt+dt/2))-int(real(ii)/real(int(1/dt+dt/2)))

            call rainfall_distribution


            if (rainfalldist<raindist1)then
                raindist1=0
            end if
               ip(ii)=(rainfalldist-raindist1)*ip(k)*100
               raindist1=rainfalldist
!**************************************************************
            pday=abs(ii-j)*dt
            IF (pday.LT.(0.264*1.0).OR.pday.GT.(0.736*1.0)) THEN    !Hydrus Manual Eq.2.75 in page 38
                ET(ii)=0.24*ET(k)
            ELSE
                ET(ii)=2.75*ET(k)*sin(2*3.1416*pday/1.0-3.1416/2)
            END IF

                !ip(ii)=ip(k)*(1+cos(2*3.1416*pday/1.0-3.1416))        !Hydrus Manual Eq.2.76 in page 38
!**********************************************************************************************************************
 	          ! j=int(i/dt+0.0001)
 	          ! k=j+int(1/dt+0.0001)
!**********************************************************************************************************************
	            if (ii /= j) then
	                !Hydro-climate data
                    Qin(ii)=Qin(j)+(Qin(k)-Qin(j))*real(ii-j)/real(k-j)
                    Qout(ii)=Qout(j)+(Qout(k)-Qout(j))*real(ii-j)/real(k-j)
                    Area(ii)=Area(j)+(Area(k)-Area(j))*real(ii-j)/real(k-j)
                    Vw(ii)=Vw(j)+(Vw(k)-Vw(j))*real(ii-j)/real(k-j)

                    H(ii)=H(j)+(H(k)-H(j))*real(ii-j)/real(k-j)
                    !ip(ii)=ip(j)+(ip(k)-ip(j))*real(ii-j)/real(k-j)
                    !ET(ii)=ET(j)+(ET(k)-ET(j))*real(ii-j)/real(k-j)
                    Qg(ii)=Qg(j)+(Qg(k)-Qg(j))*real(ii-j)/real(k-j)
                    Uw(ii)=Uw(j)+(Uw(k)-Uw(j))*real(ii-j)/real(k-j)   !SInew
                    temp(ii)=temp(j)+(temp(k)-temp(j))*real(ii-j)/real(k-j)


                    !Input concentrations
                    ONin(ii)=ONin(j)+(ONin(k)-ONin(j))*real(ii-j)/real(k-j)
                    NO3in(ii)=NO3in(j)+(NO3in(k)-NO3in(j))*real(ii-j)/real(k-j)
                    Nwin(ii)=Nwin(j)+(Nwin(k)-Nwin(j))*real(ii-j)/real(k-j)
                    NO3g(ii)=NO3g(j)+(NO3g(k)-NO3g(j))*real(ii-j)/real(k-j)
                    Ng(ii)=Ng(j)+(Ng(k)-Ng(j))*real(ii-j)/real(k-j)
                    Owin(ii)=Owin(j)+(Owin(k)-Owin(j))*real(ii-j)/real(k-j)
                    Pin(ii)=Pin(j)+(Pin(k)-Pin(j))*real(ii-j)/real(k-j)
                    Pg(ii)=Pg(j)+(Pg(k)-Pg(j))*real(ii-j)/real(k-j)
                    mwin(ii)=mwin(j)+(mwin(k)-mwin(j))*real(ii-j)/real(k-j)
                   ! kga(ii)=kga(j)+(kga(k)-kga(j))*real(ii-j)/real(k-j)
                   ! kgb(ii)=kgb(j)+(kgb(k)-kgb(j))*real(ii-j)/real(k-j)

                    Nair(ii)=Nair(j)+(Nair(k)-Nair(j))*real(ii-j)/real(k-j)       !SI
                    NO3Air(ii)=NO3Air(j)+(NO3Air(k)-NO3Air(j))*real(ii-j)/real(k-j)       !SI
                    Qa(ii)=Qa(j)+(Qa(k)-Qa(j))*real(ii-j)/real(k-j)       !SI
                    Qn(ii)=Qn(j)+(Qn(k)-Qn(j))*real(ii-j)/real(k-j)       !SI

                    !���������������������  A�ir  ����������������������
		        !	TOCin(ii)=TOCin(j)+(TOCin(k)-TOCin(j))*real(ii-j)/real(k-j)
			        LPOCin(ii)=LPOCin(j)+(LPOCin(k)-LPOCin(j))*real(ii-j)/real(k-j)
			        RPOCin(ii)=RPOCin(j)+(RPOCin(k)-RPOCin(j))*real(ii-j)/real(k-j)
			        DOCin(ii)=DOCin(j)+(DOCin(k)-DOCin(j))*real(ii-j)/real(k-j)
			        DOCatm(ii)=DOCatm(j)+(DOCatm(k)-DOCatm(j))*real(ii-j)/real(k-j)
			        TOCgw(ii)=TOCgw(j)+(TOCgw(k)-TOCgw(j))*real(ii-j)/real(k-j)
			        !���������������������������������������������������

	            end if
    end do
!**********************************************************************************************************************
! 	        open (200,file="200.txt",status="unknown")
! 	        open (201,file="201.txt",status="unknown")
! 	    DO ii=0,int((n/dt-1)+dt/2)
! 	        write (200,"(I10,9F15.5)")ii,Qin(ii)*1e-6,Qout(ii)*1e-6,Vw(ii)*1e-6,Area(ii)*1e-4,ET(ii)/Area(ii),ip(ii),Qg(ii)*1e-6,Uw(ii),temp(ii)
!
! 	        write (201,"(I10,18F12.3)")ii, ONin(ii)/1e-6 ,NO3in(ii)/1e-6,Nwin(ii)/1e-6,NO3g(ii)/1e-6,Ng(ii)/1e-6,&
! 	        &Owin(ii)/1e-6,Pin(ii)/1e-6,Pg(ii)/1e-6, mwin(ii)/1e-6,Nair(ii)/1e-6,NO3Air(ii)/1e-6,Qa(ii)/1e-6,&
! 	        &Qn(ii)/1e-6, LPOCin(ii)/1e-6 , RPOCin(ii)/1e-6 , DOCin(ii)/1e-6, DOCatm(ii)/1e-6,TOCgw(ii)/1e-6
!        end do
!**********************************************************************************************************************
END SUBROUTINE
!**********************************************************************************************************************

!**********************************************************************************************************************
SUBROUTINE SCS
!**********************************************************************************************************************
USE parm

 integer ii,kk,dis_type,timescale
 real tm1,dtdt,typeI,typeIa,typeII,typeIII
!**********************************************************************************************************************
 !distribution_type(1:typeI-2:typeIa-3:typeII-4:typeIII)
!dis_type=3
!time	dt	type I	type Ia	type II	type III
!(hours)	(hours)	24-hour	24-hour	24-hour	24-hour
real:: Dist24 (0:240,6)

data  (Dist24(0 ,j), j=1,6)/0,0,0,0,0,0/
data  (Dist24(1 ,j), j=1,6)/0.1,0.004167,0.00174,0.00224,0.00101,0.001/
data  (Dist24(2 ,j), j=1,6)/0.2,0.008333,0.00348,0.00432,0.00202,0.002/
data  (Dist24(3 ,j), j=1,6)/0.3,0.0125,0.00522,0.00628,0.00305,0.003/
data  (Dist24(4 ,j), j=1,6)/0.4,0.016667,0.00697,0.00816,0.00408,0.004/
data  (Dist24(5 ,j), j=1,6)/0.5,0.020833,0.00871,0.01,0.00513,0.005/
data  (Dist24(6 ,j), j=1,6)/0.6,0.025,0.01046,0.01184,0.00618,0.006/
data  (Dist24(7 ,j), j=1,6)/0.7,0.029167,0.0122,0.01372,0.00725,0.007/
data  (Dist24(8 ,j), j=1,6)/0.8,0.033333,0.01395,0.01568,0.00832,0.008/
data  (Dist24(9 ,j), j=1,6)/0.9,0.0375,0.0157,0.01776,0.00941,0.009/
data  (Dist24(10 ,j), j=1,6)/1,0.041667,0.01745,0.02,0.0105,0.01/
data  (Dist24(11 ,j), j=1,6)/1.1,0.045833,0.0192,0.02276,0.01161,0.011/
data  (Dist24(12 ,j), j=1,6)/1.2,0.05,0.02095,0.02568,0.01272,0.012/
data  (Dist24(13 ,j), j=1,6)/1.3,0.054167,0.0227,0.02872,0.01385,0.013/
data  (Dist24(14 ,j), j=1,6)/1.4,0.058333,0.02446,0.03184,0.01498,0.014/
data  (Dist24(15 ,j), j=1,6)/1.5,0.0625,0.02621,0.035,0.01613,0.015/
data  (Dist24(16 ,j), j=1,6)/1.6,0.066667,0.02797,0.03797,0.01728,0.016/
data  (Dist24(17 ,j), j=1,6)/1.7,0.070833,0.02972,0.04095,0.01845,0.017/
data  (Dist24(18 ,j), j=1,6)/1.8,0.075,0.03148,0.04394,0.01962,0.018/
data  (Dist24(19 ,j), j=1,6)/1.9,0.079167,0.03324,0.04695,0.02081,0.019/
data  (Dist24(20 ,j), j=1,6)/2,0.083333,0.035,0.05,0.022,0.02/
data  (Dist24(21 ,j), j=1,6)/2.1,0.0875,0.03677,0.05315,0.02321,0.02101/
data  (Dist24(22 ,j), j=1,6)/2.2,0.091667,0.03858,0.05633,0.02442,0.02203/
data  (Dist24(23 ,j), j=1,6)/2.3,0.095833,0.04041,0.05954,0.02565,0.02307/
data  (Dist24(24 ,j), j=1,6)/2.4,0.1,0.04227,0.06276,0.02688,0.02412/
data  (Dist24(25 ,j), j=1,6)/2.5,0.104167,0.04416,0.066,0.02813,0.02519/
data  (Dist24(26 ,j), j=1,6)/2.6,0.108333,0.04608,0.0692,0.02938,0.02627/
data  (Dist24(27 ,j), j=1,6)/2.7,0.1125,0.04803,0.0724,0.03065,0.02737/
data  (Dist24(28 ,j), j=1,6)/2.8,0.116667,0.05001,0.0756,0.03192,0.02848/
data  (Dist24(29 ,j), j=1,6)/2.9,0.120833,0.05201,0.0788,0.03321,0.02961/
data  (Dist24(30 ,j), j=1,6)/3,0.125,0.05405,0.082,0.0345,0.03075/
data  (Dist24(31 ,j), j=1,6)/3.1,0.129167,0.05611,0.08514,0.03581,0.03191/
data  (Dist24(32 ,j), j=1,6)/3.2,0.133333,0.05821,0.08829,0.03712,0.03308/
data  (Dist24(33 ,j), j=1,6)/3.3,0.1375,0.06033,0.09147,0.03845,0.03427/
data  (Dist24(34 ,j), j=1,6)/3.4,0.141667,0.06248,0.09471,0.03978,0.03547/
data  (Dist24(35 ,j), j=1,6)/3.5,0.145833,0.06466,0.098,0.04113,0.03669/
data  (Dist24(36 ,j), j=1,6)/3.6,0.15,0.06687,0.10147,0.04248,0.03792/
data  (Dist24(37 ,j), j=1,6)/3.7,0.154167,0.06911,0.10502,0.04385,0.03917/
data  (Dist24(38 ,j), j=1,6)/3.8,0.158333,0.07138,0.10862,0.04522,0.04043/
data  (Dist24(39 ,j), j=1,6)/3.9,0.1625,0.07367,0.11229,0.04661,0.04171/
data  (Dist24(40 ,j), j=1,6)/4,0.166667,0.076,0.116,0.048,0.043/
data  (Dist24(41 ,j), j=1,6)/4.1,0.170833,0.07835,0.11969,0.04941,0.04431/
data  (Dist24(42 ,j), j=1,6)/4.2,0.175,0.0807,0.12342,0.05084,0.04563/
data  (Dist24(43 ,j), j=1,6)/4.3,0.179167,0.08307,0.12721,0.05229,0.04697/
data  (Dist24(44 ,j), j=1,6)/4.4,0.183333,0.08545,0.13107,0.05376,0.04832/
data  (Dist24(45 ,j), j=1,6)/4.5,0.1875,0.08784,0.135,0.05525,0.04969/
data  (Dist24(46 ,j), j=1,6)/4.6,0.191667,0.09024,0.13901,0.05676,0.05107/
data  (Dist24(47 ,j), j=1,6)/4.7,0.195833,0.09265,0.1431,0.05829,0.05247/
data  (Dist24(48 ,j), j=1,6)/4.8,0.2,0.09507,0.14729,0.05984,0.05388/
data  (Dist24(49 ,j), j=1,6)/4.9,0.204167,0.09751,0.15159,0.06141,0.05531/
data  (Dist24(50 ,j), j=1,6)/5,0.208333,0.09995,0.156,0.063,0.05675/
data  (Dist24(51 ,j), j=1,6)/5.1,0.2125,0.10241,0.16059,0.06461,0.05821/
data  (Dist24(52 ,j), j=1,6)/5.2,0.216667,0.10487,0.1653,0.06624,0.05968/
data  (Dist24(53 ,j), j=1,6)/5.3,0.220833,0.10735,0.17011,0.06789,0.06117/
data  (Dist24(54 ,j), j=1,6)/5.4,0.225,0.10984,0.17501,0.06956,0.06267/
data  (Dist24(55 ,j), j=1,6)/5.5,0.229167,0.11234,0.18,0.07125,0.06419/
data  (Dist24(56 ,j), j=1,6)/5.6,0.233333,0.11485,0.18494,0.07296,0.06572/
data  (Dist24(57 ,j), j=1,6)/5.7,0.2375,0.11737,0.18999,0.07469,0.06727/
data  (Dist24(58 ,j), j=1,6)/5.8,0.241667,0.1199,0.19517,0.07644,0.06883/
data  (Dist24(59 ,j), j=1,6)/5.9,0.245833,0.12245,0.20049,0.07821,0.07041/
data  (Dist24(60 ,j), j=1,6)/6,0.25,0.125,0.206,0.08,0.072/
data  (Dist24(61 ,j), j=1,6)/6.1,0.254167,0.12761,0.21196,0.08181,0.07363/
data  (Dist24(62 ,j), j=1,6)/6.2,0.258333,0.13034,0.21808,0.08364,0.0753/
data  (Dist24(63 ,j), j=1,6)/6.3,0.2625,0.13317,0.22432,0.08549,0.07703/
data  (Dist24(64 ,j), j=1,6)/6.4,0.266667,0.1361,0.23064,0.08736,0.0788/
data  (Dist24(65 ,j), j=1,6)/6.5,0.270833,0.13915,0.237,0.08925,0.08063/
data  (Dist24(66 ,j), j=1,6)/6.6,0.275,0.1423,0.24285,0.09116,0.0825/
data  (Dist24(67 ,j), j=1,6)/6.7,0.279167,0.14557,0.24878,0.09309,0.08443/
data  (Dist24(68 ,j), j=1,6)/6.8,0.283333,0.14894,0.2549,0.09504,0.0864/
data  (Dist24(69 ,j), j=1,6)/6.9,0.2875,0.15241,0.26127,0.09701,0.08843/
data  (Dist24(70 ,j), j=1,6)/7,0.291667,0.156,0.268,0.099,0.0905/
data  (Dist24(71 ,j), j=1,6)/7.1,0.295833,0.15966,0.27517,0.10101,0.09263/
data  (Dist24(72 ,j), j=1,6)/7.2,0.3,0.16334,0.28287,0.10304,0.0948/
data  (Dist24(73 ,j), j=1,6)/7.3,0.304167,0.16706,0.29118,0.10509,0.09703/
data  (Dist24(74 ,j), j=1,6)/7.4,0.308333,0.17082,0.30019,0.10716,0.0993/
data  (Dist24(75 ,j), j=1,6)/7.5,0.3125,0.1746,0.31,0.10925,0.10163/
data  (Dist24(76 ,j), j=1,6)/7.6,0.316667,0.17842,0.33142,0.11136,0.104/
data  (Dist24(77 ,j), j=1,6)/7.7,0.320833,0.18226,0.35469,0.11349,0.10643/
data  (Dist24(78 ,j), j=1,6)/7.8,0.325,0.18614,0.37876,0.11564,0.1089/
data  (Dist24(79 ,j), j=1,6)/7.9,0.329167,0.19006,0.40255,0.11781,0.11143/
data  (Dist24(80 ,j), j=1,6)/8,0.333333,0.194,0.425,0.12,0.114/
data  (Dist24(81 ,j), j=1,6)/8.1,0.3375,0.19817,0.43936,0.12225,0.11666/
data  (Dist24(82 ,j), j=1,6)/8.2,0.341667,0.20275,0.45168,0.1246,0.11943/
data  (Dist24(83 ,j), j=1,6)/8.3,0.345833,0.20775,0.46232,0.12705,0.12232/
data  (Dist24(84 ,j), j=1,6)/8.4,0.35,0.21317,0.47164,0.1296,0.12532/
data  (Dist24(85 ,j), j=1,6)/8.5,0.354167,0.219,0.48,0.13225,0.12844/
data  (Dist24(86 ,j), j=1,6)/8.6,0.358333,0.22523,0.48904,0.135,0.13167/
data  (Dist24(87 ,j), j=1,6)/8.7,0.3625,0.23185,0.49752,0.13785,0.13502/
data  (Dist24(88 ,j), j=1,6)/8.8,0.366667,0.23885,0.50548,0.1408,0.13848/
data  (Dist24(89 ,j), j=1,6)/8.9,0.370833,0.24623,0.51296,0.14385,0.14206/
data  (Dist24(90 ,j), j=1,6)/9,0.375,0.254,0.52,0.147,0.14575/
data  (Dist24(91 ,j), j=1,6)/9.1,0.379167,0.26233,0.52664,0.1502,0.14956/
data  (Dist24(92 ,j), j=1,6)/9.2,0.383333,0.27139,0.53292,0.1534,0.15348/
data  (Dist24(93 ,j), j=1,6)/9.3,0.3875,0.28119,0.53888,0.1566,0.15752/
data  (Dist24(94 ,j), j=1,6)/9.4,0.391667,0.29173,0.54456,0.1598,0.16167/
data  (Dist24(95 ,j), j=1,6)/9.5,0.395833,0.303,0.55,0.163,0.16594/
data  (Dist24(96 ,j), j=1,6)/9.6,0.4,0.31942,0.55564,0.16628,0.17032/
data  (Dist24(97 ,j), j=1,6)/9.7,0.404167,0.34542,0.56116,0.16972,0.17482/
data  (Dist24(98 ,j), j=1,6)/9.8,0.408333,0.38784,0.56656,0.17332,0.17943/
data  (Dist24(99 ,j), j=1,6)/9.9,0.4125,0.46316,0.57184,0.17708,0.18416/
data  (Dist24(100 ,j), j=1,6)/10,0.416667,0.515,0.577,0.181,0.189/
data  (Dist24(101 ,j), j=1,6)/10.1,0.420833,0.5322,0.58198,0.18512,0.19402/
data  (Dist24(102 ,j), j=1,6)/10.2,0.425,0.5476,0.58685,0.18948,0.19928/
data  (Dist24(103 ,j), j=1,6)/10.3,0.429167,0.5612,0.59163,0.19408,0.20478/
data  (Dist24(104 ,j), j=1,6)/10.4,0.433333,0.573,0.59635,0.19892,0.21052/
data  (Dist24(105 ,j), j=1,6)/10.5,0.4375,0.583,0.601,0.204,0.2165/
data  (Dist24(106 ,j), j=1,6)/10.6,0.441667,0.59188,0.60576,0.2094,0.22272/
data  (Dist24(107 ,j), j=1,6)/10.7,0.445833,0.60032,0.61044,0.2152,0.22918/
data  (Dist24(108 ,j), j=1,6)/10.8,0.45,0.60832,0.61504,0.2214,0.23588/
data  (Dist24(109 ,j), j=1,6)/10.9,0.454167,0.61588,0.61956,0.228,0.24282/
data  (Dist24(110 ,j), j=1,6)/11,0.458333,0.623,0.624,0.235,0.25/
data  (Dist24(111 ,j), j=1,6)/11.1,0.4625,0.62982,0.62836,0.24268,0.25776/
data  (Dist24(112 ,j), j=1,6)/11.2,0.466667,0.63648,0.63264,0.25132,0.26644/
data  (Dist24(113 ,j), j=1,6)/11.3,0.470833,0.64298,0.63684,0.26092,0.27604/
data  (Dist24(114 ,j), j=1,6)/11.4,0.475,0.64932,0.64096,0.27148,0.28656/
data  (Dist24(115 ,j), j=1,6)/11.5,0.479167,0.6555,0.645,0.283,0.298/
data  (Dist24(116 ,j), j=1,6)/11.6,0.483333,0.66152,0.64889,0.30684,0.3143/
data  (Dist24(117 ,j), j=1,6)/11.7,0.4875,0.66738,0.65272,0.35436,0.3394/
data  (Dist24(118 ,j), j=1,6)/11.8,0.491667,0.67308,0.65651,0.43079,0.3733/
data  (Dist24(119 ,j), j=1,6)/11.9,0.495833,0.67862,0.66026,0.56786,0.416/
data  (Dist24(120 ,j), j=1,6)/12,0.5,0.684,0.664,0.663,0.5/
data  (Dist24(121 ,j), j=1,6)/12.1,0.504167,0.68925,0.66773,0.68196,0.584/
data  (Dist24(122 ,j), j=1,6)/12.2,0.508333,0.6944,0.67148,0.69864,0.6267/
data  (Dist24(123 ,j), j=1,6)/12.3,0.5125,0.69945,0.67527,0.71304,0.6606/
data  (Dist24(124 ,j), j=1,6)/12.4,0.516667,0.7044,0.6791,0.72516,0.6857/
data  (Dist24(125 ,j), j=1,6)/12.5,0.520833,0.70925,0.683,0.735,0.702/
data  (Dist24(126 ,j), j=1,6)/12.6,0.525,0.714,0.68665,0.74344,0.71344/
data  (Dist24(127 ,j), j=1,6)/12.7,0.529167,0.71865,0.69027,0.75136,0.72396/
data  (Dist24(128 ,j), j=1,6)/12.8,0.533333,0.7232,0.69386,0.75876,0.73356/
data  (Dist24(129 ,j), j=1,6)/12.9,0.5375,0.72765,0.69744,0.76564,0.74224/
data  (Dist24(130 ,j), j=1,6)/13,0.541667,0.732,0.701,0.772,0.75/
data  (Dist24(131 ,j), j=1,6)/13.1,0.545833,0.73625,0.70473,0.77796,0.75718/
data  (Dist24(132 ,j), j=1,6)/13.2,0.55,0.7404,0.70838,0.78364,0.76412/
data  (Dist24(133 ,j), j=1,6)/13.3,0.554167,0.74445,0.71198,0.78904,0.77082/
data  (Dist24(134 ,j), j=1,6)/13.4,0.558333,0.7484,0.71551,0.79416,0.77728/
data  (Dist24(135 ,j), j=1,6)/13.5,0.5625,0.75225,0.719,0.799,0.7835/
data  (Dist24(136 ,j), j=1,6)/13.6,0.566667,0.756,0.72245,0.8036,0.78948/
data  (Dist24(137 ,j), j=1,6)/13.7,0.570833,0.75965,0.72586,0.808,0.79522/
data  (Dist24(138 ,j), j=1,6)/13.8,0.575,0.7632,0.72926,0.8122,0.80072/
data  (Dist24(139 ,j), j=1,6)/13.9,0.579167,0.76665,0.73263,0.8162,0.80598/
data  (Dist24(140 ,j), j=1,6)/14,0.583333,0.77,0.736,0.82,0.811/
data  (Dist24(141 ,j), j=1,6)/14.1,0.5875,0.77329,0.73939,0.82367,0.81584/
data  (Dist24(142 ,j), j=1,6)/14.2,0.591667,0.77656,0.74277,0.82726,0.82057/
data  (Dist24(143 ,j), j=1,6)/14.3,0.595833,0.77981,0.74613,0.83079,0.82518/
data  (Dist24(144 ,j), j=1,6)/14.4,0.6,0.78304,0.74948,0.83424,0.82968/
data  (Dist24(145 ,j), j=1,6)/14.5,0.604167,0.78625,0.75281,0.83763,0.83406/
data  (Dist24(146 ,j), j=1,6)/14.6,0.608333,0.78944,0.75613,0.84094,0.83833/
data  (Dist24(147 ,j), j=1,6)/14.7,0.6125,0.79261,0.75943,0.84419,0.84248/
data  (Dist24(148 ,j), j=1,6)/14.8,0.616667,0.79576,0.76271,0.84736,0.84652/
data  (Dist24(149 ,j), j=1,6)/14.9,0.620833,0.79889,0.76598,0.85047,0.85044/
data  (Dist24(150 ,j), j=1,6)/15,0.625,0.802,0.76924,0.8535,0.85425/
data  (Dist24(151 ,j), j=1,6)/15.1,0.629167,0.80509,0.77248,0.85647,0.85794/
data  (Dist24(152 ,j), j=1,6)/15.2,0.633333,0.80816,0.77571,0.85936,0.86152/
data  (Dist24(153 ,j), j=1,6)/15.3,0.6375,0.81121,0.77892,0.86219,0.86498/
data  (Dist24(154 ,j), j=1,6)/15.4,0.641667,0.81424,0.78211,0.86494,0.86833/
data  (Dist24(155 ,j), j=1,6)/15.5,0.645833,0.81725,0.78529,0.86763,0.87156/
data  (Dist24(156 ,j), j=1,6)/15.6,0.65,0.82024,0.78845,0.87024,0.87468/
data  (Dist24(157 ,j), j=1,6)/15.7,0.654167,0.82321,0.7916,0.87279,0.87768/
data  (Dist24(158 ,j), j=1,6)/15.8,0.658333,0.82616,0.79474,0.87526,0.88057/
data  (Dist24(159 ,j), j=1,6)/15.9,0.6625,0.82909,0.79786,0.87767,0.88334/
data  (Dist24(160 ,j), j=1,6)/16,0.666667,0.832,0.80096,0.88,0.886/
data  (Dist24(161 ,j), j=1,6)/16.1,0.670833,0.83489,0.80405,0.88229,0.88858/
data  (Dist24(162 ,j), j=1,6)/16.2,0.675,0.83776,0.80712,0.88455,0.8911/
data  (Dist24(163 ,j), j=1,6)/16.3,0.679167,0.84061,0.81018,0.88679,0.89358/
data  (Dist24(164 ,j), j=1,6)/16.4,0.683333,0.84344,0.81322,0.889,0.896/
data  (Dist24(165 ,j), j=1,6)/16.5,0.6875,0.84625,0.81625,0.89119,0.89838/
data  (Dist24(166 ,j), j=1,6)/16.6,0.691667,0.84904,0.81926,0.89335,0.9007/
data  (Dist24(167 ,j), j=1,6)/16.7,0.695833,0.85181,0.82226,0.89549,0.90298/
data  (Dist24(168 ,j), j=1,6)/16.8,0.7,0.85456,0.82524,0.8976,0.9052/
data  (Dist24(169 ,j), j=1,6)/16.9,0.704167,0.85729,0.82821,0.89969,0.90738/
data  (Dist24(170 ,j), j=1,6)/17,0.708333,0.86,0.83116,0.90175,0.9095/
data  (Dist24(171 ,j), j=1,6)/17.1,0.7125,0.86269,0.8341,0.90379,0.91158/
data  (Dist24(172 ,j), j=1,6)/17.2,0.716667,0.86536,0.83702,0.9058,0.9136/
data  (Dist24(173 ,j), j=1,6)/17.3,0.720833,0.86801,0.83992,0.90779,0.91558/
data  (Dist24(174 ,j), j=1,6)/17.4,0.725,0.87064,0.84281,0.90975,0.9175/
data  (Dist24(175 ,j), j=1,6)/17.5,0.729167,0.87325,0.84569,0.91169,0.91938/
data  (Dist24(176 ,j), j=1,6)/17.6,0.733333,0.87584,0.84855,0.9136,0.9212/
data  (Dist24(177 ,j), j=1,6)/17.7,0.7375,0.87841,0.8514,0.91549,0.92298/
data  (Dist24(178 ,j), j=1,6)/17.8,0.741667,0.88096,0.85423,0.91735,0.9247/
data  (Dist24(179 ,j), j=1,6)/17.9,0.745833,0.88349,0.85704,0.91919,0.92638/
data  (Dist24(180 ,j), j=1,6)/18,0.75,0.886,0.85984,0.921,0.928/
data  (Dist24(181 ,j), j=1,6)/18.1,0.754167,0.88849,0.86262,0.92279,0.92959/
data  (Dist24(182 ,j), j=1,6)/18.2,0.758333,0.89096,0.86539,0.92455,0.93117/
data  (Dist24(183 ,j), j=1,6)/18.3,0.7625,0.89341,0.86815,0.92629,0.93273/
data  (Dist24(184 ,j), j=1,6)/18.4,0.766667,0.89584,0.87089,0.928,0.93428/
data  (Dist24(185 ,j), j=1,6)/18.5,0.770833,0.89825,0.87361,0.92969,0.93581/
data  (Dist24(186 ,j), j=1,6)/18.6,0.775,0.90064,0.87632,0.93135,0.93733/
data  (Dist24(187 ,j), j=1,6)/18.7,0.779167,0.90301,0.87901,0.93299,0.93883/
data  (Dist24(188 ,j), j=1,6)/18.8,0.783333,0.90536,0.88169,0.9346,0.94032/
data  (Dist24(189 ,j), j=1,6)/18.9,0.7875,0.90769,0.88435,0.93619,0.94179/
data  (Dist24(190 ,j), j=1,6)/19,0.791667,0.91,0.887,0.93775,0.94325/
data  (Dist24(191 ,j), j=1,6)/19.1,0.795833,0.91229,0.88963,0.93929,0.94469/
data  (Dist24(192 ,j), j=1,6)/19.2,0.8,0.91456,0.89225,0.9408,0.94612/
data  (Dist24(193 ,j), j=1,6)/19.3,0.804167,0.91681,0.89485,0.94229,0.94753/
data  (Dist24(194 ,j), j=1,6)/19.4,0.808333,0.91904,0.89744,0.94375,0.94893/
data  (Dist24(195 ,j), j=1,6)/19.5,0.8125,0.92125,0.90001,0.94519,0.95031/
data  (Dist24(196 ,j), j=1,6)/19.6,0.816667,0.92344,0.90257,0.9466,0.95168/
data  (Dist24(197 ,j), j=1,6)/19.7,0.820833,0.92561,0.90511,0.94799,0.95303/
data  (Dist24(198 ,j), j=1,6)/19.8,0.825,0.92776,0.90763,0.94935,0.95437/
data  (Dist24(199 ,j), j=1,6)/19.9,0.829167,0.92989,0.91014,0.95069,0.95569/
data  (Dist24(200 ,j), j=1,6)/20,0.833333,0.932,0.91264,0.952,0.957/
data  (Dist24(201 ,j), j=1,6)/20.1,0.8375,0.93409,0.91512,0.9533,0.95829/
data  (Dist24(202 ,j), j=1,6)/20.2,0.841667,0.93616,0.91759,0.95459,0.95958/
data  (Dist24(203 ,j), j=1,6)/20.3,0.845833,0.93821,0.92004,0.95588,0.96085/
data  (Dist24(204 ,j), j=1,6)/20.4,0.85,0.94024,0.92247,0.95716,0.96211/
data  (Dist24(205 ,j), j=1,6)/20.5,0.854167,0.94225,0.92489,0.95844,0.96336/
data  (Dist24(206 ,j), j=1,6)/20.6,0.858333,0.94424,0.92729,0.95971,0.9646/
data  (Dist24(207 ,j), j=1,6)/20.7,0.8625,0.94621,0.92968,0.96098,0.96582/
data  (Dist24(208 ,j), j=1,6)/20.8,0.866667,0.94816,0.93206,0.96224,0.96704/
data  (Dist24(209 ,j), j=1,6)/20.9,0.870833,0.95009,0.93442,0.9635,0.96824/
data  (Dist24(210 ,j), j=1,6)/21,0.875,0.952,0.93676,0.96475,0.96944/
data  (Dist24(211 ,j), j=1,6)/21.1,0.879167,0.95389,0.93909,0.966,0.97062/
data  (Dist24(212 ,j), j=1,6)/21.2,0.883333,0.95576,0.9414,0.96724,0.97179/
data  (Dist24(213 ,j), j=1,6)/21.3,0.8875,0.95761,0.9437,0.96848,0.97295/
data  (Dist24(214 ,j), j=1,6)/21.4,0.891667,0.95944,0.94598,0.96971,0.9741/
data  (Dist24(215 ,j), j=1,6)/21.5,0.895833,0.96125,0.94825,0.97094,0.97523/
data  (Dist24(216 ,j), j=1,6)/21.6,0.9,0.96304,0.9505,0.97216,0.97636/
data  (Dist24(217 ,j), j=1,6)/21.7,0.904167,0.96481,0.95274,0.97338,0.97747/
data  (Dist24(218 ,j), j=1,6)/21.8,0.908333,0.96656,0.95496,0.97459,0.97858/
data  (Dist24(219 ,j), j=1,6)/21.9,0.9125,0.96829,0.95717,0.9758,0.97967/
data  (Dist24(220 ,j), j=1,6)/22,0.916667,0.97,0.95936,0.977,0.98075/
data  (Dist24(221 ,j), j=1,6)/22.1,0.920833,0.97169,0.96154,0.9782,0.98182/
data  (Dist24(222 ,j), j=1,6)/22.2,0.925,0.97336,0.9637,0.97939,0.98288/
data  (Dist24(223 ,j), j=1,6)/22.3,0.929167,0.97501,0.96584,0.98058,0.98392/
data  (Dist24(224 ,j), j=1,6)/22.4,0.933333,0.97664,0.96797,0.98176,0.98496/
data  (Dist24(225 ,j), j=1,6)/22.5,0.9375,0.97825,0.97009,0.98294,0.98598/
data  (Dist24(226 ,j), j=1,6)/22.6,0.941667,0.97984,0.97219,0.98411,0.987/
data  (Dist24(227 ,j), j=1,6)/22.7,0.945833,0.98141,0.97428,0.98528,0.988/
data  (Dist24(228 ,j), j=1,6)/22.8,0.95,0.98296,0.97635,0.98644,0.98899/
data  (Dist24(229 ,j), j=1,6)/22.9,0.954167,0.98449,0.9784,0.9876,0.98997/
data  (Dist24(230 ,j), j=1,6)/23,0.958333,0.986,0.98044,0.98875,0.99094/
data  (Dist24(231 ,j), j=1,6)/23.1,0.9625,0.98749,0.98246,0.9899,0.99189/
data  (Dist24(232 ,j), j=1,6)/23.2,0.966667,0.98896,0.98447,0.99104,0.99284/
data  (Dist24(233 ,j), j=1,6)/23.3,0.970833,0.99041,0.98647,0.99218,0.99377/
data  (Dist24(234 ,j), j=1,6)/23.4,0.975,0.99184,0.98845,0.99331,0.9947/
data  (Dist24(235 ,j), j=1,6)/23.5,0.979167,0.99325,0.99041,0.99444,0.99561/
data  (Dist24(236 ,j), j=1,6)/23.6,0.983333,0.99464,0.99236,0.99556,0.99651/
data  (Dist24(237 ,j), j=1,6)/23.7,0.9875,0.99601,0.99429,0.99668,0.9974/
data  (Dist24(238 ,j), j=1,6)/23.8,0.991667,0.99736,0.99621,0.99779,0.99828/
data  (Dist24(239 ,j), j=1,6)/23.9,0.995833,0.99869,0.99811,0.9989,0.99914/
data  (Dist24(240 ,j), j=1,6)/24,1,1,1,1,1/

!**********************************************************************************************************************

        ! real tm1,dtdt,typeI,typeIa,typeII,typeIII,RainDist(0:240,6)
        !dis_type=3
        !timescale=2 !(1=Hourly;   2=Daily)
        !ndisdata(1)=10
        !ndisdata(2)=240
!        open (2,file="2_SCSRainfallDistributions.txt",status="old")
!        read (2,*) text   ! SI: title was inserted to the file
!        read (2,*) dis_type

!        read (2,*) text   ! SI: title was inserted to the file
!        read (2,*) text   ! SI: title was inserted to the file
 !       select case (timescale)
!******************************************************************************
!        CASE (1)
!******************************************************************************
!            ndisdata=10
!            do ii=0,ndisdata
!                read(2,*)tm1,dtdt,typeI,typeIa,typeII,typeIII
!                RainDist(ii,1)=dtdt
!                SELECT CASE (dis_type)
!                    CASE (1)
!                        RainDist(ii,2)=typeI
!                    CASE (2)
!                        RainDist(ii,2)=typeIa
!                    CASE (3)
!                        RainDist(ii,2)=typeII
!                    CASE (4)
!                        RainDist(ii,2)=typeIII
!                    END SELECT
!            end do
!******************************************************************************
!        CASE(2)
!******************************************************************************
!            ndisdata=10
!            do kk=1,13
!                read (2,*) text
!            end do
   dis_type=3
            do k=0,240
                !read(2,*)tm1,dtdt,typeI,typeIa,typeII,typeIII
                RainDist(k,1)=Dist24(k,2)   !dtdt
                SELECT CASE (dis_type)
                    CASE (1)
                        RainDist(k,2)=Dist24(k,3)   !typeI
                    CASE (2)
                        RainDist(k,2)=Dist24(k,4)   !typeIa
                    CASE (3)
                        RainDist(k,2)=Dist24(k,5)   !typeII
                    CASE (4)
                        RainDist(k,2)=Dist24(k,6)   !typeIII
                END SELECT
            end do
!******************************************************************************
!        END SELECT
!******************************************************************************
     !           RainDist(0,1)=0.0001*dt
!**********************************************************************************************************************
END SUBROUTINE SCS
!**********************************************************************************************************************


!**********************************************************************************************************************
SUBROUTINE rainfall_distribution
!**********************************************************************************************************************
use parm
!**********************************************************************************************************************
integer jj
real slope1 !dis_perc,
    !dis_perc= real(ii)/real(int(1/dt+dt/2))-int(real(ii)/real(int(1/dt+dt/2)))
    do jj=0,239
        if(dis_percent>=RainDist(jj,1).and.dis_percent<RainDist(jj+1,1))  then
                slope1=(RainDist(jj+1,2)-RainDist(jj,2))/(RainDist(jj+1,1)-RainDist(jj,1))
                rainfalldist=RainDist(jj,2)+slope1*(dis_percent-RainDist(jj,1)) 
        else if(dis_percent==RainDist(240,1)) then
                rainfalldist=RainDist(240,2)
        end if
    end do
!**********************************************************************************************************************
END SUBROUTINE rainfall_distribution
!**********************************************************************************************************************

!**********************************************************************************************************************
! These subroutines in this file initialize some parameters and check the calculated outputs for non-negativity.
!**********************************************************************************************************************

!**********************************************************************************************************************
SUBROUTINE equal0
!**********************************************************************************************************************

use parm ; Use Pprime

!**********************************************************************************************************************
!**********************************************************************************************************************
    SumVwONw  = 0;    SumVwNw  = 0;  SumVwNO3w  = 0;    SumVwOw = 0;  SumVwPw  = 0; SumVwmw = 0;  
    SumVwONss = 0;    SumVwNs1 = 0;  SumVwNO3s1 = 0;    SumVwa  = 0;  SumVwPs1 = 0; SumVwms = 0;
    SumVwONsf = 0;    SumVwNs2 = 0;  SumVwNO3s2 = 0;    SumVwb  = 0;  SumVwPs2 = 0; 
    SumVw     = 0;    SumVs    = 0;  SumVs1     = 0;    SumVs2  = 0;
    Sum1Vw    = 0;    Sum1Vs   = 0;  Sum1Vs1    = 0;    Sum1Vs2 = 0;
    Sum2Vw    = 0;    Sum2Vs   = 0;  Sum2Vs1    = 0;    Sum2Vs2 = 0;
    SumVwNset = 0;    SumVwNvol= 0;  SumVwNO3den= 0;    SumVwPwset  = 0;  SumVwmwset = 0;
    SumVwPdif = 0;    SumVwPmin = 0; SumVwNdif  = 0;    SumVwNO3dif = 0;
    
    !еееееееееееееее  A╡ir  ееееееееееееееее
	 SUMVwTOCw=0
	 sumvwch4w=0; sumvs1ch4s1=0 ; sumvs2ch4s2=0  ; 

    SumVwLPOCw=0   ;   SumVwRPOCw=0     ;SumLPOCset =0; SumDOCmeth =0
    SumVwDOCw=0    ;   SumVs1LPOCs1=0   ;SumRPOCset =0; SumplantC  =0 
    SumVs1RPOCs1=0 ;   SumVs1DOCs1=0    ;SumCoutw   =0; summethane = 0
    SumVs2LPOCs2=0 ;   SumVs2RPOCs2=0   ;SumDOCdif  =0;
    SumVs2DOCs2=0  ;   SumVwTOCw=0      ;SumDOCresp =0;
    Sumf1=0        ;   Sumf2=0          ;SumDOCden  =0;
    
 !**********************************************************************************************************************
    Y1_Nset=0;   Y1_Nvol=0;   Y1_NO3den=0;     Y1_Pwset=0;      Y1_mwset=0;      Y1_MNw=0;    Y1_Pdif=0;   Y1_Pmin=0;   
    Y1_Ndif=0;   Y1_NO3dif=0; Y1_ONw=0;        Y1_ONsf=0;       Y1_ONss=0;       Y1_Nw=0;     Y1_Ns1=0;    Y1_Ns2=0;
    Y1_Ow=0;     Y1_NO3w=0;   Y1_NO3s1=0;      Y1_NO3s2=0;      Y1_Pw=0;         Y1_Ps1=0;    Y1_Ps2=0;    Y1_a=0; 
    Y1_b=0;      Y1_mw=0;     Y1_ms=0;         YY1_ONw =0;      YY1_Nw =0;       YY1_NO3w =0; YY1_Pw=0;   YY1_mw =0;
    Y2_Nset=0;   Y2_Nvol=0;   Y2_NO3den=0;     Y2_Pwset=0;      Y2_mwset=0;      Y2_MNw=0;    Y2_Pdif=0;   Y2_Pmin=0;
    Y2_Ndif=0;   Y2_NO3dif=0; Y2_ONw=0;        Y2_ONsf=0;       Y2_ONss=0;       Y2_Nw=0;     Y2_Ns1=0;    Y2_Ns2=0;
    Y2_Ow=0;     Y2_NO3w=0;   Y2_NO3s1=0;      Y2_NO3s2=0;      Y2_Pw=0;         Y2_Ps1=0;    Y2_Ps2=0;    Y2_a=0; 
    Y2_b=0;      Y2_mw=0;     Y2_ms=0;         YY2_ONw =0;      YY2_Nw =0;       YY2_NO3w =0; YY2_Pw=0;   YY2_mw =0;
!**********************************************************************************************************************
    Beta1Nw = 0;   Beta1NO3w = 0;    Beta1Pw = 0;   pKc1c2 = 0; koko=0;    kvkv=0;    fNfN=0;    vrvelr_o=0;  vrvelr_s=0;
	Beta1cw=0  ; Beta1Ch4w=0  ;  sumf1=0 ; sumf2=0 !Amir

!еееееееееееееее  A╡ir for mass balance Y1, Y2 and Y12 ееееееееееееееее
    Y12_LPOCset =0; Y12_RPOCset =0; Y12_Coutw=0; Y12_DOCresp =0; Y12_DOCden  =0
    YY12_LPOCw  =0; YY12_RPOCw  =0; YY12_DOCw=0; Y12_DOCdif  =0; Y12_DOCmeth =0 ; Y12_plantC=0 ; Y12_methane=0
    Y1_LPOCset =0 ; Y1_RPOCset =0 ; Y1_Coutw=0 ; Y1_DOCresp =0 ; Y1_DOCden  =0
    YY1_LPOCw  =0 ; YY1_RPOCw  =0 ; YY1_DOCw=0 ; Y1_DOCdif  =0 ; Y1_DOCmeth =0 ; Y1_plantC=0 ; Y1_methane=0
    Y2_LPOCset =0 ; Y2_RPOCset =0 ; Y2_Coutw=0 ; Y2_DOCresp =0 ; Y2_DOCden  =0
    YY2_LPOCw  =0 ; YY2_RPOCw  =0 ; YY2_DOCw=0 ; Y2_DOCdif  =0 ; Y2_DOCmeth =0 ; Y2_plantC=0 ; Y2_methane=0

!**********************************************************************************************************************
END SUBROUTINE
!**********************************************************************************************************************


!**********************************************************************************************************************
SUBROUTINE negativity
!**********************************************************************************************************************

Use parm ; use pprime
!**********************************************************************************************************************
    !This part is to check nonnegativity
    if(.not.(ONw(i+1).ge.0))   ONw(i+1)=1e-8    ;    
    if(.not.(ONsf(i+1).ge.0))  ONsf(i+1)=1e-8   ;     
    if(.not.(ONss(i+1).ge.0))  ONss(i+1)=1e-8   ;     
    if(.not.(Nw(i+1).ge.0))    Nw(i+1)=1e-8     ;  
    if(.not.(Ns1(i+1).ge.0))   Ns1(i+1)=1e-8    ;  
    if(.not.(Ns2(i+1).ge.0))   Ns2(i+1)=1e-8    ;  
    if(.not.(NO3w(i+1).ge.0))  NO3w(i+1)=1e-8   ;  
    if(.not.(NO3s1(i+1).ge.0)) NO3s1(i+1)=1e-8  ;  
    if(.not.(NO3s2(i+1).ge.0)) NO3s1(i+1)=1e-8  ;
    if(.not.(Ow(i+1).ge.0))    Ow(i+1)=1e-8   
    if(.not.(a(i+1).ge.0))     a(i+1)=1e-8   
    if(.not.(b(i+1).ge.0))     b(i+1)=1e-8   
    if(.not.(Pw(i+1).ge.0))    Pw(i+1)=1e-8
    if(.not.(Ps1(i+1).ge.0))   Ps1(i+1)=1e-8
    if(.not.(Ps2(i+1).ge.0))   Ps2(i+1)=1e-8
    if(.not.(mw(i+1).ge.0))    mw(i+1)=1e-8
    if(.not.(ms(i+1).ge.0))    ms(i+1)=1e-8

    !еееееееееееее  A╡ir  ееееееееееееееееееееееее
    if (.not.(LPOCw(i+1).ge.0))		LPOCw(i+1)=1e-8     ;               
    if (.not.(RPOCw(i+1).ge.0))		RPOCw(i+1)=1e-8     ;          
    if (.not.(DOCw(i+1).ge.0))		DOCw(i+1)=1e-8      ;         
    if (.not.(CH4w(i+1).ge.0))		CH4w(i+1)=1e-8      ;         
    if (.not.(TOCw(i+1).ge.0))		TOCw(i+1)=1e-8      ;
    if (.not.(LPOCs1(i+1).ge.0))	LPOCs1(i+1)=1e-8    ;
    if (.not.(RPOCs1(i+1).ge.0))	RPOCs1(i+1)=1e-8    ; 
    if (.not.(DOCs1(i+1).ge.0))	DOCs1(i+1)=1e-8     ;  
    if (.not.(CH4s1(i+1).ge.0))	CH4s1(i+1)=1e-8     ;  
    if (.not.(LPOCs2(i+1).ge.0))	LPOCs2(i+1)=1e-8
    if (.not.(RPOCs2(i+1).ge.0))	RPOCs2(i+1)=1e-8    
    if (.not.(DOCs2(i+1).ge.0))	DOCs2(i+1)=1e-8       
    if (.not.(CH4s2(i+1).ge.0))	CH4s2(i+1)=1e-8     
            
            ! detection limit for DOC is 0.1 mg/lit or 1e-7 gr/cm3 (maynard carbon paper et al 2011)
            !еееееееееееееееееееееееееееееееееееееееееееее
            
!**********************************************************************************************************************
END SUBROUTINE
!**********************************************************************************************************************
  
!**********************************************************************************************************************
SUBROUTINE Equal0_Part2
!**********************************************************************************************************************
   Use PArm
   Use Pprime
!**********************************************************************************************************************
  
!**********************************************************************************************************************
    SumVwONw  = 0;    SumVwNw   = 0;  SumVwNO3w  = 0;    SumVwOw     = 0;  SumVwPw    = 0; SumVwmw = 0
    SumVwONss = 0;    SumVwNs1  = 0;  SumVwNO3s1 = 0;    SumVwa      = 0;  SumVwPs1   = 0; SumVwms = 0
    SumVwONsf = 0;    SumVwNs2  = 0;  SumVwNO3s2 = 0;    SumVwb      = 0;  SumVwPs2   = 0 
    SumVw     = 0;    SumVs     = 0;  SumVs1     = 0;    SumVs2      = 0;  SumVwmwset = 0  
    SumVwNset = 0;    SumVwNvol = 0;  SumVwNO3den= 0;    SumVwPwset  = 0;  TotMNw     = 0
    SumVwPdif = 0;    SumVwPmin = 0;  SumVwNdif  = 0;    SumVwNO3dif = 0
           
    !ееееееееееееееееее  A╡ir  еееееееееееееееееее
    SumVwLPOCw=0   ;   SumVwRPOCw=0     ;SumLPOCset =0; SumDOCmeth =0
    SumVwDOCw=0    ;   SumVs1LPOCs1=0   ;SumRPOCset =0; SumplantC  =0  
    SumVs1RPOCs1=0 ;   SumVs1DOCs1=0    ;SumCoutw   =0; summethane = 0
    SumVs2LPOCs2=0 ;   SumVs2RPOCs2=0   ;SumDOCdif  =0;
    SumVs2DOCs2=0  ;   SumVwTOCw=0      ;SumDOCresp =0;
    Sumf1=0        ;   Sumf2=0          ;SumDOCden  =0;
    SUMVwTOCw=0
	sumvwch4w=0;       sumvs1ch4s1=0    ;sumvs2ch4s2=0;         
!**********************************************************************************************************************
END SUBROUTINE
!**********************************************************************************************************************

!**********************************************************************************************************************
! This subroutine reads the master control file and to open input and output files for inputs and results, repectively.
!
!fixedparams        ::the name of basic parameters file
!initialconc        ::the name of initial concentrations file
!hydro_climparams   ::the name of hydro-climate file
!timedepparms       ::the name of input concentrations file
!generatedparms     ::the name of generated nitrogen-phosphorus-sediment parameters file
!generatedparmcarbon::the name of generated carbon parameters file 
!
!  Definition of WetQual Outputs
!Output file Name	::Symbol	::Definition, Units
!102_obs_Onw.txt	::Onw	    ::Particulate organic nitrogen concentration in free water (mg/L)
!103_obs_Onss.txt	::Onss	    ::Concentration of refractory organic nitrogen in wetland soil (mg/L)
!104_obs_Onsf.txt	::Onsf	    ::Concentration of labile organic nitrogen in wetland soil (mg/L)
!105_obs_Nw.txt	    ::Nw	    ::Total ammonia-nitrogen ([NH4+] + [NH3]) concentration in free water (mg/L)
!106_obs_Ns1.txt	::Ns1	    ::Total ammonia-nitrogen pore-water concentration in upper aerobic layer (mg/L)
!107_obs_Ns2.txt	::Ns2	    ::Total ammonia-nitrogen pore-water concentration in lower anaerobic layer (mg/L)
!108_obs_NO3w.txt	::NO3w	    ::Nitrate-nitrogen concentration in free water (mg/L)
!109_obs_NO3s1.txt	::NO3s1	    ::Nitrate-nitrogen pore-water concentration in upper aerobic layer (mg/L)
!110_obs_NO3s2.txt	::NO3s2	    ::Nitrate-nitrogen pore-water concentration in lower anaerobic layer (mg/L)
!111_obs_Ow.txt	    ::Ow	    ::Oxygen concentration in free water (mg/L)
!112_obs_a.txt	    ::a	        ::Mass of free floating plant (gr chlorophyll a)
!113_obs_b.txt	    ::b	        ::Mass of rooted plants (gr chlorophyll a)
!114_obs_Pw.txt	    ::Pw	    ::Total inorganic phosphorus concentration in free water (mg/L)
!115_obs_Ps1.txt	::Ps1	    ::Total phosphorus concentration in aerobic layer (mg/L)
!116_obs_Ps2.txt	::Ps2	    ::Total phosphorus concentration in anaerobic layer (mg/L)
!118_obs_mw.txt	    ::mw	    ::Sediment concentration in free water (mg/L)
!150_obs_DOCw.txt	::DOCw	    ::Concentrations of dissolved organic C in free water (mg/L)
!151_obs_LPOCw.txt	::LPOCw	    ::Concentrations of labile (fast reacting) particulate organic C in free water (mg/L)
!152_obs_RPOCw.txt	::RPOCw	    ::Concentrations of refractory (slow reacting) particulate organic C in free water (mg/L)
!153_obs_DOCs1.txt	::DOCs1	    ::Pore water concentrations of DOC in aerobic sediment layer (mg/L)
!154_obs_LPOCs1.txt	::LPOCs1	::Pore water concentrations of LPOC in aerobic sediment layer (mg/L)
!155_obs_RPOCs1.txt	::RPOCs1	::Pore water concentrations of RPOC in aerobic sediment layer (mg/L)
!156_obs_DOCs2.txt	::DOCs2	    ::Pore water concentrations of DOC in lower anaerobic sediment layer (mg/L)
!157_obs_LPOCs2.txt	::LPOCs2	::Pore water concentrations of LPOC in lower anaerobic sediment layer (mg/L)
!158_obs_RPOCs2.txt	::RPOCs2	::Pore water concentrations of RPOC in lower anaerobic sediment layer (mg/L)
!159_obs_TOCw.txt	::TOCw	    ::Concentrations of total organic C in free water (mg/L)
!160_obs_CH4w.txt	::CH4w	    ::Methane concentration in free water (mg/L)
!161_obs_CH4s1.txt	::CH4s1	    ::Methane concentration in aerobic sediment layer (mg/L)
!162_obs_CH4s2.txt	::CH4s2	    ::Methane concentration in anaerobic sediment layer (mg/L)

!**********************************************************************************************************************

!**********************************************************************************************************************
SUBROUTINE InOutTXT
!**********************************************************************************************************************

USE parm
IMPLICIT NONE
!**********************************************************************************************************************
INTEGER :: k,kk
!**********************************************************************************************************************

! Amir: Read input file names from file no. 1

!!Mehdi: all the input files control from "1_input_control.txt" files.
 input_control= "1_input_control.txt"
 open (1,file=input_control,status="old")
!**********************************************************************************************************************
 read (1,*) fixedparams
 read (1,*) initialconc
 read (1,*) hydro_climparams
 read (1,*) timedepparms
 read (1,*) generatedparms
 read (1,*) generatedparmcarbon
!**********************************************************************************************************************

!**********************************************************************************************************************
open (102,file="102_Onw.txt",status="unknown")           ! SI: daily averages of ON in water
open (103,file="103_Onss.txt",status="unknown")          ! SI:
open (104,file="104_Onsf.txt",status="unknown")          ! SI:
open (105,file="105_Nw.txt",status="unknown")            ! SI:
open (106,file="106_Ns1.txt",status="unknown")           ! SI:
open (107,file="107_Ns2.txt",status="unknown")           ! SI:
open (108,file="108_NO3w.txt",status="unknown")          ! SI:
open (109,file="109_NO3s1.txt",status="unknown")         ! SI:
open (110,file="110_NO3s2.txt",status="unknown")         ! SI:
open (111,file="111_Ow.txt",status="unknown")            ! SI:
open (112,file="112_a.txt",status="unknown")             ! SI:
open (113,file="113_b.txt",status="unknown")             ! SI:
open (114,file="114_Pw.txt",status="unknown")            ! SI:
open (115,file="115_Ps1.txt",status="unknown")           ! SI:
open (116,file="116_Ps2.txt",status="unknown")           ! SI:
!open (117,file="117_ms.txt",status="unknown")            ! SI: Removed by Mehdi
open (118,file="118_mw.txt",status="unknown")            ! SI:
!**********************************************************************************************************************

!еееееееееееееее  A╡ir  ееееееееееееееееееееееее
open (150,file="150_DOCw.txt",status="unknown")				! daily averages of DOC in water
open (151,file="151_LPOCw.txt",status="unknown") 
open (152,file="152_RPOCw.txt",status="unknown") 
open (153,file="153_DOCs1.txt",status="unknown") 
open (154,file="154_LPOCs1.txt",status="unknown") 
open (155,file="155_RPOCs1.txt",status="unknown") 
open (156,file="156_DOCs2.txt",status="unknown") 
open (157,file="157_LPOCs2.txt",status="unknown") 
open (158,file="158_RPOCs2.txt",status="unknown") 
open (159,file="159_TOCw.txt",status="unknown") 
open (160,file="160_CH4w.txt",status="unknown")
open (161,file="161_CH4s1.txt",status="unknown")
open (162,file="162_CH4s2.txt",status="unknown")
!**********************************************************************************************************************

!**********************************************************************************************************************
END SUBROUTINE InOutTXT
!**********************************************************************************************************************


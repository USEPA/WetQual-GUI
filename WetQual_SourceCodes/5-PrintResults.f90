!**********************************************************************************************************************
! This subroutine print the results to the output files.
!**********************************************************************************************************************

!**********************************************************************************************************************
SUBROUTINE printresults
!**********************************************************************************************************************

use parm
use pprime

tsim=t+1

!     write (102,"(i8)",advance="no") tsim  ;write (102,100) (ZOnw(j)   , j=0,n)   ! SI:
!     write (103,"(i8)",advance="no") tsim  ;write (103,100) (ZOnss(j)  , j=0,n)   ! SI:
!     write (104,"(i8)",advance="no") tsim  ;write (104,100) (ZOnsf(j)  , j=0,n)   ! SI:
!     write (105,"(i8)",advance="no") tsim  ;write (105,100) (ZNw(j)    , j=0,n)   ! SI:
!     write (106,"(i8)",advance="no") tsim  ;write (106,100) (ZNs1(j)   , j=0,n)   ! SI:
!     write (107,"(i8)",advance="no") tsim  ;write (107,100) (ZNs2(j)   , j=0,n)   ! SI:
!     write (108,"(i8)",advance="no") tsim  ;write (108,100) (ZNO3w(j)  , j=0,n)   ! SI:
!     write (109,"(i8)",advance="no") tsim  ;write (109,100) (ZNO3s1(j) , j=0,n)   ! SI:
!     write (110,"(i8)",advance="no") tsim  ;write (110,100) (ZNO3s2(j) , j=0,n)   ! SI:
!     write (111,"(i8)",advance="no") tsim  ;write (111,100) (ZOw(j)    , j=0,n)   ! SI:
!     write (112,"(i8)",advance="no") tsim  ;write (112,100) (Za(j)     , j=0,n)   ! SI:
!     write (113,"(i8)",advance="no") tsim  ;write (113,100) (Zb(j)     , j=0,n)   ! SI:
!     write (114,"(i8)",advance="no") tsim  ;write (114,100) (ZPw(j)    , j=0,n)   ! SI:
!     write (115,"(i8)",advance="no") tsim  ;write (115,100) (ZPs1(j)   , j=0,n)   ! SI:
!     write (116,"(i8)",advance="no") tsim  ;write (116,100) (ZPs2(j)   , j=0,n)   ! SI:
!!     write (117,"(i8)",advance="no") tsim  ;write (117,100) (Zms(j)    , j=0,n)   ! SI:
!     write (118,"(i8)",advance="no") tsim  ;write (118,100) (Zmw(j)    , j=0,n)   ! SI:
!
!
!    !������������������  A�ir  �������������������
!	 write (150,"(i8)",advance="no") tsim  ; write (150,100) (ZDOCw(j) , j=0,n)
!	 write (151,"(i8)",advance="no") tsim  ; write (151,100) (ZLPOCw(j) , j=0,n)
!	 write (152,"(i8)",advance="no") tsim  ; write (152,100) (ZRPOCw(j) , j=0,n)
!	 write (153,"(i8)",advance="no") tsim  ; write (153,100) (ZDOCs1(j) , j=0,n)
!	 write (154,"(i8)",advance="no") tsim  ; write (154,100) (ZLPOCs1(j) , j=0,n)
!	 write (155,"(i8)",advance="no") tsim  ; write (155,100) (ZRPOCs1(j) , j=0,n)
!	 write (156,"(i8)",advance="no") tsim  ; write (156,100) (ZDOCs2(j) , j=0,n)
!	 write (157,"(i8)",advance="no") tsim  ; write (157,100) (ZLPOCs2(j) , j=0,n)
!	 write (158,"(i8)",advance="no") tsim  ; write (158,100) (ZRPOCs2(j) , j=0,n)
!	 write (159,"(i8)",advance="no") tsim  ; write (159,100) (ZTOCw(j) , j=0,n)
!	 write (160,"(i8)",advance="no") tsim  ; write (160,100) (ZCH4w(j) , j=0,n)
!	 write (161,"(i8)",advance="no") tsim  ; write (161,100) (ZCH4s1(j) , j=0,n)
!	 write (162,"(i8)",advance="no") tsim  ; write (162,100) (ZCH4s2(j) , j=0,n)
!	!���������������������������������������������
!    100 format (3660F10.5)       

                                                                                                                                               
     write (102,"(i8)") tsim  ;write (102,100) (ZOnw(j)   , j=0,n)   ! SI:                                                                                             
     write (103,"(i8)") tsim  ;write (103,100) (ZOnss(j)  , j=0,n)   ! SI:
     write (104,"(i8)") tsim  ;write (104,100) (ZOnsf(j)  , j=0,n)   ! SI:
     write (105,"(i8)") tsim  ;write (105,100) (ZNw(j)    , j=0,n)   ! SI:
     write (106,"(i8)") tsim  ;write (106,100) (ZNs1(j)   , j=0,n)   ! SI:
     write (107,"(i8)") tsim  ;write (107,100) (ZNs2(j)   , j=0,n)   ! SI:
     write (108,"(i8)") tsim  ;write (108,100) (ZNO3w(j)  , j=0,n)   ! SI:
     write (109,"(i8)") tsim  ;write (109,100) (ZNO3s1(j) , j=0,n)   ! SI:
     write (110,"(i8)") tsim  ;write (110,100) (ZNO3s2(j) , j=0,n)   ! SI:
     write (111,"(i8)") tsim  ;write (111,100) (ZOw(j)    , j=0,n)   ! SI:
     write (112,"(i8)") tsim  ;write (112,100) (Za(j)     , j=0,n)   ! SI:
     write (113,"(i8)") tsim  ;write (113,112) (Zb(j)     , j=0,n)   ! SI:
     write (114,"(i8)") tsim  ;write (114,100) (ZPw(j)    , j=0,n)   ! SI:
     write (115,"(i8)") tsim  ;write (115,100) (ZPs1(j)   , j=0,n)   ! SI:
     write (116,"(i8)") tsim  ;write (116,100) (ZPs2(j)   , j=0,n)   ! SI:
!     write (117,"(i8)") tsim  ;write (117,100) (Zms(j)    , j=0,n)   ! SI:
     write (118,"(i8)") tsim  ;write (118,111) (Zmw(j)    , j=0,n)   ! SI:

    
    !������������������  A�ir  �������������������
	 write (150,"(i8)") tsim  ; write (150,100) (ZDOCw(j) , j=0,n)
	 write (151,"(i8)") tsim  ; write (151,100) (ZLPOCw(j) , j=0,n)
	 write (152,"(i8)") tsim  ; write (152,100) (ZRPOCw(j) , j=0,n)
	 write (153,"(i8)") tsim  ; write (153,100) (ZDOCs1(j) , j=0,n)
	 write (154,"(i8)") tsim  ; write (154,100) (ZLPOCs1(j) , j=0,n)
	 write (155,"(i8)") tsim  ; write (155,100) (ZRPOCs1(j) , j=0,n)
	 write (156,"(i8)") tsim  ; write (156,100) (ZDOCs2(j) , j=0,n)
	 write (157,"(i8)") tsim  ; write (157,100) (ZLPOCs2(j) , j=0,n)
	 write (158,"(i8)") tsim  ; write (158,100) (ZRPOCs2(j) , j=0,n)
	 write (159,"(i8)") tsim  ; write (159,100) (ZTOCw(j) , j=0,n)
	 write (160,"(i8)") tsim  ; write (160,100) (ZCH4w(j) , j=0,n)
	 write (161,"(i8)") tsim  ; write (161,100) (ZCH4s1(j) , j=0,n)
	 write (162,"(i8)") tsim  ; write (162,100) (ZCH4s2(j) , j=0,n)
	!���������������������������������������������
    100 format (365F10.5)    
    111 format (365F11.4) 
    112 format (365F10.2) 
!**********************************************************************************************************************

!**********************************************************************************************************************
END SUBROUTINE printresults
!**********************************************************************************************************************

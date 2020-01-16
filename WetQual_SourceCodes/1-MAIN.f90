!**********************************************************************************************************************
! This file includes main program of Wetland model
! 
!**********************************************************************************************************************
!
PROGRAM Wetland
!**********************************************************************************************************************
    USE parm        !0-PARMS.f90
    USE Pprime      !0-PARMS.f90

    IMPLICIT none
!**********************************************************************************************************************
    Write (*,*) '                       WetQual-Model'
    Write (*,*) ''
    Write (*,*)	'A numerical model for N, P and C cycling in ponded wetlands'
    Write (*,*)	'Developed by: '
    Write (*,*)	'   M.M. Hantush' 
    Write (*,*)	'   L. Kalin' 
    Write (*,*)	'   S.Isik' 
    Write (*,*)	'   A. Sharifi' 
    Write (*,*)	'USEPA and Auburn University (2014-2019) '
!**********************************************************************************************************************
!   Call subroutine> InOutTXT  to read the master control file and to open input and output files for inputs 
!                                       and results, repectively.
!
    CALL InOutTXT       !2.1-InOutTXT.f90
!**********************************************************************************************************************
!   Call subroutine> FixedInitial to read  the basic model parameters, initial concentrations of nutrients, 
!                                          hydro-climate and input concentration time series.
!
    CALL FixedInitial   !2.2-FIXEDINITIAL.f90
!**********************************************************************************************************************
		
    ntday=real(int((n/dt-1)+0.00001))
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !Let the MC simulation begin
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    DO t=0,sims-1     ! 100,000 times
         write(*,*)"Simulation #",t+1
        !**************************************************************************************************************
        !   Call subroutines> readparms to read some of the WetQual parameters which are considered to be random. 
        !   Call subroutines> Equal0 to check the calculated outputs for non-negativity.
        !
         Call readparms  !2.3-Parms-Calc.f90      
         Call Equal0     !2.4-Neg-EqZero.f90
        !**************************************************************************************************************
    !******************************************************************************************************************
    !****                   !individual simulations begin                                                       *******
    !****                   Please see explainations for subroutines in their files                             *******
    !******************************************************************************************************************
         DO i=0,int((n/dt-1)+dt/2)  ! i=0 to 73400
               Call GeneralCalc1    !2.3-Parms-Calc.f90
               Call Nitrogen        !3.1-Nitrogen.f90
               Call OxyPlant        !3.0-Plant-Oxygen.f90
               Call PhosphorSed     !3.2-Phos-sed.f90
               Call Carbon          !3.3-Carbon.f90
               Call Negativity      !2.4-Neg-EqZero.f90
               Call GeneralCalc2    !2.3-Parms-Calc.f90
               Call massbalance     !4.0-Daily-MassB.f90
               Call DailyCalcs      !4.0-Daily-MassB.f90
         END DO
         Call Printresults          !5-PrintResults.f90                                                                                                                                       
    END DO
!**********************************************************************************************************************
    if (sims==1) then
        Print*, "Deterministic model was successfully executed!"
    else
        Print*, "Stochastic model was successfully executed!"
    endif
!**********************************************************************************************************************
    PAUSE
    STOP
!**********************************************************************************************************************
END PROGRAM Wetland
!**********************************************************************************************************************

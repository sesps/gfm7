C****                                                                   
C**** RAY TRACE  -  MIT VERSION 1981   (01/28/82)                       
C**** DR. STANLEY KOWALSKI                                              
C**** MASS INST OF TECH                                                 
C**** BLDG 26-427                                                       
C**** CAMBRIDGE MASS 02139                                              
C**** PH 617+253-4288                                                   
C****                                                                   
C**** CHANGES ADDED AT LNL ARE MARKED:                   !JDL 10-NOV-83
C****
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8 BX, BY, BZ, K, TC, DTC 
      integer kk
cdddddddddddddddddddddddddddddddddddddddddddddcambiodddddddddddddddddd
      character*20 nombre,nomplt,nomgrf,nommca,nomdat,nomlis
cdddddddddddddddddddddddddddddddddddddddddddddcambiodddddddddddddddddd
                                             
      LOGICAL LPLT
c      REAL*4 DAET, TYME                                                
      COMMON  /BLCK00/  LPLT
      include 'rtcomm0.f'
      COMMON  /BLCK 1/  XI, YI, ZI, VXI, VYI, VZI, DELP, DELM      !JDL 
      COMMON  /BLCK 2/  XO, YO, ZO, VXO, VYO, VZO, RTL(100), RLL(100)   
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       
      COMMON  /BLCK 6/  NR,  NP,   NSKIP, JFOCAL, JMTRX, !JDL 16-MAR-84 
     1                  JNR, NPLT, NRXS,  LPAX,          !JDL 31-OCT-84
     2                  NCAX,NHAX, NVAX, MEL, MCS, MCP,  !JDL 31-OCT-84
     3                  DHAX,DVAX                        !JDL 31-0CT-84
      COMMON  /BLCK 7/  NCODE                                           
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC                           
      COMMON  /BLCK15/  TMIN,PMIN,XMAX,TMAX,YMAX,PMAX,DMAX,        !JDL 
     1                  STMN,SPMN,SXMX,STMX,SYMX,SPMX,SDMX,SUMX,   !JDL
     2                  SEED,SEEP,DXHW,DTHW,DYHW,DPHW,DEHW,DMHW,   !JDL
     3                  SEC1,SEC2,SEC3,SEC4,SEC5,SEC6,SEC7,SEC8    !JDL
      COMMON  /BLCK16/  NLOOP,NPASS,IP,NCSV,KEEP(20),    !JDL 17-NOV-83
     1                  LOOPSV(5,30),HOOPSV(30),HSAVE(30),PMSV(3), !JDL
     2                  CXXSV(12,3),CSV(36,3),CDSV(6,4,3)!JDL 17-NOV-83
      COMMON  /BLCK26/  IXS, XSCTR, ZSCTR                !JDL 16-MAR-84
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK61/  DEDX,ALPHAK,TOLD2,DEDXQ
      COMMON  /BLCK62/  GASENE,GASVEL,TOLD,GASL
      COMMON  /BLCK63/  ISEED
c      include 'rtcomm65.f'
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt

      COMMON  /BLCK66/  tfwopt,taufct,alffct,TBAR,THWHM,tau0
      common  /blck70/  vel0,en0,pm0
c-ddc to fix complaint about common blocksize
      integer*8 icalc

      common  /blck71/  gasopt,zgas125,zgas18,dreldee,enold,icalc
      DIMENSION NCOLS(10,6), VECJ(10), VECK(10)          !JDL 31-OCT-84
C*JDL DIMENSION DAET(3), TYME(2)                                        
C*IBM DIMENSION DAET(5), TYME(2)
      DIMENSION XO(100), YO(100), ZO(100), VXO(100), VYO(100), VZO(100) 
      DIMENSION XI(100), YI(100), ZI(100), VXI(100), VYI(100), VZI(100),
     1        DELP(100)                                                 
C*JDL DIMENSION NWORD(15),DATA( 75,30), IDATA(30),NTITLE(20),ITITLE(30) 
      DIMENSION TC(6), DTC(6), R(6,6), T2(5,6,6)                        
      DATA TBAR/0./
      DATA NWORD/'SENT', 'DIPO', 'QUAD', 'HEXA', 'OCTA', 'DECA', 'EDIP',
     1   'VELS', 'POLE', 'MULT', 'SHRT', 'DRIF', 'XXXX', 'SOLE', 'LENS',
     2   'CHAN'/                                         !JDL 17-NOV-83
      DATA NCOLS /10, 5, 3, 6, 4, 6, 6, 6, 4, 7,         !JDL 17-NOV-83
     1             9, 5, 4, 6, 6, 0, 0, 0, 0, 0,
     2            10, 5, 1, 8, 4, 6, 6, 0, 0, 0,
     3            10, 5, 4, 4, 4, 6, 6, 6, 6, 0,
     4             9, 4, 5, 4, 6, 6, 0, 0, 0, 0,
     5             9, 6, 4, 6, 3, 0, 0, 0, 0, 0/         !JDL 17-NOV-83
      DATA C /3.D10/                                                    
      DATA TYME/ '    ', '    ' /
c-ddc init data for date, so 'junk' isn't put in output files.
      DATA daet/ '    ', '    ','    ' /

      character *4 nt1,nt2,nwd
      character *2 foil                                      !MP 27-jul-
      DATA NT1, NT2/' RT8','2.0 '/
C
      data fTab/1.1/                                               !MP 1
      fwhmstg(Zab,Aab,Tab,Zio) =  
     1             dsqrt((Zab/Aab)*Tab*fTab)*Zio*0.924D-3          !MP 1
C                                                                       
C****fwhmstg calculates the energy straggling (fwhm) in MeV for an incid
C****ion with Z=Zio in a solid Z=Zab, A=Aab of thickness Tab (microg/cm2
C****fwhmstg is used to calculate straggling in target and entrance foil
C****fTab is a fudge factor for target thickness non-uniformity         
C    take fTab = 1.1
C****                                                                   
C****                
cdddddddddddddddddddddddddddddddddddddddddddddcambiodddddddddddddddddd
      write(6,*)'archive entry with extension?'
      read(5,2321)nombre
 2321 format(a20)
      do i=1,20
        if(nombre(i:i).eq.'.') then
         kk=i-1
        else
        endif
      enddo

      nomdat(1:20)=nombre(1:20)
c      nomdat(1:kk)=nombre(1:kk)
      nomdat(kk+1:kk+5)='.dat'
      nomdat(lnblnk(nomdat)+1:lnblnk(nomdat)+1)=''

      nomplt(1:20)=nombre(1:20)
c      nomplt(1:kk)=nombre(1:kk)
      nomplt(kk+1:kk+5)='.plt'
      nomplt(lnblnk(nomplt)+1:lnblnk(nomplt)+1)=''

      nomgrf(1:20)=nombre(1:20)
c      nomgrf(1:kk)=nombre(1:kk)
      nomgrf(kk+1:kk+5)='.grf'
      nomgrf(lnblnk(nomgrf)+1:lnblnk(nomgrf)+1)=''

      nommca(1:20)=nombre(1:20)
c      nommca(1:kk)=nombre(1:kk)
      nommca(kk+1:kk+5)='.mca'
      nommca(lnblnk(nommca)+1:lnblnk(nommca)+1)=''

      nomlis(1:20)=nombre(1:20)
c      nomlis(1:kk)=nombre(1:kk)
      nomlis(kk+1:kk+5)='.lis'
      nomlis(lnblnk(nomlis)+1:lnblnk(nomlis)+1)=''
      

	open(unit=10,file=nomdat,status='old')  
	open(unit=2,file=nomgrf,status='unknown')  
	open(unit=3,file=nommca,status='unknown')  
	open(unit=4,file=nomlis,status='unknown')  
	open(unit=11,file=nomplt,status='unknown')  
                          
cdddddddddddddddddddddddddddddddddddddddddddddcambiodddddddddddddddddd
                                                   
  100 FORMAT( 8F10.5 )                                                  
  120 FORMAT( 5F5.0,a2,3x,5F10.5 )
  101 FORMAT( 20A4 )                                                    
  102 FORMAT(10I5)                                                      
  103 FORMAT( ///  10X, 'KEY WORD DOES NOT MATCH STORED LIST - NWD= 'A4)
  104 FORMAT( // 10X, ' GO TO STATEMENT IN MAIN FELL THROUGH - I= ' I5/)
  105 FORMAT( 1H1, 10X, 20A4  )                                         
  106 FORMAT( 1H1 )                                                     
  107 FORMAT( 5F10.5/5F10.5/3F10.5/6F10.5/4F10.5/6F10.5/6F10.5/    !JDL 
     1        6F10.5/ 4F10.5/ 7F10.5/ 7F10.5                           )
  108 FORMAT('1',62X, 'RAY ', I4, //  30X, 'ENERGY=',F8.3,' MEV ', 7X,  
     1   'PMOM=', F8.3, ' MEV/C', 6X, 'VELC=', 1PD11.3, ' CM/SEC'    /  
     2   30X, 'DELE/E=', 0PF8.3, ' (PC)', 5X, 'DELP/P=', F8.3,          
     3   ' (PC) ', 4X, 'DELV/V=', F7.3, '     (PC)'        /)           
  109 FORMAT( 3F10.5/ 5F10.5/ 4F10.5/ 6F10.5/ 6F10.5                   )
  111 FORMAT( 2F10.5/ 6F10.5/ 2F10.5/ 6F10.5/ 3F10.5 )                  
  112 FORMAT( 3F10.5/ 4F10.5/ 5F10.5/ 4F10.5/ 6F10.5/ 6F10.5    )       
  113 FORMAT( A4, 16X, A4  )                                            
  114 FORMAT( 1F10.5 / 5F10.5 / 2F10.5  )                               
  115 FORMAT( 4F10.5/ 5F10.5/ 4F10.5/ 4F10.5/ 4F10.5/ 6F10.5/ 6F10.5/   
     1   6F10.5/ 6F10.5   )                                             
C****                              !Changes from here... !JDL 10-MAR-84
1115  format( /10x,' Z(target) =', f10.1,/                         !MP 3
     1         10x,' A(target) =', f10.1,/                         !MP 3
     2         10x,' t(target) =', f10.1,' microg/cm2')            !MP 3
1116  format(//10x,' foil      =   ',a2,/                          !MP 3
     1         10x,' t(foil)   =', f10.1,' microg/cm2')            !MP 3
11165 format(//10x,' target energy loss (MeV) =', f10.2/
     1         10x,' target fwhm straggling (keV) =', f10.1)
11166 format(//10x,' foil energy loss (MeV) =', f10.2/
     1         10x,' foil fwhm straggling (keV) =', f10.1)
1117  format(//10x,' energy before target =', f10.4,' MeV',/       !MP 2
     1         10x,' energy after  target =', f10.4,' MeV',/       !MP 2
     2         10x,' energy after  foil   =', f10.4,' MeV')        !MP 2
  116 FORMAT(//10X, '  PARTICLE ENERGY =', F10.4,  '  MEV   ',          
     1 15X,'NR     =', I4, 7X, 'JNR    =', I4,11X, 'ENERGY =', F9.4, /
     2         10X, 'PARTICLE MOMENTUM =', F10.4,  '  MEV/C ',          
     3 15X,'NP     =', I4, 7X, 'NPLT   =', I4,11X, 'DEN    =', F9.4, /
     4         10X, 'PARTICLE VELOCITY =', F10.4,  '  CM/NS ',          
     5 15X,'NSKIP  =', I4, 7X, 'NRXS   =', I4,11X, 'XNEN   =', F9.4, /
     6         10X, '             MASS =', F10.4,  '  AMU   ',          
     7 15X,'JFOCAL =', I4, 7X, 'LPAX   =', I4,11X, 'PMASS  =', F9.4, /
     8         10X, '           CHARGE =', F10.4,  '  EQ    ',          
     9 15X,'JMTRX  =', I4, 7X, 12X,           11X, 'Q0     =', F9.4, /
     X 62X,    12X,        7X, 12X,           11X, 'DMASS  =', F9.4  )
117   FORMAT( 10X, 62X, 3A4, 1X, 2A4)                    !JDL 10-MAR-84 
C****                                  !...down to here. !JDL 10-MAR-84
C*IBM117   FORMAT( 10X, 3A4, 1X, 2A4, 2A4 )
118   FORMAT(4F10.5/5F10.5/F10.5/4F10.5/4F10.5/6F10.5/6F10.5)
119   FORMAT( F10.0, 10X, 6F10.5, /, 8F10.5)             !JDL 13-OCT-84
C****                                                                   
c      CALL DATE(DAET)                                                  
c      CALL TIME(TYME)                                                  
C*IBM CALL WHEN(DAET)
C**** CALL ERRSET( NUMBER, CONT, COUNT, TYPE, LOG, MAXLIN     )         
c      CALL ERRSET( 63, .TRUE., .FALSE., .FALSE., .FALSE., 2048)
c      CALL ERRSET( 72, .TRUE., .FALSE., .FALSE., .TRUE.,  2560)        
c      CALL ERRSET( 74, .TRUE., .FALSE., .FALSE., .TRUE.,  2560)        
c      CALL ERRSET( 88, .TRUE., .FALSE., .FALSE., .TRUE.,  2560)        
c      CALL ERRSET( 89, .TRUE., .FALSE., .FALSE., .TRUE.,  2560)        
C*IBM CALL ERRSET( 207, 256, 1 )
C*IBM CALL ERRSET( 208, 256, 1 )
C*IBM CALL ERRSET( 209, 256, 1 )
C*IBM CALL ERRSET( 210, 256, 1 )                                        
C****                                                                   
C****                                                                   
    5 LPLT = .FALSE.
      JRAY = 0
      NSK1 = 0
      NSK2 = 0
      T = 0.
      GASL = 0.
      TOLD = 0.
      TOLD2 = 0.
      NCSV=0                                             !JDL 18-NOV-83
      NLOOP=0                                            !JDL 17-NOV-83
      IVEC = 0                                                          
      LNEN = 0                                                          
      NPASG = 0
      DLSQR0=0.
      ALOS0 = 0.
      DEDX0 = 0.
      qaver = 0.
      GSMFP0 = 0.
      SIGC0 = 0.
      SIGL0 = 0.
      tau0 = 0.
      icalc = 0
      DO 1  I=1,30                                                      
      IDATA(I)= 0                                                       
      DO 1  J=1,75                                                      
      DATA(J,I) = 0.                                                    
    1 CONTINUE                                                          
      READ ( 10,101,END=99) NTITLE                                       
      IF( NTITLE(1) .EQ. 'END ' ) GO TO 99               !JDL 10-MAR-84
        NTITLE(19) = NT1
        NTITLE(20) = NT2
      READ (10,102) NR, IP,   NSKIP, JFOCAL, JMTRX,       !JDL 11-MAR-84 
     1            JNR, NPLT, NRXS,  LPAX                 !JDL 31-OCT-84
C***  add input data for target thickness and Z       !MP 27-jul-93 
      READ (10,1022) ENERGY, DEN, ttgt,
     2  XNEN, ztgt, PMASS, atgt, Q0, DMASS               !JDL 10-NOV-83 
C***  ttgt : effective thickness of target in microg/cm2  !MP 27-jul-93
C***  ztgt : Z of target                                  !MP 27-jul-93
C***  atgt : A of target
 1022 format(f10.5,6f5.0,2f10.5)                          !MP 27-jul-93
      en00 = energy                                       !MP 27-jul-93
C
C*** en00 is used to recall original input energy; will never be changed
C
      IF( NPLT .NE. 0 ) LPLT = .TRUE.
      IF(( NR .GT. 100).AND.( NRXS/10 .NE. 1 )) NR=100   !JDL 23-MAY-84 
C***
C***
C*** add input data for pressure foil and thickness       !MP 27-jul-93
      READ (10,120) GAS, qopt,                             !MP 27-jul-93
     * AGAS, tfoil, ZGAS, foil, ZION, PRESS, GASopt,
     *             ACAPT,TFWopt                           !*** MP
C
C****** qopt   = 0.: use Dimitriev formula for qbar
C******        = 1.: use Betz gas formula for qbar            !   29-jan
C******        = more options : see rtqdist.for
C***    tfoil : thickness of pressure foil in microgram/cm2   !MP 27-jul
C***    foil   = MY :  mylar foil                             !MP 27-jul
C***           = PP :  polypropylene foil                     !MP 27-jul
C****** TFWopt > 0 : TFWopt = constant FWHM (radians) for mult.scatt
C****** TFWopt < 0 : Small angle scattering calculated according
C****** to Meyer-Sigmund-Winterbon theory by subroutine SMANGSC;
C****** dabs(tfwopt) is then used as fudge factor to the hwhm cal-
C****** culated in SMANGSC, i.e. TFWopt = -1. corresponds to the
C****** original formula; TFWopt = -1.2 gives a 1.2 factor in cal-
C****** culated hwhm.
C
C*** calculates energy loss in effective target thickness and  !MP 27-ju
C*** replaces energy value by energy - loss                    !MP 27-ju
C*** This is for initial printing purpose. The actual calcula-
C*** tion will be sampled for each ray in the ray loop.   
      ztgt0 = ztgt                                             !MP 27-ju
      if (ztgt0.le.0.) ztgt = 6.
      if (ztgt0.le.0.) atgt = 12.
      dextgt = fsdedx(ztgt,atgt)                               !MP 28-ju
      detgt = 0.001*ttgt*dextgt
      fwhmtgt = fwhmstg(ztgt,atgt,ttgt,zion)
      energy = energy - detgt   
      en01 = energy                                            !
C                                                              !MP 27-ju
C*** en01 is used only in PRINT statement
C   
C*** calculates de/dx (MeV/mg/cm2) in foil material (mylar,polypropylene
C*** or elemental)
C
      zc = 6.                                                  !MP 27-ju
      acar = 12.                                               !MP 29-ju
      zh = 1.                                                  !MP 27-ju
      ahy = 1.                                                 !MP 29-ju
      zox = 8.                                                 !MP 27-ju
      aox = 16.                                                !MP 29-ju
      if (foil.eq.'pp') go to 11111                            !MP 08-oc
      if (foil.eq.'my') go to 11112                            !MP 08-oc
      if (foil.eq.'ti') go to 11113                            !MP 08-oc
      if (foil.eq.'ni') go to 11113                            !MP 08-oc
      go to 11113                       
11111 continue
C*** for polypropylene (CH3CH=CH2)n :                          !MP 27-ju
C*** f(C) = 36/42.08 = 0.856                                   !MP 27-ju
C*** f(H) = 6.08/42.08  = 0.144                                !MP 27-ju
      fcpp = 0.856                                             !MP 27-ju
      fhpp = 0.144                                             !MP 27-ju
C
      zeff = fcpp*zc + fhpp*zh                                 !MP 12-no
      aeff = fcpp*acar + fhpp*ahy
      fwhmfl = fwhmstg(zeff,aeff,tfoil,zion)                   !MP 13-no
C
      dexcpp = fsdedx(zc,acar)                                 !MP 28-ju
      dexhpp = fsdedx(zh,ahy)                                  !MP 28-ju
      dexf =  fcpp*dexcpp + fhpp*dexhpp                        !MP 27-ju
      go to 2222                                               !MP 27-ju
11112 continue
C*** for mylar (C10H8O4) :                                     !MP 27-ju
C***  fcmy = 120/192 = 0.625                                   !MP 27-ju
C***  fhmy = 8/192   = 0.042                                   !MP 27-ju
C***  fomy = 64/192  = 0.333                                   !MP 27-ju
      fcmy = 0.625                                             !MP 27-ju
      fhmy = 0.042                                             !MP 27-ju
      fomy = 0.333                                             !MP 27-ju
C
      zeff = fcmy*zc + fhmy*zh + fomy*zox                      !MP 12-no
      aeff = fcmy*acar + fhmy*ahy + fomy*aox
      fwhmfl = fwhmstg(zeff,aeff,tfoil,zion)                   !MP 13-no
C 
      dexcmy = fsdedx(zc ,acar)                                !MP 28-ju
      dexhmy = fsdedx(zh ,ahy )                                !MP 28-ju
      dexomy = fsdedx(zox,aox )                                !MP 28-ju
      dexf = fcmy*dexcmy + fhmy*dexhmy + dexomy*fomy           !MP 27-ju
      go to 2222
C*** elemental foils; default = Ti
11113 continue
      zfoil = 22.
      afoil = 47.89
      if (foil.eq.'ti') zfoil = 22.
      if (foil.eq.'ti') afoil = 47.89
      if (foil.eq.'ni') zfoil = 28.
      if (foil.eq.'ni') afoil = 58.69
C********* 
      fwhmfl = fwhmstg(zfoil,afoil,tfoil,zion)
      dexf = fsdedx(zfoil,afoil)
C                                                              !MP 27-ju
C*** calculates energy loss in foil and                        !MP 27-ju
C*** replaces energy value by energy - loss                    !MP 27-ju
C
2222  continue
      defoil = 0.001*tfoil*dexf
      energy = energy - defoil
C
C
      GASENE = ENERGY                                    !***MP 1-JAN-85
      enold = energy
      EMASS = PMASS*931.48                                              
      ETOT = EMASS + ENERGY                                             
      VEL = ( DSQRT( ( 2.*EMASS + ENERGY)*ENERGY) / ETOT ) * C          
      VEL0 = VEL                                                        
      EN0 = ENERGY                                                      
      PM0 = PMASS                                        !JDL 12-MAR-84
      PMOM0 = DSQRT( (2.*EMASS + EN0)*EN0)                              
      HENR = ENERGY                                      !JDL 19-NOV-83
      HPMS = PMASS                                       !JDL 19-NOV-83
      HPMO = PMOM0                                       !JDL 19-NOV-83
      HVEL = VEL                                         !JDL 19-NOV-83
      Q00 = Q0
      NEN = XNEN                                                        
      IF( NEN  .EQ.  0 ) NEN = 1                                        
      THWHM = TFWopt * 0.5 * 1.D-3
      IF (ZGAS.EQ.0.) ZGAS = 7.
      IF (AGAS.EQ.0.) AGAS = 28.
      IF (ZION.EQ.0.) ZION = 28.
      IF (GAS.EQ.0.)   PRESS = 0.
      RHOGAS = AGAS*PRESS/(22.4*760.)
      GASK = (RHOGAS*C**2)/EMASS
      ALPHAK = .014*DSQRT(ZION*ZGAS/ 
     1                             (ZION**.3333+ZGAS**.3333))
      ATBCC = 6.D4*press/(22.4*760.)
c     ATBCC is 10**-16 * nb. of atoms or molecules ( for molecular gases
c
c     initial charge distribution in solid target ( entrance window )
c
      call sqdist
      qbars = qbar
      dsqrs0 = delsqr
      if (q00.le.0.) q0 = dnint(qbars)
c
      QMC = EMASS/(9.D10*Q0)                                            
      IF (GAS.EQ.0.) GO TO 6
c
c     calculation of 
c     capture cross sections and charge distribution in gas
c
c      if (qopt .ne. 0. .and. qopt .ne. 1. .and. qopt .ne. 2.) then
c         print *,'**** illegal value read for qopt:',qopt
c         print *,'  use qopt = 0. for Dimitriev or qopt = 1. for Betz'
c         print *,'  or qopt = 2. for Rehm (18F-18O) '
c         stop 99
c      endif
      zgas125 = zgas**1.25
      zgas18 = zgas**1.8
      CALL QDIST
      if (gasopt.le.0.) call sigcap
      if (gasopt.gt.0.) gassig = gasopt
      QAVER = QBAR
      DELQ0 = Q0 - QBAR
      CALL QSIG(DELQ0)
      ztil = dsqrt(zion**.6666667 + zgas**.6666667)
c **  Oct 17, 1991
c **  added factor gas in following expression of taufct; see doc
c **
      taufct = gas*41.5/60./(ztil*ztil)
      alffct = 1.1d-3*zion*zgas*ztil*dabs(tfwopt)/57.296
      if (tfwopt.lt.0.) call smangsc       
      DEDX = FDEDX(QAVER)
      dedxq = dedx*(q0/qbar)**2
      DLSQR0 = DELSQR
      ALOS0 = ALOS
      DEDX0 = DEDX
      GSMFP0 = GASMFP
      SIGC0 = SIGC
      SIGT0 = SIGT
    6 CONTINUE
      NO = 1                                                            
    2 READ (10,113) NWD, ITITLE(NO)                                      
      DO 3  I=1,16                                       !JDL 17-NOV-83 
      IF( NWD  .EQ. NWORD(I) ) GO TO 4                                  
    3 CONTINUE                                                          
      PRINT 103, NWD                                                    
   99 continue
      close(11)
      close(2)
      close(3)
      print *,'Bye ...'
      CALL EXIT                                                         
    4 GO TO( 11,12,13,13,13,13,17,18,19,20,21,22,23,24,25,26), I   !JDL 
C****                                                                   
C****                                                                   
   23 CONTINUE                                                          
C****                                                                   
      PRINT 104,  I                                                     
      CALL EXIT                                                         
C****                                                                   
C**** DIPOLE  LENS           TYPE = 2                                   
C****                                                                   
   12 IDATA(NO) = 2                                                     
      READ (10,107) ( DATA(J,NO), J=1,5 ), ( DATA(J,NO), J=11,24 ), !JDL 
     1          ( DATA( J,NO ) , J=25,64)                               
      NO = NO + 1                                                       
      GO TO 2                                                           
C****                                                                   
C**** PURE MULTIPOLES                                                   
C**** QUADRUPOLE LENS        TYPE = 3                                   
C**** HEXAPOLE  LENS         TYPE = 4                                   
C**** OCTAPOLE  LENS         TYPE = 5                                   
C**** DECAPOLE  LENS         TYPE = 6                                   
C****                                       
   13 IDATA(NO) = I                                                     
      READ (10,109)( DATA( J,NO ) , J=1,3 ), ( DATA( J,NO ), J=10,30 )   
      NO = NO + 1                                                       
      GO TO 2                                                           
C****   
C****   ELECTROSTATIC DEFLECTOR  TYPE=7
C****
17      IDATA(NO) = 7
        read(10,118) (DATA(J, NO), J=1, 4), (DATA(J, NO), J=11,20),
     1              (DATA(J, NO), J=25,40)
        NO = NO + 1
        GO TO 2
C****                                                                   
C**** VELOCITY SELECTOR      TYPE = 8                                   
C****                                                                   
   18 IDATA(NO) = 8                                                     
      READ (10,115) ( DATA(J,NO),J=1,4), (DATA(J,NO), J=11,51 )          
      NO = NO + 1                                                       
      GO TO 2                                                           
C****                                                                   
C**** MULTIPOLE (POLES)      TYPE =  9                                  
C****                                                                   
   19 IDATA(NO) = 9                                                     
      READ (10,112) ( DATA( J,NO ) , J=1,3 ), ( DATA( J,NO ), J=10,34 )  
      NO = NO + 1                                                       
      GO TO 2                                                           
C****                                                                   
C**** MULTIPOLE LENS         TYPE = 10                                  
C****                                                                   
   20 IDATA(NO) = 10                                                    
      READ (10,111) ( DATA( J,NO ) , J=1,2 ), ( DATA( J,NO ), J=10,17 ), 
     1          ( DATA( J,NO ) , J=20,28 )                              
      NO = NO + 1                                                       
      GO TO 2                                                           
C****                                                                   
C**** SHIFT AND ROTATE       TYPE = 11                                  
C****                                                                   
   21 IDATA(NO) = 11                                                    
      READ (10,100) ( DATA( J,NO ) , J=1,6 )                             
      NO = NO + 1                                                       
      GO TO 2                                                           
C****                                                                   
C**** DRIFT                  TYPE = 12                                  
C****                                                                   
   22 IDATA(NO) = 12                                                    
      READ (10,100) ( DATA( J,NO ) , J=1,1 )                             
      NO = NO + 1                                                       
      GO TO 2                                                           
C****                                                                   
C**** SOLENOID               TYPE = 14                                  
C****                                                                   
   24 IDATA(NO) = 14                                                    
      READ (10,114) (DATA(J,NO),J=1,1), ( DATA(J,NO), J=10,16)           
      NO = NO+1                                                         
      GO TO 2                                                           
C****                                                                   
C**** LENS                   TYPE = 15                                  
C****                                                                   
   25 IDATA(NO) = 15                                                    
      READ (10,100) (DATA(J,NO), J=1,8  )                                
      NO = NO+1                                                         
      GO TO 2                                                           
C**** Changes from here ...                              !JDL 17-NOV-83
C****
C**** CHANGE DATA & REPEAT   TYPE = 16
C****
   26 IDATA(NO) = 16
      NO=NO+1
      IF(NO .GT. 30) GO TO 99
      NLOOP=0
      NPASS=0
      DO 1680 NLP=1,30
      READ (10,1605) NWD, IROW, JCOL, KUPLE, ITITLE(NO), HDATA
 1605 FORMAT(       A4,4X,I2,3X,I2, X,A4,     A4,  6X,  F10.5)
      IF(NWD .EQ. NWORD(1)) GO TO 11   !SENTINEL ENCOUNTERED
      NLOOP=NLOOP+1
      IF(HDATA .EQ. 0.0) HDATA=0.01
      NN=NO-1
      DO 1620 INO=1,NN
      J=IDATA(INO)
      IF((NWD .EQ. NWORD(J)) .AND. 
     1   (ITITLE(NO) .EQ. ITITLE(INO))) GO TO 1630
 1620 CONTINUE
      GO TO 1690
 1630 IF((J .EQ. 4) .OR. (J .EQ. 5) .OR. (J .GT. 10)) J=3
      IF(J .GT. 3) J=J-3   !COMPRESSES NCOLS(I,J) TABLE TO 6 TYPES.
      IJ=0
      IR=IROW-2    !COUNT OF LINES INCLUDES ELEMENT TITLE AS ROW 1.
      IF((IR .LT. 0) .OR. (IR .GT. 10)) GO TO 1690
      IF(IR .EQ. 0) GO TO 1650
      DO 1640 I=1,IR
 1640 IJ=IJ+NCOLS(I,J-1)
 1650 IJ=IJ+JCOL
      IF((IJ .LT. 1) .OR. (IJ .GT. 64)) GO TO 1690
      LOOPSV(1,NLOOP)=INO
      LOOPSV(2,NLOOP)=IROW
      LOOPSV(3,NLOOP)=JCOL
      LOOPSV(4,NLOOP)=KUPLE
      LOOPSV(5,NLOOP)=IJ
      HOOPSV(  NLOOP)=HDATA
 1680 CONTINUE
C****
 1690 PRINT 103, NWD
      CALL EXIT
C****
C**** ... down to here.                                  !JDL 17-NOV-83
C****
C****
C****                                                                   
C**** SYSTEM END             TYPE = 1                                   
C****                                                                   
   11 IDATA(NO) = 1                                                     
C****                                                                   
C**** STANDARD RAYS AUTOMATIC SET-UP                                    
C**** IF( NR .GT. JNR ) APPEND ADDITIONAL RAYS FROM INPUT
C****
C****                                      !From here... !JDL 10-MAR-84
      MCP = 0    !CONTOUR
      MCS = 0    !SPECTRUM
      MEL = 0    !ELLIPSE
        IF (JNR.EQ.0) GO TO 66                                          
        READ (10,100) TMIN,PMIN,XMAX,TMAX,YMAX,PMAX,DMAX                 
        IF( NRXS .NE. 0 )
     1  READ (10,100) STMN,SPMN,SXMX,STMX,SYMX,SPMX,SDMX,SUMX
        CALL RAYS( JNR, NR, NRXS )                       !JDL 10-MAR-84 
C****
C**** SUBROUTINE RAYS CREATES STANDARD RAY SETS WHEN JNR = 2, 6, 14, 46
C**** USING DATA FROM A SINGLE RAY CARD FOLLOWING SENTINEL.  THEN,
C**** IF NR < JNR, RAYTRACE WILL USE ONLY THE FIRST NR OF THE JNR TOTAL.
C**** HOWEVER, IF NR > JNR, THEN ADDITIONAL RAYS WILL BE APPENDED
C**** IN ONE OF THE FOLLOWING OPTIONAL WAYS:
C****
C****    NRXS = 0,  READ (NR-JNR) MORE RAY CARDS FROM INPUT FILE.
C****
C****          1-9, READ ONE MORE RAY CARD FROM INPUT FILE AND
C****               AUTOMATICALLY GENERATE PHASE-SPACE ELLIPSES
C****               CONTAINING (NR-JNR) EQUALLY SPACED RAYS USING
C****               DIAGONAL  SCAN (NRXS=1, X AND Y IN PHASE) OR
C****               CIRCULAR  SCAN (NRXS=2, X LEADS Y BY 90-DEG) OR
C****               ELLIPSOID SCAN (NRXS=3, X ELLIPSE AT EACH Y).
C****
C****        11-19, READ ONE MORE RAY CARD FROM INPUT FILE AND
C****               AUTOMATICALLY GENERATE (NR-JNR) RANDOM RAYS
C****               STARTING FROM A FIXED  SEED (NRXS=11) OR
C****               STARTING FROM A RANDOM SEED (NRXS=12).
C****
C****           13, READ TWO DATA CARDS (TOTAL OF 4 AFTER SENTINEL)
C****               FOR SEED INPUT AND/OR SPECTRUM ENHANCEMENT.
C****
C****        21-29, READ ONE MORE RAY CARD FROM INPUT FILE AND
C****               AUTOMATICALLY GENERATE CONTOUR PLOT FROM A
C****               FIXED GRID EXTENDING (NR-JNR) RAYS EACH SIDE
C****               OF CENTER IN TWO DIMENSIONS (SELECTED BY LPAX)
C****               WITH INCREMENTS FROM THIS CARD.  ANY NON-ZERO
C****               ENTRIES NOT SELECTED AS AXES GIVE FIXED OFFSETS.
C****
C****
C****
      IF((NR .LE. JNR)  .OR.  (NRXS .EQ.  0)) GO TO 66
      IF((NRXS .GE.  1) .AND. (NRXS .LE.  9)) MEL = JNR
      IF((NRXS .GE. 11) .AND. (NRXS .LE. 19)) MCS = NR-JNR
      IF((NRXS .GE. 21) .AND. (NRXS .LE. 29)) MCP = NR-JNR
      SEED = 0.0
      IF((MEL .NE. 0 ) .OR. ( MCP .NE. 0 )) GO TO 52
      IF( MCS .EQ. 0 ) GO TO 66
C****
C**** FOR SPECTRUM ENHANCEMENT OPTION (NRXS = 13)
C**** READ TWO MORE DATA CARDS:
C****      SEED,     DXHW,DTHW,DYHW,DPHW,DEHW,DMHW,
C****      SEC1,SEC2,SEC3,SEC4,SEC5,SEC6,SEC7,SEC8
C****
      IF( NRXS .EQ. 13 ) read(10,119)
     1     SEED,     DXHW,DTHW,DYHW,DPHW,DEHW,DMHW,
     2     SEC1,SEC2,SEC3,SEC4,SEC5,SEC6,SEC7,SEC8
c      ISEED = INT(100.0*SECNDS(0.0))
      ISEED = INT(100.0*SECNDS(0.0))
      dummy = RAN(iseed)
      IF( NRXS .EQ. 11 )  ISEED = 29
      IF(( NRXS .NE. 13 ) .OR. ( SEED .EQ. 0.0 )) GO TO 149
      IF( SEED .LT. 4.0D9 ) ISEED = IDINT( SEED - 1.0D9 )
      IF( SEED .GT. 4.0D9 ) ISEED = IDINT( SEED - 7.0D9 )
  149 SEEP = DFLOAT( ISEED ) + 1.0D9
      IF( ISEED .LT. 0 )  SEEP = SEEP + 6.0D9
      IF( STMN .EQ. 0.0 ) STMN = 0.1  !DEFAULT TO 0.1 CM PER CHANNEL
      GO TO 52
C*JDL IF( JNR .GE. NR ) GO TO 52
C*JDL JNRP = JNR+1
C*JDL DO 49 J=JNRP,NR
C**49 read(10,100,END=60) XI(J),VXI(J),YI(J),VYI(J),ZI(J),VZI(J),
C*JDL1                    DELP(J)
C*JDL   GO TO 52                                                        
C****
C**** INPUT RAYS
C****
   66 CONTINUE
      IF( NR .LE. JNR ) GO TO 52
      JNRP = JNR+1
      NRMX = NR
      IF( NRMX .GT. 100 ) NRMX = 100
      DO 56  J=JNRP,NRMX                    !...to here. !JDL  5-MAR-84 
      read(10,100,END=60 )XI(J),VXI(J),YI(J),VYI(J),ZI(J),VZI(J),DELP(J) 
   56 CONTINUE                                                          
      GO TO 52                                                          
   60 NR = J-1                                                          
   52 CONTINUE                          !From here ...   !JDL 19-NOV-83
      ENERGY = HENR
      PMASS  = HPMS
      PMOM0  = HPMO
      VEL    = HVEL
      VEL0   = VEL
      EN0    = ENERGY
      PM0    = PMASS
      EMASS  = PMASS*931.48
      QMC    = EMASS/(9.0D10*Q0)
      ETOT   = EMASS + EN0
C****
C**** BEGIN ENERGY-STEP LOOP
C****                                   ! ... to here.   !JDL 19-NOV-83
      DO 53 JEN=1,NEN                                    !JDL 19-NOV-83 
C****                                                                   
C****                                                                   
C****                                                                   
      NP = IP                                                           
      IF( (NP .LE. 100)  .OR.  (NP .GE. 200)  ) GO TO 65                
      IF( JEN   .EQ.   (NEN/2+1)  )  NP = IP-100                        
   65 CONTINUE                                                          
      IF( (NP .GT. 100)  .AND.  (JEN .NE. 1) )  GO TO 55                
      IF( (IP .GT. 500)  .AND. (NCSV .GT. 0) )  GO TO 55 !JDL 15-NOV-83
      PRINT 105, NTITLE                                                 
      PRINT 117, DAET, TYME                                             
      VNS = VEL0*1.0D-9                                  !JDL 10-MAR-84
C**
C**
      print 1115, ztgt,atgt,ttgt                          !MP 30-jul-93
      print 1116, foil,tfoil                              !MP 30-jul-93
      print 11165, detgt,1.D3*fwhmtgt
      print 11166, defoil,1.D3*fwhmfl
      print 1117, en00,en01,en0                           !MP 30-jul-93
      PRINT 116, EN0,   NR,    JNR,  HENR,               !JDL 10-MAR-84 
     1           PMOM0, IP,    NPLT, DEN,                !JDL 10-MAR-84
     2           VNS,   NSKIP, NRXS, XNEN,               !JDL 10-MAR-84
     3           PM0,   JFOCAL,LPAX, HPMS,               !JDL 31-OCT-84
     4           Q00,   JMTRX,       Q00,    DMASS      !JDL 10-MAR-84,M
      PRINT 1160,
     1           GAS,AGAS,ZGAS,PRESS,
     2           PMASS,ZION,
     *           qbars,dsqrs0,
     3           QAVER,DLSQR0,
     $           qopt,
     $           TFWopt,thwhm,
     4           Q0,
     *           gasopt,ACAPT,ALOS0,
     5           GSMFP0,SIGC0,SIGT0,
     6           DEDX0
      DO 54  NO = 1,30                                                  
      ITYPE = IDATA(NO)                                                 
      IF( ITYPE .EQ. 1 ) GO TO 151                       !JDL 16-MAR-84 
   54 CALL PRNT( ITYPE, NO )                                            
C****                              !Changes from here... !JDL  6-MAR-84
  151 CALL PRNTA
   55 IF((NP .GT. 100).AND.(IP .LT. 500).AND.(JEN .EQ. 1))PRINT 106!JDL 
      IF(( MCS .NE. 0 ) .OR. ( MCP .NE. 0 )) NR = JNR + 1
      NVAX = MOD( LPAX, 10 )          !VERT AXIS FOR LINEPRINTER PLOT
      NHAX = MOD( LPAX/10, 10 )       !HORZ AXIS
      NCAX = MOD( LPAX/100, 10 )      !CONTOUR AXIS
      IF( NVAX .EQ. 0 ) NVAX = 10
      IF( NHAX .EQ. 0 ) NHAX = 10
      IF((NCAX .EQ. 0 ) .AND. ( MCP .NE. 0 )) NCAX = 10
      IF( LPAX .NE. 0 ) GO TO 1552
      NCAX = 0    !DEFAULT TO ORIGINAL PLOT (THETA-VERSES-X).
      NHAX = 1
      NVAX = 2
      IF( MCP .EQ. 0 ) GO TO 1552
      NCAX = 1    !DEFAULT TO X-CONTOURS IN (PHI-VERSES-THETA) PLANE.
      NHAX = 2
      NVAX = 4
 1552 CONTINUE
C****
C**** BEGIN RAY-TRACE LOOP
C****
      DO 57  J=1,NR                                                     
      IF(J.GT.JNR) JRAY=1
      IF(J .LE. JNR) GO TO 155
      IF(MEL .EQ. 0) GO TO 152
      NM = 3  !ELLIPSES MAY BE DIVIDED INTO 3 MASS GROUPS.
      NS = (NR-JNR+NM-1)/NM
      DELM  = SUMX*FLOAT((J-JNR-1)/NS-1)
      PMASS = (1.0+DELM/100.0)*PM0
  152 IF(( MCS .EQ. 0 ) .AND. ( MCP .EQ. 0 )) GO TO 155
C****
C**** MULTI-CHANNEL SPECTRUM OPTION
C****
C**** TO USE, SET NR = TOTAL RAYS, JNR = 2, 6, 14 OR 46 AND
C**** NRXS = 11 (FIXED SEED) OR 12 (RANDOM START).  AFTER SENTINEL,
C**** PLACE ON SECOND RAY CARD THE FOLLOWING (8F10.5):
C****
C****    STMN = DISPLACEMENT (CM) PER CHANNEL IN SPECTRUM.
C****    SPMN = DISPLACEMENT (CM) AT  CHANNEL 50 (CENTER).
C****    SXMX = SAMPLE LIMITS IN X (CM).
C****    STMX = SAMPLE LIMITS IN THETA (MRAD).
C****    SYMX = SAMPLE LIMITS IN Y (CM).
C****    SPMX = SAMPLE LIMITS IN PHI   (MRAD).
C****    SDMX = SAMPLE LIMITS IN ENERGY (PERCENT).
C****    SUMX = SAMPLE LIMITS IN MASS   (PERCENT) (INTEGERS).
C****
C**** STARTING RAYS ARE SELECTED AT RANDOM FROM WITHIN PLUS/MINUS
C**** RANGE OF SAMPLE LIMITS.  STANDARD RAYS BASED ON CHOICE OF JNR
C**** ARE USED TO DETERMINE FOCAL PLANE AND DO MATRIX ANALYSIS,
C**** BUT ONLY RANDOM RAYS APPEAR IN SPECTRUM LISTINGS AND PLOTS.
C****
      NRAY = MCS
      DO 153 I= NR, 100
      XI(I)  = 0.0
      YI(I)  = 0.0
      ZI(I)  = 0.0
      VXI(I) = 0.0
      VYI(I) = 0.0
      VXI(I) = 0.0
      DELP(I)= 0.0
      XO(I)  = 0.0
      YO(I)  = 0.0
      ZO(I)  = 0.0
      VXO(I) = 0.0
      VYO(I) = 0.0
      VZO(I) = 0.0
      RLL(I) = 0.0
      RTL(I) = 0.0
  153 CONTINUE
      IF( MCP .EQ. 0 ) GO TO 154
C****
C**** CONTOUR-MAP OPTION
C****
C**** TO USE:  SET NRXS=21-29, SELECT CONTOUR AXES USING LPAX, AND
C**** SET CORRESPONDING NON-ZERO GRID RANGES ON CARD 2 AFTER SENTINEL.
C****
C****   LPAX = C H V               (C,H,V)  RAYS      SPECTRA   CONTOURS
C****          : : :
C****          : : :...VERT AXIS        1 = X(CM)     X(CM)       SXMX
C****          : :.....HORZ AXIS        2 = TH(MR)    COUNTS(X)   STMX
C****          :....CONTOUR AXIS        3 = Y(CM)     Y(CM)       SYMX
C****                                   4 = PHI(MR)   COUNTS(Y)   SPMX
C****   NRXS = 21,   1 (a)              5 = DL(CM)    ALL-CNTS(X) SDMX
C****          22,   3 (b)  NUMBER      6 = DT(NS)    COUNTS(T)   SUMX
C****          23,   5 (c)    OF        7 = DE(PCT)   *DE(PCT)    0.0
C****          24,   7 (d) CONTOURS     8 = DM(PCT)   *DM(PCT)    0.0
C****          25,   9 (e)              9 = ENERGY    *ENERGY     0.0
C****          26,  11 (f) (center      0 = MASS      *MASS       0.0
C****          27,  15 (h)  letter)
C****          28,  21 (k)                    * = ABNORMAL USAGE
C****          29,  25 (m)
C****
C****
C****     ...CONTOUR SPACING (CM).
C****     :     ...CONTOUR CENTER (CM).
C****     :     :     .................................GRID MAXIMA
C****     :     :     :     :     :     :     :     :     (SELECT 2).
C****   STMN, SPMN, SXMX, STMX, SYMX, SPMX, SDMX, SUMX
C****     *     *     1     2     3     4     5     6   = LPAX DIGIT.
C****
C****
C****
      IF( STMN .EQ. 0.0 ) STMN = 0.1    !DEFAULT O.1CM INTERVALS
      IF( NHAX .EQ. 5 ) NHAX = 7    !EXTERNAL ENTRIES 5 AND 6
      IF( NVAX .EQ. 5 ) NVAX = 7    !BECOME INTERNAL  7 AND 8.
      IF( NHAX .EQ. 6 ) NHAX = 8
      IF( NVAX .EQ. 6 ) NVAX = 8
      VECJ(1)  = SXMX     !LPAX POINTS TO VARIABLES ON AXES.
      VECJ(2)  = STMX     !ALL OTHERS ARE CONSTANT OFFSETS.
      VECJ(3)  = SYMX
      VECJ(4)  = SPMX
      VECJ(5)  = 0.0
      VECJ(6)  = 0.0
      VECJ(7)  = SDMX
      VECJ(8)  = SUMX
      VECJ(9)  = 0.0
      VECJ(10) = 0.0
      IF( MCP .GT. 8 ) MCP = 8
      IF((MCP .GT. 6 ) .AND. ( JNR .GT. 14 )) MCP = 6
      DVAX = DFLOAT( MCP )
      DHAX = VECJ(NHAX)/DVAX
      DVAX = VECJ(NVAX)/DVAX
      VECJ(NHAX) = 0.0    !AXIS VARIABLES CENTERED ON ZERO.
      VECJ(NVAX) = 0.0
      NCQAD = 4           !SET LOOP INDICES
      NCHOR = MCP + 1
      NCVER = MCP + 1
      NCSAV = JNR + 2
C**** GO TO 154
C****
  154 CONTINUE    !ENTRY POINT FOR RAY-SAMPLING LOOPS  (J=JNR+1)
      DO 1541 I = J,49
      K = 50+J-I
      XI(K)   = XI(K-1)    !PUSH USED INPUT RAYS ONTO FIFO STACK.
      YI(K)   = YI(K-1)    !SAVE THE LAST (50-JNR) RAY SAMPLES.
      ZI(K)   = ZI(K-1)
      VXI(K)  = VXI(K-1)
      VYI(K)  = VYI(K-1)
 1541 DELP(K) = DELP(K-1)
      IF( MCP .NE. 0 ) GO TO 1545
      XI(J)  = SXMX*(2.0*RAN(0)-1.0)    !RANDOM RAYS FOR SPECTRA
      YI(J)  = SYMX*(2.0*RAN(0)-1.0)
      ZI(J)  = 0.0
      VXI(J) = STMX*(2.0*RAN(0)-1.0)
      VYI(J) = SPMX*(2.0*RAN(0)-1.0)
      VZI(J) = 0.0
      DELP(J)= SDMX*(2.0*RAN(0)-1.0)
      DELM   = SUMX*(2.0*RAN(0)-1.0)
      PMASS  = PM0+ANINT(PM0*DELM/100.0)
      GO TO 155
 1545 XI(J)  = VECJ(1)    !RAYS FOR CONTOUR GRID
      YI(J)  = VECJ(3)
      ZI(J)  = 0.0
      VXI(J) = VECJ(2)
      VYI(J) = VECJ(4)
      VZI(J) = 0.0
      DELP(J)= VECJ(7)
      DELM   = VECJ(8)
      PMASS  = PM0*( 1.0 + DELM/100.0 )
      DHAX = DABS( DHAX )
      DVAX = DABS( DVAX )
      IF(( NCQAD .EQ. 2 ) .OR. ( NCQAD .EQ. 3 )) DHAX = -DHAX
      IF(( NCQAD .EQ. 3 ) .OR. ( NCQAD .EQ. 4 )) DVAX = -DVAX
C**** GO TO 155
  155 CONTINUE    !RESUME NORMAL PATH
      IF (JRAY.NE.0.AND.DMASS.NE.0.)
     1        PMASS = PM0 + 0.5*DMASS*RANDOM(1,ISEED)
      IXS = 0
      XSCTR = 0.0
      ZSCTR = 0.0
      EMASS = PMASS*931.48
      TOLD = 0.
      NSK1 = 0
      NSK2 = 0
      TOLD2 = 0
      GASL = 0.
      NPASG = 0
      tau0 = 0.
      icalc = 0
C****                                  !...down to here. !JDL  6-MAR-84
      ENERGY = (1.+DELP(J)/100. ) *EN0                                  
      IF (JRAY.NE.0.AND.DEN.NE.0.)
     1  ENERGY = EN0 + 0.5*DEN*RANDOM(1,ISEED)
      ETOT = EMASS + ENERGY                                             
      VEL = ( DSQRT( (2.*EMASS + ENERGY) *ENERGY) /ETOT)*C              
      PMOM =  DSQRT( (2.*EMASS + ENERGY) *ENERGY)                       
C***  bypasses previous change of energy                            !MP 
C   
C***  initializes energy to input value
C
      energy = en00
C
C***  includes now energy loss + straggling in target              !MP 1
C
      if (jray.ne.0) 
     1 energy = energy - detgt + 0.5*fwhmtgt*random(1,iseed)       !MP 1
C
C***  includes now energy loss + straggling in foil                !MP 1
C***  using Zeff,Aeff values for foil and tfoil                    !MP 1
C
      if(jray.ne.0)
     1  energy = energy - defoil + 0.5*fwhmfl*random(1,iseed)      !MP 1
C
C
C***  following initializing lines copied from elsewhere           !MP 1
C
C
      GASENE = ENERGY                                    !***MP 1-JAN-85
      enold = energy
      EMASS = PMASS*931.48                                              
      ETOT = EMASS + ENERGY                                             
      VEL = ( DSQRT( ( 2.*EMASS + ENERGY)*ENERGY) / ETOT ) * C          
      VEL0 = VEL                                                        
      EN0 = ENERGY                                                      
      PM0 = PMASS                                        !JDL 12-MAR-84
      PMOM0 = DSQRT( (2.*EMASS + EN0)*EN0)                              
C


      Q0 = Q00
      IF (Q00.GT.0) GO TO 1542
c
c     initial charge distribution in solid target ( entrance window )
c
c      CALL sQDIST
c      QHWHM = DSQRT(DELSQR)*1.17741
c 1543 Q0 = QBAR + QHWHM*RANDOM(1,ISEED)
c      Q0 = DNINT(Q0)
c
c     sample charge distribution (Sayer formula, setup by SDQDIST)
c
 1543 call sqsamp
      IF (Q0.GT.ZION.OR.Q0.LE.0.) GO TO 1543		! should never h
      IF (JRAY.EQ.0) Q0 = DNINT(qbars)
 1542 CONTINUE
      QMC = EMASS/(9.D10*Q0)
      K = (Q0/ETOT)*9.D10
C****
      T = 0.
      JRAYGAS = JRAY*GAS
      IF (JRAYGAS.EQ.0) GO TO 1551
c
c     charge distribution in gas
c
      enold = energy
      CALL QDIST
      if (gasopt.le.0.) call sigcap
      DELQ0 = Q0 - QBAR
      CALL QSIG(DELQ0)
      call discol0			! initialise collision distances
      if (tfwopt.lt.0.) call smangsc
      DEDX = FDEDX(Q0)
      dedxq = dedx*(q0/qbar)**2
1551  CONTINUE
      NUM = 0
      XA = XI(J)                                                        
      YA = YI(J)                                                        
      ZA = ZI(J)                                                        
      VXA =VEL*DSIN( VXI(J)/1000. ) * DCOS( VYI(J)/1000. )              
      VYA =VEL*DSIN( VYI(J)/1000. )                                     
      VZA =VEL*DCOS( VXI(J)/1000. ) * DCOS( VYI(J)/1000. )              
      XDVEL = (VEL-VEL0)*100./VEL0                                      
      DELTP = (PMOM-PMOM0)*100./PMOM0                                   
      IF( NP .LE. 100) PRINT 108,J, ENERGY,PMOM,VEL,DELP(J),DELTP,XDVEL 
      DO 50 NO =1,30                                                    
      ITYPE = IDATA(NO )                                                
      GO TO(31,32,33,33,33,33,37,38,39,40,41,42,43,44,45,46),ITYPE !JDL 
   43 CALL EXIT                                          !JDL 17-NOV-83 
C****                                                                   
C****                                                                   
   32 CALL DIPOLE ( NO, NP, T, TP ,NUM )                                
      GO TO 51                                                          
   33 NCODE = ITYPE-2                                                   
      CALL MULTPL ( NO, NP, T, TP ,NUM )                                
      GO TO 51                                                          
37      IVEC = 1
        CALL EDIPL(NO, NP, T, TP, NUM)
        IVEC = 0
        GO TO 51
   38 IVEC = 1                                                          
      CALL VELS   ( NO, NP, T, TP ,NUM )                                
      IVEC = 0                                                          
      GO TO 51                                                          
   39 CALL POLES  ( NO, NP, T, TP ,NUM )                                
      GO TO 51                                                          
   40 CALL MULT   ( NO, NP, T, TP ,NUM )                                
      GO TO 51                                                          
   41 CALL SHROT  ( NO, NP, T, TP ,NUM )                                
      GO TO 50                                                          
   42 CALL DRIFT  ( NO, NP, T, TP ,NUM )                                
      GO TO 50                                                          
   44 CALL SOLND  ( NO, NP, T, TP ,NUM )                                
      GO TO 51                                                          
   45 CALL LENS   ( NO, NP, T, TP ,NUM )                                
      GO TO 50                                                          
   46 GO TO 50                                           !JDL 17-NOV-83
   51 XA = TC(1)                                                        
      YA = TC(2)                                                        
      ZA = TC(3)                                                        
      VXA= TC(4)                                                        
      VYA= TC(5)                                                        
      VZA= TC(6)                                                        
   50 CONTINUE                                                          
   31 CONTINUE                                                          
      CALL OPTIC( J, JFOCAL, NP, T, TP )                                
      IF (LPLT ) CALL PLTOUT ( JEN, J, NUM )
C****                              !Changes from here... !JDL  6-MAR-84
      IF(((MCS .EQ. 0) .AND. (MCP .EQ. 0)) .OR. (J .LE. JNR)) GO TO 57
      IF( MCP .NE. 0 ) GO TO 250
      ZI(J) = XSCTR    !CENTER COORDINATES AT SLITS.
C**** COUNT THE RAYS THAT PASS BETWEEN SLITS IN DIPOLE MAGNET.
      IF(IXS .EQ.  0) DELP(88) = DELP(88)+1.0  !ERROR, SLITS NOT FOUND
      IF(IXS .EQ. -2) DELP(91) = DELP(91)+1.0  !RAY LOW  IN X
      IF(IXS .EQ. +1) DELP(92) = DELP(92)+1.0  !RAY THRU SLITS
      IF(IXS .EQ. -1) DELP(93) = DELP(93)+1.0  !RAY HIGH IN X
      DELP(100) = DELP(100)+1.0                !ALL RAYS
C****
C**** SORT RAYS INTO MULTI-CHANNEL SPECTRA.
C**** IF NRXS=13, APPLY SPECTRUM ENHANCEMENT TO X-COORDINATE
C**** BASED ON KNOWN CORRELATION OF X WITH THETA AND ENERGY.
C**** INCLUDE RANDOM MEASUREMENT UNCERTAINTIES:
C****      DXHW(CM) = HALF-WIDTH OF GAUSSIAN NOISE IN X.
C****      DTHW(MR) = HALF-WIDTH OF GAUSSIAN NOISE IN THETA.
C****      DEHW(%)  = HALF-WIDTH OF GAUSSIAN NOISE IN ENERGY.
C****
c
c write additional multi-channel spectrum file of final state of particl
c to use other programs for graphic disply.
c x0,y0 [cm]  coordinates in focal plane
c GASENE [MeV] final energy of particle
c q final charge state
c t*1E9 [nsec] time of flight throughout instrument
c 
      WRITE(3,3333) XO(J),yo(j),GASENE,q0,t*1.E9
 3333 FORMAT(5F10.4)
      IF(NRXS .NE. 13) GO TO 256
      K=1
      XOJ  = XO(J)
      VXOJ = VXO(J)
      DEOJ = 100.0*(ENERGY - EN0)/EN0
      IF(DXHW .NE. 0.0) XOJ  = XOJ +DXHW*RANDOM(1,ISEED)
      IF(DTHW .NE. 0.0) VXOJ = VXOJ+DTHW*RANDOM(1,ISEED)
      IF(DEHW .NE. 0.0) DEOJ = DEOJ+DEHW*RANDOM(1,ISEED)
      VXOT = VXOJ +  SEC1*XOJ
      XO(J) = XOJ + (SEC2*VXOT*VXOT+SEC3*VXOT*DEOJ+SEC4*DEOJ*DEOJ)
     1            + (SEC5*VXOT*VXOT+SEC6*VXOT*DEOJ+SEC7*DEOJ*DEOJ)*VXOT
     2            + (                              SEC8*DEOJ*DEOJ)*DEOJ
  256 CONTINUE
      IX=INT((XO(J)-SPMN)/STMN+50.5)
      IY=INT((YO(J)     )/STMN+50.5)
      IZ=INT((RTL(J)*1.0D+09/VEL)/STMN+50.5)
      IF((IX .GE.  1) .AND. (IX .LE.  50)) XI(IX+50) = XI(IX+50) +1.0
      IF((IX .GE. 51) .AND. (IX .LE. 100)) VXI(IX)   = VXI(IX)   +1.0
      IF(IXS .LT.  0)  GO TO 156   !RAY HIT A SLIT
      IF((IX .GE.  1) .AND. (IX .LE.  50)) XO(IX+50) = XO(IX+50) +1.0
      IF((IX .GE. 51) .AND. (IX .LE. 100)) VXO(IX)   = VXO(IX)   +1.0
      IF((IY .GE.  1) .AND. (IY .LE.  50)) YO(IY+50) = YO(IY+50) +1.0
      IF((IY .GE. 51) .AND. (IY .LE. 100)) VYO(IY)   = VYO(IY)   +1.0
      IF((IZ .GE.  1) .AND. (IZ .LE.  50)) RLL(IZ+50)= RLL(IZ+50)+1.0
      IF((IZ .GE. 51) .AND. (IZ .LE. 100)) RTL(IZ)   = RTL(IZ)   +1.0
      IF((IX .GE.  1) .AND. (IX .LE. 100))
     1                DELP(97) = DELP(97)+1.0  !RAY WITHIN SPECTRUM
      IF(IX  .LT.  1) DELP(96) = DELP(96)+1.0  !RAY BELOW  SPECTRUM
      IF(IX  .GT.100) DELP(98) = DELP(98)+1.0  !RAY ABOVE  SPECTRUM
  156 NRAY=NRAY-1
      IF(NRAY .GT. 0) GO TO 154   !LOCK TO J=(JNR+1) FOR NRAY PASSES
      GO TO 57
C**** GENERATE RAYS IN RECTANGULAR GRID FOR CONTOUR MAPPING.
  250 CONTINUE
      VECK(1)  = XO(J)
      VECK(2)  = VXO(J)
      VECK(3)  = YO(J)
      VECK(4)  = VYO(J)
      VECK(5)  = RLL(J)
      VECK(6)  = RTL(J)
      VECK(7)  = DELP(J)
      VECK(8)  = DELM
      VECK(9)  = ENERGY
      VECK(10) = MASS
      XOJ = ( VECK(NCAX) - SPMN ) / STMN     !RE-SCALE TO CONTOUR STEPS
      IF( NCQAD .EQ. 1 ) YO(NCSAV)  = XOJ    !SAVE BY QUADRANTS
      IF( NCQAD .EQ. 2 ) VYO(NCSAV) = XOJ
      IF( NCQAD .EQ. 3 ) RLL(NCSAV) = XOJ
      IF( NCQAD .EQ. 4 ) RTL(NCSAV) = XOJ
      NCSAV = NCSAV + 1
      VECJ(NHAX) = VECJ(NHAX) + DHAX
      NCHOR = NCHOR - 1
      IF( NCHOR .GT. 0 ) GO TO 154
      NCHOR = MCP + 1
      VECJ(NHAX) = 0.0
      VECJ(NVAX) = VECJ(NVAX) + DVAX
      NCVER = NCVER - 1
      IF( NCVER .GT. 0 ) GO TO 154
      NCVER = MCP + 1
      VECJ(NVAX) = 0.0
      NCSAV = JNR + 2
      NCQAD = NCQAD - 1
      IF( NCQAD .GT. 0 ) GO TO 154
C**** GO TO 57
C****                                  !...down to here. !JDL  6-MAR-84
   57 CONTINUE                                                          
C****
C**** END RAY-TRACE LOOP
C****
      IF(MCS .NE. 0) NR = JNR                            !JDL  6-MAR-84
      ENERGY = EN0                                                      
      PMASS  = PM0                                       !JDL 12-MAR-84
      VEL = VEL0
      IF( NP .GT. 100 ) GO TO 59                                        
      PRINT 105, NTITLE                                                 
      PRINT 117, DAET,TYME                                              
      PRINT 116, EN0,   NR,    JNR,  HENR,               !JDL 10-MAR-84 
     1           PMOM0, IP,    NPLT, DEN,                !JDL 10-MAR-84
     2           VNS,   NSKIP, NRXS, XNEN,               !JDL 10-MAR-84
     3           PM0,   JFOCAL,LPAX, HPMS,               !JDL 31-OCT-84
     4           Q00,   JMTRX,       Q00,   DMASS       !JDL 10-MAR-84,M
 1160 FORMAT(////,'          GAS =',F10.0,'  AT/MOL', 5X,
     1                                'AGAS   =', F10.0, 13X,
     *                                'ZGAS   =', F10.0, 13x,
     1                                'PRESS  =   ',F10.2,'  TORR',/
     2            '         AION =',F10.0,           13X,
     3                                'ZION   =', F10.0,//
     *            ' window:   Qbar =   ',f10.3, 10x,
     *                                'Dsqr   =    ', f10.4,/
     4            ' gas   :   Qbar =   ',F10.3, 10X,
     5                                'Dsqr   =    ',F10.4,/,
     $            '           QDist formula = ',F5.0,
     $' (0.=Dmitriev;1.=Betz;2.=Rehm;3.=Ninov (low vel.);4.=Schiwietz)'/
     1            '       A1/2 opt =',   F10.3,10X,
     *                                'THWHM  =  ',d10.4,'  RAD',/  
     6            '           q0   =',   F10.0,/
     *            '        gas opt =',   F10.3,10X,
     *                                'ACAP   =    ',F10.4, 9X,
     7                                'ALOS0  =    ',F10.4,/
     8            '           MFP0 =   ',F10.3,'  CM ',5X,
     9                                'SIGC0  =',D10.3, ' A0SQR', 7x,
     *                                'SIGT0  =',D10.3, ' A0SQR', /
     1            '          DEDX0 =   ',F10.3,' MEV/(MG/CM2)'/)
      DO 58 NO =1,30                                                    
      ITYPE = IDATA(NO )                                                
      IF ( ITYPE  .EQ.  1 ) GO TO 158                    !JDL 16-MAR-84 
   58 CALL PRNT( ITYPE, NO )                                            
  158 CALL PRNTA                                         !JDL 16-MAR-84
   59 CONTINUE                                                          
C****                              !Changes from here... !JDL 10-MAR-84
C*JDL IF( NSKIP .NE. 0 ) GO TO 61                                       
C*JDL IF( NR  .GE.  46  )  GO TO 62                                     
C*JDL IF( NR  .GE.  14  )  GO TO 63                                     
C*JDL IF( NR  .GE.   6  )  GO TO 64                                     
      IF( NSKIP .NE. 0  )  GO TO 161
      XO1SV  = XO(1)
      YO1SV  = YO(1)
      VXO1SV = VXO(1)
      VYO1SV = VYO(1)
      DO 162 I = 1, NR
      XO(I)  = XO(I)  - XO1SV   !TO GIVE MATRICES NORMALIZED DATA
      YO(I)  = YO(I)  - YO1SV
      VXO(I) = VXO(I) - VXO1SV
  162 VYO(I) = VYO(I) - VYO1SV
      MM = NR
      IF( JNR .NE. 0) MM = JNR !MIT PROTOCOL ALSO REQUIRES (NRXS .NE. 0)
      IF( MM  .GE.  46  )  GO TO 62
      IF( MM  .GE.   2  )  GO TO 63
C****                                  !...down to here. !JDL 10-MAR-84
      GO TO 61                                                          
   62 CALL MATRIX(R,T2)                                                 
      GO TO 61                                                          
   63 IF((IP .GT. 500) .AND. (NEN .GT. 1)) GO TO 163     !JDL 21-NOV-83
      PRINT 105, NTITLE                                                 
      PRINT 117, DAET, TYME                                             
  163 CALL MTRX1( MM, JEN, NEN, NR, ENERGY, PMASS  )     !JDL 10-MAR-84 
      LNEN = 1                                                          
      GO TO 61                                                          
C**64 IF((IP .GT. 500) .AND. (NEN .GT. 1)) GO TO 164     !JDL 21-NOV-83
C*JDL PRINT 105, NTITLE                                                 
C*JDL PRINT 117, DAET, TYME                                             
C*164 CALL MTRX1( 1, JEN, NEN, NR, ENERGY, PMASS  )      !JDL 10-NOV-83 
C*JDL LNEN = 1                                                          
C****                              !Changes from here... !JDL  6-MAR-84
   61 CONTINUE
      DO 166 I = 1, NR
      XO(I)  = XO(I)  + XO1SV   !RESTORE OUTPUT ARRAYS
      YO(I)  = YO(I)  + YO1SV
      VXO(I) = VXO(I) + VXO1SV
  166 VYO(I) = VYO(I) + VYO1SV
  161 IF(MCS .EQ. 0) GO TO 261
C****
C**** 100-CHANNEL SPECTRA REPLACE RAYS IN PRINTED OUTPUT:
C****
C**** LABEL    ARRAY         CONTENT
C****
C****  X     = XI(I)   = STANDARD INPUT RAYS FROM JNR = 2, 6, 14, OR 46.
C****  THETA = VXI(I)  = (USED TO DETERMINE FOCAL PLANE ORIENTATION AND
C****  Y     = YI(I)   =  TO DO MATRIX ANALYSIS BASED ON STANDARD RAYS.
C****  PHI   = VYI(I)  =  THESE RAYS NOT INCLUDED IN SPECTRA.)
C****  ZI    = ZI(I)   =
C****  DELE  = DELP(I) = ERR-88;SX1-91,THRU-92,SX2-93,LO-96,CTS-97,HI-98
C****  XO    = XO(I)   = CHANNEL VALUE IN X (CM).
C****  XS    = VXO(I)  = COUNTS PER X-CHANNEL (RAYS THRU SLITS).
C****  YO    = YO(I)   = CHANNEL VALUE IN Y (CM).
C****  YS    = VYO(I)  = COUNTS PER Y-CHANNEL (RAYS THRU SLITS).
C****  L(CM) = RLL(I)  = COUNTS PER X-CHANNEL (ALL RAYS).
C****  T(NS) = RTL(I)  = COUNTS PER T-CHANNEL (RAYS THRU SLITS).
C****
      NR=100
      DO 160 I=1,100
      IF(I .GT. 50) GO TO 159
      VXO(I)=XO(I+50)  !UNFOLD SPECTRA
      VYO(I)=YO(I+50)
      RTL(I)=RLL(I+50)
      RLL(I)=XI(I+50)
      RLL(I+50)=VXI(I+50)
      XI(I+50) =0.0
      VXI(I+50)=0.0
  159 YO(I)=FLOAT(I-50)*STMN
      XO(I)=YO(I)+SPMN
  160 CONTINUE
  261 N1 = MEL + 1
C****                                  !...down to here. !JDL  6-MAR-84
      CALL PRNT1 ( N1, NR, JEN, NEN, WIDTH )             !JDL  1-NOV-84 
      EN0 = EN0 + DEN                                                   
      ENERGY = EN0                                                      
C**** PM0 = PM0 + DMASS                !MP FEB 85        !JDL 10-NOV-83
      PMASS = PM0                                        !JDL 10-NOV-83
      EMASS = PMASS*931.48                               !JDL 10-NOV-83
      QMC = EMASS/(9.0D10*Q0)                            !JDL 10-NOV-83
      ETOT = EMASS + EN0                                                
      VEL0 = ( DSQRT( ( 2.*EMASS + EN0)*EN0 ) /ETOT)*C                  
      PMOM0 = DSQRT( (2.*EMASS + EN0)*EN0)                              
   53 CONTINUE                                                          
C****
C**** END ENERGY-STEP LOOP
C****
C**** Changes from here ...                              !JDL 31-OCT-84
      IF( LNEN .EQ. 0 )  GO TO 9600                      !JDL  1-DEC-83 
      IF((NEN .EQ. 1 ) .AND. (NLOOP .EQ. 0)) GO TO 9600
      PRINT 105, NTITLE                                                 
C      CALL TIME(TYME)
      PRINT 117, DAET, TYME                                             
C*IBM CALL WHEN(DAET)
      CALL MPRNT( NEN, WIDTH )                           !JDL  1-DEC-83 
 9600 CONTINUE
      IF( MCS .EQ. 0 ) GO TO 9602
      NH = NHAX
      NV = NVAX
      IF( NHAX .EQ. 5 ) NHAX = 12    !CHANGE SPECTRUM LABEL TO COUNTS
      IF( NVAX .EQ. 5 ) NVAX = 12
      IF((NHAX .EQ. 2) .OR. (NHAX .EQ. 4) .OR. (NHAX .EQ. 6)) NHAX = 11
      IF((NVAX .EQ. 2) .OR. (NVAX .EQ. 4) .OR. (NVAX .EQ. 6)) NVAX = 11
      IF((NHAX .EQ. 1) .AND. (NV  .EQ. 6)) NHAX = 6    !LABEL DT(NS)
      IF((NVAX .EQ. 1) .AND. (NH  .EQ. 6)) NVAX = 6
      NCAX = 0
 9602 JEN = NEN
      CALL PRNT1A( JEN )
C*JDL PRINT 106                                          !JDL  1-DEC-83 
C****
C****
C**** CHANGE DATA & REPEAT   ITYPE = 16   (EXECUTION)
C****
      IF((NLOOP .EQ. 0) .OR. (LNEN .EQ. 0)) GO TO 5
      IF(NPASS .EQ. 0) GO TO 9650
 9610 KUPLE=LOOPSV(4,NPASS)
      IF(KUPLE .EQ. 0) GO TO 9650
      DO 9620 J=NPASS,NLOOP
      IF(LOOPSV(4,J) .NE. KUPLE) GO TO 9620
      IF((KUPLE .EQ. 4    ) .AND. (J .NE. NPASS)) GO TO 9620
      IF(SPASS .EQ. 0.0) LOOPSV(4,J)=0
      INO=LOOPSV(1,J)
      IJ= LOOPSV(5,J)
      IF(SPASS .GT. 0.0) HSAVE(J)=DATA(IJ,INO)
      DATA(IJ,INO)=HSAVE(J)+SPASS*HOOPSV(J)
 9620 CONTINUE
C****
      IF(SPASS) 9640,9650,9630
C****
 9630 SPASS=-1.0     !JUST DID PLUS  SETUP (MINUS SETUP NEXT)
      GO TO 52       !GO DO POSITIVE-INCREMENT PASS
C****
 9640 SPASS= 0.0     !JUST DID MINUS SETUP (RESET COMES NEXT)
      GO TO 52       !GO DO NEGATIVE-INCREMENT PASS
C****
 9650 SPASS=+1.0     !JUST DID A RESET (PLUS SETUP NEXT)
      NPASS=NPASS+1
      IF(NPASS .LE. NLOOP) GO TO 9610
C****
C**** ... down to here.                                  !JDL 17-NOV-83
      GO TO 5                                                           
cddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
      close(11)
      close(2)
      close(3)
      close(4)
      close(10)
      stop
cddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd      
      END     
      
                                                                
      SUBROUTINE BDIP                                                   
C****                                                                   
C****                                                                   
C**** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION                 
C**** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE           
C**** MTYP=3  :    NONUNIFORM FIELD STANDARD APPROXIMATION              
C**** MTYP=4  :    NONUNIFORM FIELD  B=BF/(1+N*DR/R)                    
C**** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION                  
C****                                                                   
C**** THE RELATIONSHIP BETWEEN B0, ......... B12 AND B(I,J) RELATIVE TO 
C**** AXES (Z,X) IS GIVEN BY                                            
C****                                                                   
C****                                                                   
C****                                                                   
C**** B0  = B( 0, 0 )                                                   
C**** B1  = B( 1, 0 )                                                   
C**** B2  = B( 2, 0 )                                                   
C**** B3  = B( 1, 1 )                                                   
C**** B4  = B( 1,-1 )                                                   
C**** B5  = B( 0, 1 )                                                   
C**** B6  = B( 0, 2 )                                                   
C**** B7  = B( 0,-1 )                                                   
C**** B8  = B( 0,-2 )                                                   
C**** B9  = B(-1, 0 )                                                   
C**** B10 = B(-2, 0 )                                                   
C**** B11 = B(-1, 1 )                                                   
C**** B12 = B(-1,-1 )                                                   
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8  NDX, BX, BY, BZ, K, TC, DTC
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT,CSC                          
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8,SCOR           
      COMMON  /BLCK22/  D, DG, S, BF, BT                                
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK24/  RB, XC, ZC                                      
      COMMON  /BLCK25/  IN, MTYP                                        
      DIMENSION TC(6), DTC(6)                                           
      DIMENSION XX(11), ZZ(11), DD(11)                                  
      DATA PI2   / 1.570796325D0       /                                
      DATA PI4   / .7853981625D0       /                                
      DATA RT2   /  1.41421356D0       /                                
C****                                                                   
C****                                                                   
      GO TO ( 10,10,6,6,10 )     ,MTYP                                  
      CALL EXIT                                                         
      RETURN                                                            
    6 CALL NDIP                                                         
      RETURN                                                            
C****                                                                   
C**** MTYP = 1 , 2, 5                                                   
C**** UNIFORM FIELD MAGNETS                                             
C****                                                                   
   10 CONTINUE                                                          
      GO TO( 2, 1, 2, 4 ) , IN                                          
    7 PRINT 8, IN                                                       
    8 FORMAT(  35H0 ERROR -GO TO -  IN BFUN   IN=        I5  )          
    1 BX = 0.                                                           
      BY = BF                                                           
      BZ = 0.                                                           
      BT = BF                                                           
      RETURN                                                            
    2 X = TC(1)                                                         
      Y = TC(2)                                                         
      Z = TC(3)                                                         
      IF( MTYP .NE. 2 ) GO TO 9                                         
C****                                                                   
C**** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE           
C****                                                                   
      XP = X                                                            
      XP2 = XP*XP                                                       
      XP3 = XP2*XP                                                      
      XP4 = XP3 * XP                                                    
      ZP = -(S2*XP2 + S3*XP3 + S4*XP4 + S5*XP4*XP + S6*XP4*XP2 +        
     1       S7*XP4*XP3 + S8*XP4*XP4 )                                  
      AZ = (Z-ZP)/5.D0                                                  
      DO 11 I=1,11                                                      
      XP    = X + AZ*(I-6)                                              
      XP2 = XP*XP                                                       
      XP3 = XP2*XP                                                      
      XP4 = XP3*XP                                                      
      ZP = -(S2*XP2 + S3*XP3 + S4*XP4 + S5*XP4*XP + S6*XP4*XP2 +        
     1       S7*XP4*XP3 + S8*XP4*XP4 )                                  
      XXP = X-XP                                                        
      ZZP = Z-ZP                                                        
      XX(I) = XP                                                        
      ZZ(I) = ZP                                                        
      DD(I) = DSQRT( XXP*XXP + ZZP*ZZP )                                
   11 CONTINUE                                                          
C****                                                                   
C**** SEARCH FOR SHORTEST OF THE 11 DISTANCES                           
C****                                                                   
      XP = XX(1)                                                        
      ZP = ZZ(1)                                                        
      DP = DD(1)                                                        
      DO 12 I=2,11                                                      
      IF( DD(I) .GE. DP ) GO TO 12                                      
      XP = XX(I)                                                        
      ZP = ZZ(I)                                                        
      DP = DD(I)                                                        
   12 CONTINUE                                                          
C****     
C****  DIVIDE INTERVAL AND REPEAT FOR MORE EXACT
C****  SHORTEST DISTANCE.
C****
      AZ = AZ/5.D0
      X1 = XP
      DO 13 I=1,11                                                      
      XP    = X1+ AZ*(I-6)                                              
      XP2 = XP*XP                                                       
      XP3 = XP2*XP                                                      
      XP4 = XP3*XP                                                      
      ZP = -(S2*XP2 + S3*XP3 + S4*XP4 + S5*XP4*XP + S6*XP4*XP2 +        
     1       S7*XP4*XP3 + S8*XP4*XP4 )                                  
      XXP = X-XP                                                        
      ZZP = Z-ZP                                                        
      XX(I) = XP                                                        
      ZZ(I) = ZP                                                        
      DD(I) = DSQRT( XXP*XXP + ZZP*ZZP )                                
   13 CONTINUE                                                          
C****                                                                   
C**** SEARCH FOR SHORTEST OF THE 11 DISTANCES                           
C****                                                                   
      XP = XX(1)                                                        
      ZP = ZZ(1)                                                        
      DP = DD(1)                                                        
      DO 15 I=2,11                                                      
      IF( DD(I) .GE. DP ) GO TO 15                                      
      XP = XX(I)                                                        
      ZP = ZZ(I)                                                        
      DP = DD(I)                                                        
   15 CONTINUE                                                          
C****                                                                   
C**** ITERATION LOOP FOR MORE EXACT SHORTEST DISTANCE                   
C****                                                                   
C*    ZSIGN = Z-ZP                                                      
C*    XP2 = XP*XP                                                       
C*    XP3 = XP2*XP                                                      
C*    XP4 = XP3*XP                                                      
C*    DO 13 I=1,3                                                       
C****                                                                   
C**** SLOPE OF CURVE AT XP, ZP                                          
C****                                                                   
C*    DZDXC = -(2.*S2*XP + 3.*S3*XP2+ 4.*S4*XP3 + 5.*S5*XP4 +           
C*   1   6.*S6*XP4*XP + 7.*S7*XP4*XP2 + 8.*S8*XP4*XP3 )                 
C****                                                                   
C**** NEXT APPROXIMATION TO CLOSEST POINT IS                            
C****                                                                   
C*    XP = ( DZDXC*(Z-ZP)  +  DZDXC*DZDXC*XP + X ) / (1.+DZDXC*DZDXC)   
C*    IF( I  .EQ.  1 )  XP = (3.*XP +  X ) / 4.                         
C*    XP2 = XP*XP                                                       
C*    XP3 = XP2*XP                                                      
C*    XP4 = XP3*XP                                                      
C*    ZP = -( S2*XP2 + S3*XP3 + S4*XP4 + S5*XP4*XP + S6*XP4*XP2 +       
C*   1   S7*XP4*XP3 + S8*XP4*XP4 )                                      
C* 13 CONTINUE                                                          
C*    XXP = X-XP                                                        
C*    ZZP = Z-ZP                                                        
C****
C****
      ZSIGN = Z-ZP
      XP2 = XP*XP
      XP3 = XP2*XP
      XP4 = XP3*XP
      S = DSIGN( 1.D0,ZSIGN) * DP                        / D - DELS     
      SCON = S                                                          
C****                                                                   
C**** TRIM CORRECTION FOR EFFECTIVE EDGE CURVATURE                      
C****                                                                   
      DZDXC = -(2.*S2*XP + 3.*S3*XP2+ 4.*S4*XP3 + 5.*S5*XP4 +           
     1   6.*S6*XP4*XP + 7.*S7*XP4*XP2 + 8.*S8*XP4*XP3 )                 
      DZDXC2= -(2.*S2+6.*S3*XP +12.*S4*XP2 +20.*S5*XP3 +30.*S6*XP4 +    
     1         42.*S7*XP4*XP +56.*S8*XP4*XP2 )                          
      RCR = DZDXC2 / DSQRT( 1.D0 + DZDXC*DZDXC ) **3                    
      S = S + SCOR*D*RCR                                                
      S0 = S                                                            
      CALL BDPP( B0, Z, X, Y )                                          
      IF( Y .NE. 0. ) GO TO 14                                          
      BX = 0.                                                           
      BY = B0                                                           
      BZ = 0.                                                           
      BT = B0                                                           
      RETURN                                                            
   14 GD = DG/D                                                         
      DELTA = DATAN(DZDXC)                                              
      SCON = (1.D0 + SCON*D*RCR) *GD*DG*RCR/2.D0                        
      DCS = DCOS( DELTA         )                                       
      S = S0-  SCON*( 1.D0 - DCS*DCS ) +      GD*DCS                    
      CALL BDPP( B1 , Z, X, Y )                                         
      S =S0-4.*SCON*( 1.D0 - DCS*DCS ) +   2.*GD*DCS                    
      CALL BDPP( B2 , Z, X, Y )                                         
      S = S0-  SCON*( 1.D0 - DCS*DCS ) -      GD*DCS                    
      CALL BDPP( B9 , Z, X, Y )                                         
      S =S0-4.*SCON*( 1.D0 - DCS*DCS ) -   2.*GD*DCS                    
      CALL BDPP( B10, Z, X, Y )                                         
      DCS = DCOS( DELTA + PI4   )                                       
      S =S0-2.*SCON*( 1.D0 - DCS*DCS ) +  RT2*GD*DCS                    
      CALL BDPP( B3 , Z, X, Y )                                         
      S =S0-2.*SCON*( 1.D0 - DCS*DCS ) -  RT2*GD*DCS                    
      CALL BDPP( B12, Z, X, Y )                                         
      DCS = DCOS( DELTA - PI4   )                                       
      S =S0-2.*SCON*( 1.D0 - DCS*DCS ) +  RT2*GD*DCS                    
      CALL BDPP( B4 , Z, X, Y )                                         
      S =S0-2.*SCON*( 1.D0 - DCS*DCS ) -  RT2*GD*DCS                    
      CALL BDPP( B11, Z, X, Y )                                         
      DCS = DCOS( DELTA + PI2   )                                       
      S = S0-  SCON*( 1.D0 - DCS*DCS ) +      GD*DCS                    
      CALL BDPP( B5 , Z, X, Y )                                         
      S =S0-4.*SCON*( 1.D0 - DCS*DCS ) +   2.*GD*DCS                    
      CALL BDPP( B6 , Z, X, Y )                                         
      S = S0-  SCON*( 1.D0 - DCS*DCS ) -      GD*DCS                    
      CALL BDPP( B7 , Z, X, Y )                                         
      S =S0-4.*SCON*( 1.D0 - DCS*DCS ) -   2.*GD*DCS                    
      CALL BDPP( B8 , Z, X, Y )                                         
      GO TO 5                                                           
    9 CALL BDPP ( B0, Z, X, Y )                                         
      S0 = S                                                            
      IF( Y .NE. 0. )   GO TO 3                                         
      BX = 0.                                                           
      BY = B0                                                           
      BZ = 0.                                                           
      BT = B0                                                           
      RETURN                                                            
    3 CALL BDPP ( B1 , Z + DG, X , Y )                                  
      CALL BDPP ( B2 , Z + 2.*DG, X , Y )                               
      CALL BDPP ( B3 , Z + DG, X + DG , Y )                             
      CALL BDPP ( B4 , Z + DG, X - DG , Y )                             
      CALL BDPP ( B5 , Z , X + DG , Y )                                 
      CALL BDPP ( B6 , Z , X + 2.*DG , Y )                              
      CALL BDPP ( B7 , Z , X - DG , Y )                                 
      CALL BDPP ( B8 , Z , X - 2.*DG , Y )                              
      CALL BDPP ( B9 , Z - DG, X , Y )                                  
      CALL BDPP ( B10, Z - 2.*DG, X , Y )                               
      CALL BDPP ( B11, Z - DG, X + DG , Y )                             
      CALL BDPP ( B12, Z - DG, X - DG , Y )                             
    5 CONTINUE                                                          
      S = S0                                                            
      YG1 = Y/DG                                                        
      YG2 = YG1**2                                                      
      YG3 = YG1**3                                                      
      YG4 = YG1**4                                                      
      BX = YG1 * ( (B5-B7)*2./3. - (B6-B8)/12. )  +                     
     1     YG3*( (B5-B7)/6. - (B6-B8)/12. -                             
     2     (B3 + B11 - B4 - B12 - 2.*B5 + 2.*B7 ) / 12. )               
      BY = B0 - YG2*( ( B1 + B9 + B5 + B7 - 4.*B0 ) *2./3. -            
     1     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. ) +                     
     2     YG4* (-( B1 + B9 + B5 + B7 - 4.*B0 ) / 6. +                  
     3     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. +                       
     4     ( B3 + B11 + B4 + B12 - 2.*B1 - 2.*B9 -                      
     5     2.*B5 - 2.*B7 + 4.*B0 ) / 12. )                              
      BZ = YG1*( (B1 - B9 ) *2./3. - ( B2 - B10 ) /12. ) +              
     1     YG3*( ( B1 - B9 ) / 6. - ( B2 - B10 ) / 12. -                
     2     ( B3 + B4 - B11 - B12 - 2.*B1 + 2.*B9 ) / 12.  )             
      BT = DSQRT(BX*BX + BY*BY + BZ*BZ)                                 
      RETURN                                                            
    4 BX = 0.                                                           
      BY = BR                                                           
      BZ = 0.                                                           
      BT = BR                                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE  BDPP ( BFLD, Z, X, Y )                                
C****                                                                   
C****                                                                   
C****                                                                   
C**** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION                 
C**** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE           
C****              MORE ACCURATE 3'RD AND HIGHER ORDER CURVATURES       
C**** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION                  
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8  NDX, BX, BY, BZ, K, TC, DTC 
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT,CSC                          
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8,SCOR           
      COMMON  /BLCK22/  D, DG, S, BF, BT                                
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK24/  RB, XC, ZC                                      
      COMMON  /BLCK25/  IN, MTYP                                        
      DIMENSION TC(6), DTC(6)                                           
C****                                                                   
      GO TO (10,13,6,6,11 ) ,MTYP                                       
    6 CALL EXIT                                                         
      RETURN                                                            
C****                                                                   
C**** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION                 
C****                                                                   
   10 X2=X*X                                                            
      X3=X*X2                                                           
      X4=X*X3                                                           
      S = (    Z   +S2*X2 + S3*X3 + S4*X4 + S5*X*X4 + S6*X2*X4 +        
     1   S7*X3*X4 + S8*X4*X4  ) / D - DELS                              
      GO TO 13                                                          
C****                                                                   
C**** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION                  
C****                                                                   
   11 IF( DABS(RCA)  .GE. 1.D-08  ) GO TO 12                            
      S = Z/D - DELS                                                    
      GO TO 13                                                          
   12 A = 1./RCA                                                        
      S = ( DSIGN(1.D0,A) * DSQRT( (Z+A)**2 + X*X ) - A ) / D - DELS    
   13 CS=C0+S*(C1+S*(C2+S*(C3+S*(C4+S*C5))))                            
      IF( DABS(CS)  .GT.  70.  )  CS =DSIGN( 70.D0 ,CS  )               
      E=DEXP(CS)                                                        
      P0 = 1.0 + E                                                      
      DB=BF-BR                                                          
      BFLD=BR + DB/P0                                                   
C****
C**** PRINT 100, X, Y, Z,  DR, S, BFLD
C*100 FORMAT( 1P6D15.4 )
C****
      RETURN                                                            
      END                                                               
      SUBROUTINE BEFN (F,Z,X,Y,IBEX)                                    
C****                                                                   
C****                                                                   
C****    CALCULATES S, THEN DETERMINES B (OR E) FIELD.                  
C****                                                                   
C****                                                                   
      IMPLICIT  REAL*8 (A-H,O-Z)                                        
      COMMON /BLCK71/  CB0,CB1,CB2,CB3,CB4,CB5                          
      COMMON /BLCK72/  CE0,CE1,CE2,CE3,CE4,CE5                          
      COMMON /BLCK73/  IN                                               
      COMMON /BLCK74/  BF,EF,S,DG                                       
      COMMON /BLCK75/  BC2,BC4,EC2,EC4                                  
      COMMON /BLCK76/  DB,DE,WB,WE                                      
C****                                                                   
      IF (IBEX .NE. 0 ) GO TO 10                                        
      F1 = BF                                                           
      D = DB                                                            
      C02 = BC2                                                         
      C04 = BC4                                                         
      W2 = WB*WB                                                        
      C0 = CB0                                                          
      C1 = CB1                                                          
      C2 = CB2                                                          
      C3 = CB3                                                          
      C4 = CB4                                                          
      C5 = CB5                                                          
      GO TO 20                                                          
C****                                                                   
   10 F1 = EF                                                           
      IF( IN .EQ. 1 ) F1 = -EF                                          
      D = DE                                                            
      C02 = EC2                                                         
      C04 = EC4                                                         
      W2 = WE*WE                                                        
      C0 = CE0                                                          
      C1 = CE1                                                          
      C2 = CE2                                                          
      C3 = CE3                                                          
      C4 = CE4                                                          
      C5 = CE5                                                          
   20 ZD1 = Z/D                                                         
      ZD2 = C02*ZD1*X*X/W2                                              
      W4 = W2*W2                                                        
      ZD3 = C04*(X**4)/W4                                               
      S = ZD1+ZD2+ZD3                                                   
      CS = C0+S*(C1+S*(C2+S*(C3+S*(C4+S*C5))))                          
      IF ( DABS(CS) .GT. 70. ) CS = DSIGN ( 70.D0,CS )                  
      E = DEXP(CS)                                                      
      P0 = 1.0+E                                                        
      F = F1/P0                                                         
      RETURN                                                            
      END                                                               
      SUBROUTINE BEVC                                                   
C****                                                                   
C****  CALCULATES B AND E FIELDS                                        
C****                                                                   
C****                                                                   
      IMPLICIT     REAL*8 (A-H,O-Z)                                     
      REAL*8 BX, BY, BZ, K, TC, DTC
      COMMON /BLCK10/  BX, BY, BZ, K, TC, DTC                           
      COMMON /BLCK11/  EX, EY, EZ, QMC, IVEC                            
      COMMON /BLCK71/  CB0,CB1,CB2,CB3,CB4,CB5                          
      COMMON /BLCK72/  CE0,CE1,CE2,CE3,CE4,CE5                          
      COMMON /BLCK73/  IN                                               
      COMMON /BLCK74/  BF,EF,S,DG                                       
      DIMENSION TC(6),DTC(6),BEF(3)                                     
C****                                                                   
      GO TO (2,1,2) , IN                                                
      PRINT  100,IN                                                     
  100 FORMAT (  35H0 ERROR -GO TO -  IN BFUN   IN=     I5 )             
    1 BX = 0.                                                           
      BY = BF                                                           
      BZ = 0.                                                           
      EX = EF                                                           
      EY = 0.                                                           
      EZ = 0.                                                           
      RETURN                                                            
C****                                                                   
C**** IN THE FRINGE:  FIND B AND E FIELDS                               
C****                                                                   
    2 X = TC(1)                                                         
      Y = TC(2)                                                         
      Z = TC(3)                                                         
      IF ( Y .EQ. 0. ) GO TO 3                                          
      CALL BEY( BEF,Z,X,Y,0 )                                           
      BX = BEF(1)                                                       
      BY = BEF(2)                                                       
      BZ = BEF(3)                                                       
      GO TO 4                                                           
C****                                                                   
    3 CALL BEFN(B0,Z,X,Y,0)                                             
      BX = 0.                                                           
      BY = B0                                                           
      BZ = 0.                                                           
C****                                                                   
C**** NOW FIND E FIELD                                                  
C****                                                                   
    4 IF ( X .EQ. 0 ) GO TO 5                                           
      CALL BEY( BEF,Z,Y,X,1 )                                           
      EX = BEF(2)                                                       
      EY = BEF(1)                                                       
      EZ = BEF(3)                                                       
      RETURN                                                            
    5 CALL BEFN ( B1,Z,Y,X,1 )                                          
      EX = B1                                                           
      EY = 0.                                                           
      EZ = 0.                                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE BEY (BEF,Z,X,Y,IBEX )                                  
C****                                                                   
C**** CALCULATE B OR E FIELD OFF THE MEDIAN PLANE                       
C****                                                                   
C****                                                                   
      IMPLICIT  REAL*8 (A-H,O-Z)                                        
      COMMON /BLCK74/  BF,EF,S,DG                                       
      DIMENSION BEF(3)                                                  
C****                                                                   
      CALL BEFN(F0,Z,X,Y,IBEX )                                         
      CALL BEFN(F1,Z+DG,X,Y,IBEX )                                      
      CALL BEFN(F2,Z+2.*DG,X,Y,IBEX )                                   
      CALL BEFN(F3,Z+DG,X+DG,Y,IBEX )                                   
      CALL BEFN(F4,Z+DG,X-DG,Y,IBEX )                                   
      CALL BEFN(F5,Z   ,X+DG,Y,IBEX )                                   
      CALL BEFN(F6,Z,X+2.*DG,Y,IBEX )                                   
      CALL BEFN(F7,Z,X-DG,Y,IBEX )                                      
      CALL BEFN(F8,Z,X-2.*DG,Y,IBEX )                                   
      CALL BEFN(F9,Z-DG,X,Y,IBEX )                                      
      CALL BEFN(F10,Z-2.*DG,X,Y,IBEX )                                  
      CALL BEFN(F11,Z-DG,X+DG,Y,IBEX )                                  
      CALL BEFN(F12,Z-DG,X-DG,Y,IBEX )                                  
C****                                                                   
      YG1 = Y/DG                                                        
      YG2 = YG1**2                                                      
      YG3 = YG1**3                                                      
      YG4 = YG1**4                                                      
C****                                                                   
      BEF(1) = YG1 * ( (F5-F7)*2./3. - (F6-F8)/12. ) +                  
     1         YG3 * ( (F5-F7)/6. - (F6-F8)/12. -                       
     2         ( F3 + F11 - F4 - F12 - 2.*F5 + 2.*F7 )/12. )            
      BEF(2) = F0 - YG2*( (F1 + F9 + F5 + F7 - 4.*F0) * 2./3. -         
     1         ( F2 + F10 + F6 + F8 - 4.*F0 )/24. ) +                   
     2         YG4 * (-( F1 + F9 + F5 + F7 - 4.*F0 )/6. +               
     3         ( F2 + F10 +      F6 + F8 - 4.*F0 )/24. +                
     4         ( F3 + F11 + F4 + F12 - 2.*F1 - 2.*F9 -                  
     5         2.*F5 - 2.*F7 + 4.*F0 )/12. )                            
      BEF(3) = YG1 * ( (F1 - F9)*2./3. - (F2 - F10)/12. ) +             
     1         YG3 * ( (F1 - F9)/6. - (F2 - F10)/12. -                  
     2         (F3 + F4 - F11 - F12 - 2.*F1 + 2.*F9)/12. )              
      RETURN                                                            
      END                                                               
      SUBROUTINE BFLD                                                   
C****                                                                   
C**** CALCULATION OF FIELD COMPONENTS FOR EACH PURE MULTIPOLE           
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8 BX, BY, BZ, K, TC, DTC
      COMMON  /BLCK 7/ NCODE                                            
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK50/  D, GRAD, S, BT                                  
      COMMON  /BLCK51/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK52/  IN                                              
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      DIMENSION TC(6), DTC(6)                                           
      JRAYGAS = JRAY*GAS
      X = TC(1)                                                         
      Y = TC(2)                                                         
      Z = TC(3)                                                         
      GO TO ( 11, 12, 13, 14 ) , NCODE                                  
C****                                                                   
C**** QUADRUPOLE                                                        
C****                                                                   
   11 CONTINUE                                                          
      GO TO ( 2, 1, 2 ) , IN                                            
      PRINT 3, IN                                                       
    3 FORMAT( '  ERROR IN BQUAD  IN= ' I5 ///)                          
      CALL EXIT                                                         
    1 BX = GRAD*Y                                                       
      BY = GRAD*X                                                       
      BZ = 0.                                                           
      BT =   DSQRT( BX*BX + BY*BY )                                     
      RETURN                                                            
    2 S = Z/D                                                           
      CS = C0 + C1*S + C2*S**2 + C3*S**3 + C4*S**4 + C5*S**5            
      CSP = C1 + 2.*C2*S + 3.*C3*S**2 + 4.*C4*S**3 + 5.*C5*S**4         
      CSPP = 2.*C2 + 6.*C3*S + 12.*C4*S**2 + 20.*C5*S**3                
      IF( DABS(CS) .GT. 70. )  CS = DSIGN(70.D0, CS )                   
      E = DEXP(CS)                                                      
      RE = 1./(1. + E)                                                  
      CB1 = GRAD*RE                                                     
      CB2 = CB1*E*RE*( CSP**2 + CSPP - 2.*E*RE*CSP**2 )/(12.*D*D )      
      BX = CB1*Y + CB2*( 3.*X*X + Y*Y ) * Y                             
      BY = CB1*X + CB2*( 3.*Y*Y + X*X ) * X                             
      BZ = -CB1*E*CSP*RE*X*Y / D                                        
      BT =   DSQRT( BX*BX + BY*BY + BZ*BZ )                             
      RETURN                                                            
C****                                                                   
C**** HEXAPOLE                                                          
C****                                                                   
   12 BA2 = GRAD                                                        
      GO TO ( 22, 21, 22 ) , IN                                         
      PRINT 23, IN                                                      
   23 FORMAT( '  ERROR IN BHEX   IN= ' I5 ///)                          
      CALL EXIT                                                         
   21 BX = 2.*BA2*X*Y                                                   
      BY = BA2*( X*X - Y*Y )                                            
      BZ = 0.                                                           
      BT =   DSQRT( BX*BX + BY*BY )                                     
      RETURN                                                            
   22 S = Z/D                                                           
      IF( S .LT. 0. ) GO TO 21                                          
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      RETURN                                                            
C****                                                                   
C**** OCTAPOLE                                                          
C****                                                                   
   13 BA3 = GRAD                                                        
      GO TO ( 32, 31, 32 ) , IN                                         
      PRINT 33, IN                                                      
   33 FORMAT( '  ERROR IN BOCT   IN= ' I5 ///)                          
      CALL EXIT                                                         
   31 BX = BA3*( 3.*X*X*Y - Y**3 )                                      
      BY = BA3*( X**3 - 3.*X*Y*Y )                                      
      BZ = 0.                                                           
      BT =   DSQRT( BX*BX + BY*BY )                                     
      RETURN                                                            
   32 S = Z/D                                                           
      IF( S .LT. 0. ) GO TO 31                                          
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      RETURN                                                            
C****                                                                   
C**** DECAPOLE                                                          
   14 BA4 = GRAD                                                        
      GO TO ( 42, 41, 42 ) , IN                                         
      PRINT 43, IN                                                      
   43 FORMAT( '  ERROR IN BDEC   IN= ' I5 ///)                          
      CALL EXIT                                                         
   41 BX = 4.D0*BA4*( X**3 *Y - X*(Y**3) )                              
      BY = BA4*( X**4 - 6.D0* X*X*Y*Y + Y**4  )                         
      BZ = 0.                                                           
      BT =   DSQRT( BX*BX + BY*BY )                                     
      RETURN                                                            
   42 S = Z/D                                                           
      IF( S .LT. 0. ) GO TO 41                                          
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE BMULT                                                  
C****                                                                   
C****                                                                   
C**** THE RELATIONSHIP BETWEEN B0, ......... B12 AND B(I,J) RELATIVE TO 
C**** AXES (Z,X) IS GIVEN BY                                            
C****                                                                   
C****                                                                   
C****                                                                   
C**** B0  = B( 0, 0 )                                                   
C**** B1  = B( 1, 0 )                                                   
C**** B2  = B( 2, 0 )                                                   
C**** B3  = B( 1, 1 )                                                   
C**** B4  = B( 1,-1 )                                                   
C**** B5  = B( 0, 1 )                                                   
C**** B6  = B( 0, 2 )                                                   
C**** B7  = B( 0,-1 )                                                   
C**** B8  = B( 0,-2 )                                                   
C**** B9  = B(-1, 0 )                                                   
C**** B10 = B(-2, 0 )                                                   
C**** B11 = B(-1, 1 )                                                   
C**** B12 = B(-1,-1 )                                                   
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8  BX, BY, BZ, K, TC, DTC, L
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                         
      COMMON  /BLK100/  W, L, D, DG, S, BF, BT                          
      COMMON  /BLK101/  C0, C1, C2, C3, C4, C5, C6, C7, C8              
      DIMENSION TC(6), DTC(6)                                           
      X = TC(1)                                                         
      Y = TC(2)                                                         
      Z = TC(3)                                                         
      CALL MLTT ( B0, Z, X, Y )                                         
      CALL MLTT ( B1 , Z + DG, X , Y )                                  
      CALL MLTT ( B2 , Z + 2.*DG, X , Y )                               
      CALL MLTT ( B3 , Z + DG, X + DG , Y )                             
      CALL MLTT ( B4 , Z + DG, X - DG , Y )                             
      CALL MLTT ( B5 , Z , X + DG , Y )                                 
      CALL MLTT ( B6 , Z , X + 2.*DG , Y )                              
      CALL MLTT ( B7 , Z , X - DG , Y )                                 
      CALL MLTT ( B8 , Z , X - 2.*DG , Y )                              
      CALL MLTT ( B9 , Z - DG, X , Y )                                  
      CALL MLTT ( B10, Z - 2.*DG, X , Y )                               
      CALL MLTT ( B11, Z - DG, X + DG , Y )                             
      CALL MLTT ( B12, Z - DG, X - DG , Y )                             
      YG1 = Y/DG                                                        
      YG2 = YG1**2                                                      
      YG3 = YG1**3                                                      
      YG4 = YG1**4                                                      
      BX = YG1 * ( (B5-B7)*2./3. - (B6-B8)/12. )  +                     
     1     YG3*( (B5-B7)/6. - (B6-B8)/12. -                             
     2     (B3 + B11 - B4 - B12 - 2.*B5 + 2.*B7 ) / 12. )               
      BY = B0 - YG2*( ( B1 + B9 + B5 + B7 - 4.*B0 ) *2./3. -            
     1     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. ) +                     
     2     YG4* (-( B1 + B9 + B5 + B7 - 4.*B0 ) / 6. +                  
     3     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. +                       
     4     ( B3 + B11 + B4 + B12 - 2.*B1 - 2.*B9 -                      
     5     2.*B5 - 2.*B7 + 4.*B0 ) / 12. )                              
      BZ = YG1*( (B1 - B9 ) *2./3. - ( B2 - B10 ) /12. ) +              
     1     YG3*( ( B1 - B9 ) / 6. - ( B2 - B10 ) / 12. -                
     2     ( B3 + B4 - B11 - B12 - 2.*B1 + 2.*B9 ) / 12.  )             
      BT  =DSQRT(BX*BX + BY*BY + BZ*BZ)                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE BPOLES                                                 
C****                                                                   
C**** CALCULATION OF MULTIPOLE(POLES) FIELD COMPONENTS                  
C****                                                                   
C****                                                                   
C****                                                                   
C**** 2 - QUADRUPOLE  (GRAD1)                                           
C**** 3 - HEXAPOLE    (GRAD2)                                           
C**** 4 - OCTAPOLE    (GRAD3)                                           
C**** 5 - DECAPOLE    (GRAD4)                                           
C**** 6 - DODECAPOLE  (GRAD5)                                           
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8 BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK90/  D, S, BT, GRAD1,GRAD2,GRAD3,GRAD4,GRAD5         
      COMMON  /BLCK91/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK92/  IN                                              
      DIMENSION TC(6), DTC(6)                                           
      X = TC(1)                                                         
      Y = TC(2)                                                         
      Z = TC(3)                                                         
      X2 = X*X                                                          
      X3 = X2*X                                                         
      X4 = X3*X                                                         
      X5 = X4*X                                                         
      Y2 = Y*Y                                                          
      Y3 = Y2*Y                                                         
      Y4 = Y3*Y                                                         
      Y5 = Y4*Y                                                         
      GO TO ( 2, 1, 2 ) , IN                                            
      PRINT 3, IN                                                       
    3 FORMAT( '  ERROR IN BPOLES IN= ' I5 ///)                          
      CALL EXIT                                                         
    1 CONTINUE                                                          
      B2X = GRAD1*Y                                                     
      B2Y = GRAD1*X                                                     
      B3X = GRAD2*2.*X*Y                                                
      B3Y = GRAD2*(X2-Y2)                                               
      B4X = GRAD3*(3.*X2*Y-Y3)                                          
      B4Y = GRAD3*(X3-3.*X*Y2)                                          
      B5X = GRAD4*4.*(X3*Y-X*Y3)                                        
      B5Y = GRAD4*(X4-6.*X2*Y2+Y4)                                      
      B6X = GRAD5*(5.*X4*Y-10.*X2*Y3+Y5)                                
      B6Y = GRAD5*(X5-10.*X3*Y2+5.*X*Y4)                                
      BX = B2X + B3X + B4X + B5X + B6X                                  
      BY = B2Y + B3Y + B4Y + B5Y + B6Y                                  
      BZ = 0.                                                           
      BT =   DSQRT( BX*BX + BY*BY )                                     
      RETURN                                                            
    2 S = Z/D                                                           
      CS = C0 + C1*S + C2*S**2 + C3*S**3 + C4*S**4 + C5*S**5            
      CP1 =(C1 + 2.*C2*S + 3.*C3*S**2 + 4.*C4*S**3 + 5.*C5*S**4) / D    
      CP2 = (2.*C2 + 6.*C3*S + 12.*C4*S**2 + 20.*C5*S**3  ) / (D*D)     
      CP3 = ( 6.*C3 + 24.*C4*S + 60.*C5*S*S ) / (D**3)                  
      CP4 = ( 24.*C4 + 120.*C5*S ) / (D**4)                             
      IF( DABS(CS) .GT. 70. )  CS = DSIGN(70.D0, CS )                   
      E = DEXP(CS)                                                      
      RE = 1./(1. + E)                                                  
      ERE = E*RE                                                        
      ERE2= ERE*ERE                                                     
      ERE3= ERE*ERE2                                                    
      ERE4= ERE*ERE3                                                    
      CP12 = CP1*CP1                                                    
      CP22 = CP2*CP2                                                    
      CP13 = CP1**3                                                     
      CP14 = CP1**4                                                     
      G1 = -CP1*ERE*RE                                                  
      G2 =-( CP2+CP12   )*ERE*RE    + 2.*CP12 * ERE2*RE                 
      G3 =-(CP3 + 3.*CP1*CP2 + CP13  ) * ERE*RE    +                    
     1   6.*(CP1*CP2 + CP13)*ERE2*RE - 6.*CP13*ERE3*RE                  
      G4 = -(CP4 + 4.*CP1*CP3 + 3.*CP22 + 6.*CP12*CP2 + CP14)*ERE*RE+   
     1   (8.*CP1*CP3 + 36.*CP12*CP2 + 6.*CP22 + 14.*CP14)*ERE2*RE -     
     2   36.*(CP12*CP2 + CP14)*ERE3*RE    + 24.*CP14*ERE4*RE            
      B2X = GRAD1*( RE*Y - (G2/12.)*(3.*X2*Y + Y3) +                    
     1   (G4/384.)*(5.*X4*Y + 6.*X2*Y3 + Y5 ) )                         
      B2Y = GRAD1*( RE*X - (G2/12.)*(X3 + 3.*X*Y2) +                    
     1   (G4/384.)*(X5 + 6.*X3*Y2 + 5.*X*Y4 ) )                         
      B2Z = GRAD1*( G1*X*Y - (G3/12.)*(X3*Y + X*Y3 ) )                  
      B3X = GRAD2*( RE*2.*X*Y - (G2/48.)*(12.*X3*Y + 4.*X*Y3 ) )        
      B3Y = GRAD2*( RE*(X2-Y2) - (G2/48.)*(3.*X4 + 6.*X2*Y2 - 5.*Y4 ) ) 
      B3Z = GRAD2*( G1*(X2*Y - Y3/3.) - (G3/48.)*(3.*X4*Y+2.*X2*Y3-Y5)) 
      B4X = GRAD3*( RE*(3.*X2*Y - Y3) - (G4/80.)*(20.*X4*Y - 4.*Y5 ) )  
      B4Y = GRAD3*( RE*(X3 - 3.*X*Y2) - (G4/80.)*(4.*X5-20.*X*Y4 ) )    
      B4Z = GRAD3*G1*(X3*Y - X*Y3 )                                     
      B5X = GRAD4*RE*(4.*X3*Y - 4.*X*Y3)                                
      B5Y = GRAD4*RE*(X4 - 6.*X2*Y2 + Y4 )                              
      B5Z = GRAD4*G1*(X4*Y - 2.*X2*Y3 + Y5/5. )                         
      B6X = GRAD5*RE*(5.*X4*Y - 10.*X2*Y3 + Y5 )                        
      B6Y = GRAD5*RE*(X5 - 10.*X3*Y2 + 5.*X*Y4 )                        
      B6Z = 0.                                                          
      BX = B2X + B3X + B4X + B5X + B6X                                  
      BY = B2Y + B3Y + B4Y + B5Y + B6Y                                  
      BZ = B2Z + B3Z + B4Z + B5Z + B6Z                                  
      BT =   DSQRT( BX*BX + BY*BY + BZ*BZ )                             
      RETURN                                                            
      END                                                               
      SUBROUTINE BSOL                                                   
C****                                                                   
C****                                                                   
C**** ROUTINE VALID FOR FIELDS OUTSIDE CENTRAL ZONE OF ELEMENTAL        
C**** SOLENOID                                                          
C**** BF    = FIELD AT CENTER OF INFINITE SOLENOID; CURR. DEN. (NI/M)   
C**** M.W.GARRETTT  JOURNAL OF APP. PHYS. 34,(1963),P2567               
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8 BX, BY, BZ, K, TC, DTC  
      DIMENSION  TC(6), DTC(6)                                          
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK30/  BF ,      AL, RAD                               
      COMMON  /BLCK31/  S, BT                                           
      COMMON  /BLCK32/  IN                                              
C****                                                                   
C****                                                                   
      DATA PI4/12.566370616D0 /                                         
C****                                                                   
C****                                                                   
C****                                                                   
      X = TC(1)                                                         
      Y = TC(2)                                                         
      Z = TC(3)                                                         
      R =DSQRT( X **2 + Y**2 )                                          
      IF( R  .LT.  (RAD/1.D4)  )  GO TO 5                               
      RADR = RAD+R                                                      
      AAPR = 4.D0*RAD/RADR                                              
      AAMR = (RAD-R)/(2.D0*RAD)                                         
      RCSQ = 4.D0*RAD*R/(RADR*RADR)                                     
C****                                                                   
C**** SOLENOID LEFT  HAND SOURCE                                        
C****                                                                   
      ZZ = -(AL+Z)                                                      
      R1SQ = RADR*RADR  + ZZ*ZZ                                         
      R1 = DSQRT(R1SQ)                                                  
      RKSQ = 4.D0*RAD*R/R1SQ                                            
      CALL FB01AD(RKSQ,       VKS, VES )                                
      CALL FB03AD(RCSQ, RKSQ, P )                                       
      BZS1 = AAPR*ZZ*(VKS+AAMR*(P-VKS) ) /R1                            
      BRS1 = R1*(2.D0*(VKS-VES) - RKSQ*VKS)                             
C****                                                                   
C**** SOLENOID RIGHT HAND SOURCE                                        
C****                                                                   
      ZZ = AL-Z                                                         
      R1SQ = RADR*RADR  + ZZ*ZZ                                         
      R1 = DSQRT(R1SQ)                                                  
      RKSQ = 4.D0*RAD*R/R1SQ                                            
      CALL FB01AD(RKSQ,       VKS, VES )                                
      CALL FB03AD(RCSQ, RKSQ, P )                                       
      BZS2 = AAPR*ZZ*(VKS+AAMR*(P-VKS) ) /R1                            
      BRS2 = R1*(2.D0*(VKS-VES) - RKSQ*VKS)                             
      BZ = BF*( BZS2-BZS1 )/PI4                                         
      BR = BF*( BRS2-BRS1 )/(R*PI4)                                     
      BX = BR * X /R                                                    
      BY = BR *  Y/R                                                    
      BT =DSQRT( BX**2 + BY**2 + BZ**2 )                                
      RETURN                                                            
    5 CONTINUE                                                          
C****                                                                   
C****                                                                   
C****                                                                   
      COSA = (AL-Z) / DSQRT( RAD*RAD + (AL-Z)**2  )                     
      COSB =-(AL+Z) / DSQRT( RAD*RAD + (AL+Z)**2  )                     
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = BF*(COSA-COSB)/2.D0                                          
      BT = DABS(BZ)                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE  DERIV( BFUN,T,bflag )                                 
C****                                                                   
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      integer bflag
      REAL*8  BX, BY, BZ, K, TC, DTC 
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC                           
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK61/  DEDX,ALPHAK,TOLD2,DEDXQ
      COMMON  /BLCK62/  GASENE,GASVEL,TOLD,GASL
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt
c      include 'rtcomm65.f'
      DIMENSION DTCGAS(3)
      DIMENSION TC(6), DTC(6)                                           
      DATA  C /3.D10 /,NSK2MX/10/                                       
C****                                                                   
C****                                                                   
C****GASK = RHOGAS(MG/CM**3)*C**2(CM2/SEC2)/(3.*EMASS(MEV))     !***MP 1
      if (bflag.ne.0) then
      CALL BFUN                                                         
      endif
      DTC(1) = TC(4)                                                    
      DTC(2) = TC(5)                                                    
      DTC(3) = TC(6)                                                    
      IF( IVEC .NE. 0 )  GO TO 4                                        
      DTC(4) = K * ( TC(5) * BZ - TC(6) * BY )                          
      DTC(5) = K * ( TC(6) * BX - TC(4) * BZ )                          
      DTC(6) = K * ( TC(4) * BY - TC(5) * BX )                          
      IF (GAS*JRAY.EQ.0.)RETURN                               !***MP 1-J
      VEL = DSQRT( TC(4)**2 + TC(5)**2 + TC(6)**2 )           !***
      GAMMA = 1./DSQRT(1.-VEL*VEL/(C*C) )                     !***
      G3= GAMMA**3                                            !***
      GASENE = EMASS*( GAMMA-1.)                              !***
      ENERGY = GASENE
200   DTCGAS(1) = -GASK*TC(4)*DEDXQ         / (VEL*G3)        !***
      DTCGAS(2) = -GASK*TC(5)*DEDXQ         / (VEL*G3)        !***
      DTCGAS(3) = -GASK*TC(6)*DEDXQ         / (VEL*G3)        !***
      DTC(4) = DTC(4) + DTCGAS(1)
      DTC(5) = DTC(5) + DTCGAS(2)                             !***
      DTC(6) = DTC(6) + DTCGAS(3)                             !***MP 1-J
      RETURN                                                            
    4 VEL = DSQRT( TC(4)**2 + TC(5)**2 + TC(6)**2 )                     
      GAMMA = 1./DSQRT( 1.-VEL*VEL/(C*C) )                              
      K = 1./(QMC*GAMMA)                                                
      AK = K/(9.D13)                                                    
      ETERM = (EX*TC(4)+EY*TC(5)+EZ*TC(6) )*AK                          
      DTC(4) = K*( TC(5)*BZ - TC(6)*BY + EX*1.D7 ) - TC(4)*ETERM        
      DTC(5) = K*( TC(6)*BX - TC(4)*BZ + EY*1.D7 ) - TC(5)*ETERM        
      DTC(6) = K*( TC(4)*BY - TC(5)*BX + EZ*1.D7 ) - TC(6)*ETERM        
      RETURN                                                            
      END                                                               
      SUBROUTINE DIPOLE ( NO, NP, T, TP ,NUM )                          
C****                                                                   
C****                                                                   
C**** SINGLE MAGNET RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIAL
C**** EQUATIONS OF MOTION.                                              
C     T = TIME                                                          
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
c      REAL*4 DAET, TYME                                  !JDL 31-OCT-84
      REAL*8  LF1, LF2, LU1, K, NDX,bx,by,bz,tc,dtc                      
      EXTERNAL BDIP                                                     
      include 'rtcomm0.f'
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT,CSC                          
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8,SCOR           
      COMMON  /BLCK22/  D, DG, S, BF, BT                                
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK24/  RB, XC, ZC                                      
      COMMON  /BLCK25/  IN, MTYP                                        
      COMMON  /BLCK26/  IXS, XSCTR, ZSCTR                !JDL 16-MAR-84
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
C*JDL DIMENSION DATA(  75,30 ), ITITLE(30)               !JDL 17-NOV-83 
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             
C**** DATA  C/ 3.D10/                                                   
C****                                                                   
      JRAYGAS = JRAY*GAS
      LF1  = DATA(  1,NO )                                              
      LU1  = DATA(  2,NO )                                              
      LF2  = DATA(  3,NO )                                              
      DG   = DATA(  4,NO )                                              
      MTYP = DATA(  5,NO )                                              
      A    = DATA( 11,NO )                                              
      B    = DATA( 12,NO )                                              
      D    = DATA( 13,NO )                                              
      RB   = DATA( 14,NO )                                              
      BF   = DATA( 15,NO )                                              
      PHI  = DATA( 16,NO )                                              
      ALPHA= DATA( 17,NO )                                              
      BETA = DATA( 18,NO )                                              
      NDX  = DATA( 19,NO )                                              
      BET1 = DATA( 20,NO )                                              
      GAMA = DATA( 21,NO )                                              
      DELT = DATA( 22,NO )                                              
      XS1  = DATA( 23,NO )                               !JDL  6-MAR-84
      XS2  = DATA( 24,NO )                               !JDL  6-MAR-84
      Z11  = DATA( 25,NO )                                              
      Z12  = DATA( 26,NO )                                              
      Z21  = DATA( 27,NO )                                              
      Z22  = DATA( 28,NO )                                              
      BR1  = DATA( 41,NO )                                              
      BR2  = DATA( 42,NO )                                              
      XCR1 = DATA( 43,NO )                                              
      XCR2 = DATA( 44,NO )                                              
      IF( MTYP .EQ. 0  )  MTYP = 1                                      
      DTF1= LF1/ VEL                                                    
      DTF2= LF2/ VEL                                                    
      DTU = LU1/ VEL                                                    
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      S = 0.                                                            
      BR = BR1                                                          
      IF(IXS .GT. 0) IXS=0                               !JDL  6-MAR-84
      IF( NP  .GT. 100 ) GO TO 5                                        
      PRINT 100, ITITLE(NO)                                             
  100 FORMAT(  ' DIPOLE  ****  ', A4,'  ****************************'/) 
      PRINT 101                                                         
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HBX, 8X, 4HY CM , 7X, 2HBY,
     1   8X, 4HZ CM, 7X, 2HBZ, 8X, 6HVELZ/C , 6X, 8HTHETA MR , 5X,      
     2   6HPHI MR , 6X, 1HB             )                               
      IF (JRAYGAS.NE.0) PRINT 1010
 1010 FORMAT(14X,4H/Q  ,7X,2H/E,8X,4H/QB ,6X,3H/D2,8X,4H/MFP,6X,
     1       3H/SC,8X,6H / ST ,6X,8H/  ACAP ,5X,6H/ ALOS,7X,6H /DEDX)
      CALL PRNT2 ( T,S,XA   ,YA   ,ZA   ,BX,BY,BZ,BT,VXA  ,VYA  ,VZA   )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      PRINT 103                                                         
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO B AXIS SYSTEM '       )  
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.         
C****                                                                   
    5 COSA =DCOS( ALPHA/57.29578)                                       
      SINA =DSIN( ALPHA/57.29578)                                       
      TC(1) = ( A-ZA ) * SINA - ( XA + XCR1 ) * COSA                    
      TC(2) = YA                                                        
      TC(3) = ( A-ZA ) * COSA + ( XA + XCR1 ) * SINA                    
      TC(4) = -VZA * SINA - VXA * COSA                                  
      TC(5) = VYA                                                       
      TC(6) = -VZA * COSA + VXA * SINA                                  
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** TRANSLATE PARTICLE TO START OF FIRST FRINGE FIELD                 
C****                                                                   
C****                                                                   
      IF(  BR1  .EQ.  0. ) GO TO 20                                     
      IN = 4                                                            
      XDTF1 = DTF1                                                      
      IF(  Z11  .GT.  TC(3) )  XDTF1 = -DTF1                            
      IF( NP  .LE. 100) PRINT 108                                       
  108 FORMAT(/ ' CONSTANT FIELD CORRECTION IN FRINGE FIELD REGION    ' )
      NSTEP = 0                                                         
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  0    )            
   21 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 22  I=1,NP                                                     
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF(  XDTF1  .LT.  0. )  GO TO 23                                  
      IF(  Z11  .GE.  TC(3) )  GO TO 24                                 
      GO TO 22                                                          
   23 IF(  Z11  .LE.  TC(3) )  GO TO 24                                 
   22 CONTINUE                                                          
      GO TO 21                                                          
   24 DO 2 I=1,2                                                        
      XDTF1 = (TC(3) - Z11) / DABS(TC(6))                               
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  0    )            
    2 CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
C****                                                                   
C****                                                                   
C****                                                                   
   20 TDT = ( TC(3) - Z11 ) /DABS( TC(6) )                              
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      IF (Q00.LT.0.) TOLD=T
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** IN DESIGNATES MAGNET REGIONS FOR BFUN                             
C****                                                                   
      IN = 1                                                            
      XC= RB*DCOS( ALPHA/ 57.29578 )                                    
      ZC=-RB*DSIN( ALPHA/ 57.29578 )                                    
C****                                                                   
      C0   = DATA( 29,NO )                                              
      C1   = DATA( 30,NO )                                              
      C2   = DATA( 31,NO )                                              
      C3   = DATA( 32,NO )                                              
      C4   = DATA( 33,NO )                                              
      C5   = DATA( 34,NO )                                              
      DELS = DATA( 45,NO )                                              
      RCA  = DATA( 47,NO )                                              
      CSC = DCOS( ALPHA/57.29578 )                                      
      SCOR = DATA(49,NO)                                                
      S2   = DATA( 51,NO ) / RB    + RCA/2.D0                           
      S3   = DATA( 52,NO ) / RB**2                                      
      S4   = DATA( 53,NO ) / RB**3 + RCA**3/8.D0                        
      S5   = DATA( 54,NO ) / RB**4                                      
      S6   = DATA( 55,NO ) / RB**5 + RCA**5/16.D0                       
      S7   = DATA( 56,NO ) / RB**6                                      
      S8   = DATA( 57,NO ) / RB**7 + RCA**7/25.6D0                      
      IF( NP  .LE. 100) PRINT 104                                       
  104 FORMAT( 22H0FRINGING FIELD REGION    )                            
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BDIP,  0    )            
      NSTEP = 0                                                         
    6 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 7 I = 1, NP                                                    
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BDIP,  1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( Z12 .GE. TC(3) ) GO TO 8                                      
    7 CONTINUE                                                          
      GO TO 6                                                           
    8 CONTINUE                                                          
      XDTF1 =-( Z12 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  0    )            
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
  105 FORMAT( 10H   NSTEPS=  I5 )                                       
C***                                                                    
C***  UNIFORM FIELD REGION                                              
C**** TRANSFORM TO SECOND VFB COORD SYSTEM                              
C***                                                                    
      COPAB =DCOS( (PHI-ALPHA-BETA)/57.29578)                           
      SIPAB =DSIN( (PHI-ALPHA-BETA)/57.29578)                           
      COSPB =DCOS( (PHI/2.-BETA)/57.29578 )                             
      SINPB =DSIN( (PHI/2.-BETA)/57.29578 )                             
      SIP2 =DSIN( (PHI/2.)/57.29578 )                                   
      XT = TC(1)                                                        
      ZT = TC(3)                                                        
      VXT = TC(4)                                                       
      VZT = TC(6)                                                       
      TC(3) = - ZT  *COPAB +  XT  *SIPAB -2.*RB*SIP2*COSPB              
      TC(1) = - ZT  *SIPAB -  XT  *COPAB -2.*RB*SIP2*SINPB              
      TC(6) = - VZT *COPAB +  VXT *SIPAB                                
      TC(4) = - VZT *SIPAB -  VXT *COPAB                                
C****                                                                   
C****                                                                   
C**** UNIFORM FIELD INTEGRATION REGION                                  
C****                                                                   
C****                                                                   
      IN = 2                                                            
      XC=-RB*DCOS( BETA / 57.29578 )                                    
      ZC=-RB*DSIN( BETA / 57.29578 )                                    
      IF( NP  .LE. 100) PRINT 106                                       
  106 FORMAT(   '0UNIFORM FIELD REGION IN C AXIS SYSTEM '  )            
      IF( TC(3)  .LT.  Z21 ) GO TO 15                                   
C****                                                                   
C**** THIS SECTION CORRECTS FOR MAGNETS WHOSE FRINGING FIELDS INTERSECT 
C****                                                                   
      IF( NP  .LE. 100) PRINT 102                                       
  102 FORMAT( / '   INTEGRATE BACKWARDS    '  )                         
      CALL FNMIRK( 6, T,-DTU ,TC, DTC, DS, ES, BDIP,  0    )            
      NSTEP = 0                                                         
   16 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 17  I =1, NP                                                   
      CALL FNMIRK( 6, T,-DTU, TC, DTC, DS, ES, BDIP,  1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( TC(3)  .LE.  Z21 )  GO TO 18                                  
   17 CONTINUE                                                          
      GO TO 16                                                          
   18 CONTINUE                                                          
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, BDIP,  0    )            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, BDIP,  1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
      IF( NP  .LE. 100) PRINT 107                                       
  107 FORMAT( / )                                                       
      GO TO 19                                                          
C****                                                                   
C****                                                                   
   15 CONTINUE                                                          
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, BDIP,  0    )            
      NSTEP = 0                                                         
    9 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 10  I =1, NP                                                   
      CALL FNMIRK( 6, T, DTU, TC, DTC, DS, ES, BDIP,  1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( TC(3)  .GE.  Z21 )  GO TO 11                                  
C****                              !Changes from here... !JDL  6-MAR-84
C****
C**** FIND THE X-COORDINATE (XSCTR) AT PHI/2 (ZSCTR) AND INDICATE IF:
C****
C****   IXS =  0, SLITS NOT FOUND IN UNIFORM FIELD REGION.
C****         -1, RAY OUTSIDE  (MORE POSITIVE THAN XS1 AND XS2).
C****         +1, RAY PASSES   (BETWEEN SLITS AT   XS1 AND XS2).
C****         -2, RAY INSIDE   (MORE NEGATIVE THAN XS1 AND XS2).
C****
C**** DATA CARD 5 NOW CONTAINS:  NDX, BET1, GAMA, DELT, XS1, XS2.
C****
      IF(IXS .NE. 0) GO TO 10
      ZSCTR=TC(1)*SINPB+TC(3)*COSPB+RB*SIP2
      IF(ZSCTR .LT. 0.0) GO TO 10
      IXS=1
      IF((XS1 .EQ. 0.0) .AND. (XS2 .EQ. 0.0)) GO TO 10
      XSCTR=TC(1)*COSPB-TC(3)*SINPB-RB*(1.0-DCOS((PHI/2.0)/57.29578))
      IF((XSCTR .GT. XS1) .AND. (XSCTR .GT. XS2)) IXS=-1
      IF((XSCTR .LT. XS1) .AND. (XSCTR .LT. XS2)) IXS=-2
C****                                  !...down to here. !JDL  6-MAR-84
   10 CONTINUE                                                          
      GO TO 9                                                           
   11 CONTINUE                                                          
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, BDIP,  0    )            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, BDIP,  1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
   19 CONTINUE                                                          
C***                                                                    
C***                                                                    
C**** SETUP FOR SECOND FRINGE FIELD AND INTEGRATION                     
C****                                                                   
C****                                                                   
      BR   = BR2                                                        
      C0   = DATA( 35,NO )                                              
      C1   = DATA( 36,NO )                                              
      C2   = DATA( 37,NO )                                              
      C3   = DATA( 38,NO )                                              
      C4   = DATA( 39,NO )                                              
      C5   = DATA( 40,NO )                                              
      DELS = DATA( 46,NO )                                              
      RCA  = DATA( 48,NO )                                              
      SCOR = DATA(50,NO)                                                
      CSC = DCOS( BETA /57.29578 )                                      
      S2   = DATA( 58,NO ) / RB    + RCA/2.D0                           
      S3   = DATA( 59,NO ) / RB**2                                      
      S4   = DATA( 60,NO ) / RB**3 + RCA**3/8.D0                        
      S5   = DATA( 61,NO ) / RB**4                                      
      S6   = DATA( 62,NO ) / RB**5 + RCA**5/16.D0                       
      S7   = DATA( 63,NO ) / RB**6                                      
      S8   = DATA( 64,NO ) / RB**7 + RCA**7/25.6D0                      
      IN = 3                                                            
      IF( NP  .LE. 100) PRINT 104                                       
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, BDIP,  0    )            
      NSTEP = 0                                                         
   12 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 13  I =1, NP                                                   
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, BDIP,  1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( TC(3) .GE. Z22 )  GO TO 14                                    
   13 CONTINUE                                                          
      GO TO 12                                                          
   14 CONTINUE                                                          
      XDTF2 = ( Z22 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  0    )            
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
C****                                                                   
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 
C****                                                                   
      COSB =DCOS( BETA/57.29578 )                                       
      SINB =DSIN( BETA/57.29578 )                                       
      XT = TC(1)                                                        
      ZT = TC(3)                                                        
      VXT = TC(4)                                                       
      VZT = TC(6)                                                       
      TC(3) = ZT*COSB - XT*SINB - B                                     
      TC(1) = ZT*SINB + XT*COSB - XCR2                                  
      TC(6) = VZT*COSB - VXT*SINB                                       
      TC(4) = VZT*SINB + VXT*COSB                                       
      IF( NP  .LE. 100) PRINT 109                                       
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
C****                                                                   
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           
C****                                                                   
      IF(  BR2  .EQ.  0. ) GO TO 30                                     
      IN = 4                                                            
      XDTF2 = DTF2                                                      
      IF( TC(3)  .GT. 0. ) XDTF2 = -DTF2                                
      IF( NP  .LE. 100) PRINT 108                                       
      NSTEP = 0                                                         
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  0    )            
   31 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 32  I=1,NP                                                     
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( XDTF2  .LT. 0. ) GO TO 33                                     
      IF( TC(3)  .GE. 0. ) GO TO 34                                     
      GO TO 32                                                          
   33 IF( TC(3)  .LE. 0. ) GO TO 34                                     
   32 CONTINUE                                                          
      GO TO 31                                                          
   34 DO 3 I=1,2                                                        
      XDTF2 = -TC(3) / DABS(TC(6))                                      
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  0    )            
    3 CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
C****                                                                   
C****                                                                   
C****                                                                   
   30 TDT = -TC(3) /DABS( TC(6) )                                       
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      TP = T * VEL                                                      
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      S  = 0.                                                           
      VXF    = 1000. *DATAN2( TC(4), TC(6)  )                           
      VYF    = 1000. *DASIN ( TC(5)/ VEL    )                           
      VZF    = TC(6) / VEL                                              
      IF( NP  .LE. 100) PRINT 115,TP,TC(1),TC(2),TC(3),VZF,VXF,VYF      
  115 FORMAT( F10.4, 10X, F10.3, 11X, F10.3, 11X, F10.3, 11X,           
     1   F13.5, F13.2, F11.2                   )                        
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** CALCULATE INTERCEPTS IN SYSTEM D                                  
C****                                                                   
      Z0X = -TC(1)/ ( TC(4) / TC(6)    + 1.E-10 )                       
      Z0Y = -TC(2)/ ( TC(5) / TC(6)    + 1.E-10 )                       
      IF( NP  .LE. 100) PRINT 111, VXF, VYF, Z0X, Z0Y                   
  111 FORMAT( / ' INTERSECTIONS WITH VER. AND HOR. PLANES '          ,  
     X       /15X, 5H  XP=F10.4, 10H MR    YP= F10.4, 3H MR   /         
     1        15X, 5H Z0X=F10.2, 10H CM   Z0Y= F10.2, 3H CM   /        )
      RETURN                                                            
99      CALL PRNT4(NO, IN)                                              
        RETURN                                                          
      END                                                               
      SUBROUTINE DISCOL(DT,H,IQSW)
c
c     Entry points:
c     DISCOL	Determine distance to charge changing collision
c		If distance to go in this FNMIRK integration step is les
c		remaining distance to next collision, update integration
c		size, signal collision to FNMIRK and compute distance to
c		charge changing collision.
c
c     DISCOL0	Initialisation for new ray
c		(Re)sets "distance since last collision" counter and
c		determines distance to next collision
c
c     Parameters:
c     dt	default FNMIRK integration (time) step size
c     h		(time) step size FNMIRK going to use,
c		initially .eq.dt, updated if collision
c     iqsw	charge changing collision at end of this step
c
c     Called by
c     FNMIRK	Runge-Kutta integration routine
c     RAYTRACE  Main program before ray is launched 
c
c     Calls
c     RAN	FORTRAN RTL random number generator
c
c     Global Variables
c     vel	velocity of ion
c     gasmfp	mean free path length in gas
c     iseed	random generator seed
c
c     Comment
c     one possible optimisation is to have coldst, curdst in time domain
c     would save 1/ in collision, 1* when no collision, 
c     but velocity changes might give wrong results (though small:
c     velocity changes high: lots of gas --> lots of collisions
c     thin gas: velocity changes between charge changing is small)
c

c-ddc clearly repeatedly enters, and changes a values 'curdst', 'coldst'
c     which are neither in common blocks or 'save'd 

      IMPLICIT REAL*8(A-H,O-Z)                                          
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK63/  ISEED
c     COMMON  /BLCK65/QBAR,DELSQR,ACAPT,ALOS,NSK1,NSK2,SIGC,SIGT,ATBCC
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt

      DATA NPASG/0/
c
      real*8 curdst			! distance since previous collis
      real*8 curodst			! to remember where this step st
      real*8 coldst			! distance between prev and next
c					!   collision
      real*8 r				! temp for random generator
c
c*****
c     if we are here the first time, we have to setup first collision
c     (could GOTO r=ran(0), and save some instructions, but what if
c     first coldst .le. vel*dt ?  OK, than we have lots of collisions ..
c
      curodst=curdst			! remember where we are
      curdst=curdst+vel*dt		! where will this integration st
      if (curdst.lt.coldst) return	! short of next collision is ok.
      h=(coldst-curodst)/vel		! Don't go that far.
      iqsw=1				! tell it there'll be a collisio
      curdst=0.				! we just had one next time
      r = ran(0)			! where will be the following on
      coldst = -dlog(1.0D0-r)*gasmfp
      return
c
c     initialisation routine for next ray
c
      entry discol0
c
      curdst=0.				! we just are starting
      coldst=-dlog(1.0D0-ran(0))*gasmfp ! there will be the 1st coll
      return
c
c*****
c      previous algorithm: remembered very small step size when 
c      collision occured at approximation of field boundary,
c      so that integration didn't terminate within 10000 steps.
c
c      IF (NPASG.GT.0) GO TO 10
c      IQSW = 1
c      R = RAN(0)
c      DIST = -DLOG(1.-R)*GASMFP
c      H = DIST/VEL
c      IF (H.LE.DT) RETURN
c      NP = JIDINT(H/DT) +1
c      HP = H/NP
c10    IQSW = 0
c      H = HP
c      NPASG = NPASG + 1
c      IF (NPASG.LT.NP) RETURN
c      NPASG = 0
c      IQSW = 1
c      RETURN
      END
C********
C********
C********
C                   START CHARGE CHANGE ROUTINES      MP 1-JAN-85
C
C
C********
      SUBROUTINE  DRIFT( NO,  NP,T, TP ,NUM )                           
C****                                                                   
C****                                                                   
C**** Z-AXIS DRIFT ROUTINE                                              
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
c      REAL*4 DAET, TYME                                  !JDL 31-OCT-84
      include 'rtcomm0.f'
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       
C*JDL DIMENSION DATA(  75,30 ), ITITLE(30)               !JDL 17-NOV-83 
C**** DATA  C/ 3.D10/                                                   
  100 FORMAT( /  '   Z-AXIS DRIFT  ****  ', A4, '****************',//   
     1'      T CM', 18X, 'X CM', 7X, 'Y CM', 7X, 'Z CM' , '      VELZ/C'
     2   , '    THETA MR      PHI MR'  /  )                             
  103 FORMAT( F10.4, 11X, 3F11.3, F12.5, 2F12.3  )                      
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 1
      CALL PLT2 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 100, ITITLE(NO)                           
      VXP = 1000. *DATAN2( VXA,VZA )                                    
      VYP = 1000. *DASIN ( VYA/VEL )                                    
      VZP =  VZA  / VEL                                                 
      TP = T*VEL                                                        
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        
      TDT =(DATA(1,NO) - ZA) / DABS(VZA)                                
      T = T + TDT                                                       
      TP = T*VEL                                                        
      XA = XA + TDT*VXA                                                 
      YA = YA + TDT*VYA                                                 
      ZA = 0.                                                           
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT2 ( NUM, NO, NBR, TPAR )
      RETURN                                                            
      END                                                               
      SUBROUTINE EDIP
C****
C****   CALCULATES E-FIELD COMPONENTS FOR A CYLINDRICAL 
C****   ELECTROSTATIC DEFLECTOR
C****
        IMPLICIT REAL*8 (A-H, O-Z)
        REAL*8 K,bx,by,bz,tc,dtc
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC                           
      COMMON  /BLCK20/ EC2, EC4, WE, WC, dum
      COMMON  /BLCK22/  D, DG, S, EF, ET                                
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK24/  RB, XC, ZC                                      
      COMMON  /BLCK25/  IN, MTYP                                        
        DIMENSION TC(6), DTC(6)
C****
C****         
        X = TC(1)
        Y = TC(2)
        Z = TC(3)
        DX = X - XC
        DZ = Z
        RP2 = DX * DX + Z * Z
        RP = DSQRT(RP2)
        GO TO (1, 2, 3) , IN
100     FORMAT( ' ERROR -GO TO-  IN EDIP IN = ', I5)
        PRINT 100, IN
C****
C****   UNIFORM FIELD REGION
C****
2       EX = - EF * RB * DX / RP2
        EY = 0.
        EZ = - EF * RB * Z / RP2
        ET = DSQRT(EX * EX + EZ * EZ)
        RETURN
C****
C****   FRINGE FIELD REGION
C****
1       CONTINUE
3       CONTINUE
        ZP1 = DZ + DG
        ZP2 = DZ + 2. * DG
        ZM1 = DZ - DG
        ZM2 = DZ - 2. * DG
        DRP1 = DSQRT( DX * DX + ZP1 * ZP1 )
        DRP2 = DSQRT( DX * DX + ZP2 * ZP2 )
        DRM1 = DSQRT( DX * DX + ZM1 * ZM1 )
        DRM2 = DSQRT( DX * DX + ZM2 * ZM2 )
        CALL EDPP (F0,   Z  ,  X, Y      , RP   )
        S0 = S
        CALL EDPP (F1,  ZP1 ,  X, Y      , DRP1 )
        CALL EDPP (F2,  ZP2 ,  X, Y      , DRP2 )
        CALL EDPP (F3,  ZP1 ,  X, Y+DG   , DRP1 )
        CALL EDPP (F4,  ZP1 ,  X, Y-DG   , DRP1 )
        CALL EDPP (F5,   Z  ,  X, Y+DG   , RP   )
        CALL EDPP (F6,   Z  ,  X, Y+2.*DG, RP   )
        CALL EDPP (F7,   Z  ,  X, Y-DG   , RP   )
        CALL EDPP (F8,   Z  ,  X, Y-2.*DG, RP   )
        CALL EDPP (F9,  ZM1 ,  X, Y      , DRM1 )
        CALL EDPP (F10, ZM2 ,  X, Y      , DRM2 )
        CALL EDPP (F11, ZM1 ,  X, Y+DG   , DRM1 )
        CALL EDPP (F12, ZM1 ,  X, Y-DG   , DRM1 )
        S = S0
      XG1 = X/DG                                                        
      XG2 = XG1*XG1                                                     
      XG3 = XG2*XG1                                                     
      XG4 = XG3*XG1                                                     
C****                                                                   
      EY = XG1 * ( (F5-F7)*2./3. - (F6-F8)/12. ) +                      
     1         XG3 * ( (F5-F7)/6. - (F6-F8)/12. -                       
     2         ( F3 + F11 - F4 - F12 - 2.*F5 + 2.*F7 )/12. )            
      EX = F0 - XG2*( (F1 + F9 + F5 + F7 - 4.*F0) * 2./3. -             
     1         ( F2 + F10 + F6 + F8 - 4.*F0 )/24. ) +                   
     2         XG4 * (-( F1 + F9 + F5 + F7 - 4.*F0 )/6. +               
     3         ( F2 + F10 +      F6 + F8 - 4.*F0 )/24. +                
     4         ( F3 + F11 + F4 + F12 - 2.*F1 - 2.*F9 -                  
     5         2.*F5 - 2.*F7 + 4.*F0 )/12. )                            
      EZ = XG1 * ( (F1 - F9)*2./3. - (F2 - F10)/12. ) +                 
     1         XG3 * ( (F1 - F9)/6. - (F2 - F10)/12. -                  
     2         (F3 + F4 - F11 - F12 - 2.*F1 + 2.*F9)/12. )              
      ET = DSQRT( EX * EX + EY * EY + EZ * EZ)
       RETURN
       END
      SUBROUTINE EDIPL( NO, NP, T, TP ,NUM )                            
C****                                                                   
C****                                                                   
C**** SINGLE MAGNET RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIAL
C**** EQUATIONS OF MOTION.                                              
C     T = TIME                                                          
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
c      REAL*4 DAET, TYME                                  !JDL 31-OCT-84
      REAL*8  LF1, LF2, LU1, K,bx,by,bz,tc,dtc                      
      EXTERNAL EDIP                                                     
      include 'rtcomm0.f'
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC                           
      COMMON  /BLCK20/ EC2, EC4, WE, WC, dum
      COMMON  /BLCK22/  D, DG, S, EF, ET                                
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK24/  RB, XC, ZC                                      
      COMMON  /BLCK25/  IN, MTYP                                        
      COMMON  /BLCK26/  IXS, XSCTR, ZSCTR                !JDL 25-MAY-84
C*JDL DIMENSION DATA(  75,30 ), ITITLE(30)               !JDL 17-NOV-83 
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             
C**** DATA  C/ 3.D10/                                                   
C****                                                                   
      LF1  = DATA(  1,NO )                                              
      LU1  = DATA(  2,NO )                                              
      LF2  = DATA(  3,NO )                                              
      DG   = DATA(  4,NO )                                              
      A    = DATA( 11,NO )                                              
      B    = DATA( 12,NO )                                              
      D    = DATA( 13,NO )                                              
      RB   = DATA( 14,NO )                                              
      EF   = DATA( 15,NO )                                              
      PHI  = DATA( 16,NO )                                              
      EC2  = DATA( 17,NO )
      EC4  = DATA( 18,NO )
      WE   = DATA( 19,NO )
      WC   = DATA( 20,NO )
      Z11  = DATA( 25,NO )                                              
      Z12  = DATA( 26,NO )                                              
      Z21  = DATA( 27,NO )                                              
      Z22  = DATA( 28,NO )                                              
      DTF1= LF1/ VEL                                                    
      DTF2= LF2/ VEL                                                    
      DTU = LU1/ VEL                                                    
        IF (WE .EQ. 0.) WE = 1000. * RB
      BX = 0.
      BY = 0.
      BZ = 0.
      EX = 0.                                                           
      EY = 0.                                                           
      EZ = 0.                                                           
      ET = 0.                                                           
      S = 0.                                                            
      XS2 = +D/2.0                                        !JDL 25-MAY-84
      XS1 = -XS2                                          !JDL 25-MAY-84
      IF( IXS .GT. 0 ) IXS = 0                            !JDL 25-MAY-84
      IF( NP  .GT. 100 ) GO TO 5                                        
      PRINT 100, ITITLE(NO)                                             
  100 FORMAT(  ' E.S.-DIPOLE ****', A4,'  ***************************'/)
      PRINT 101                                                         
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HEX, 8X, 4HY CM , 7X, 2HEY,
     1   8X, 4HZ CM, 7X, 2HEZ, 8X, 6HVELZ/C , 6X, 8HTHETA MR , 5X,      
     2   6HPHI MR , 6X, 1HE             )                               
      CALL PRNT5 ( T,S,XA   ,YA   ,ZA   ,EX,EY,EZ,ET,VXA  ,VYA  ,VZA   )
      PRINT 103                                                         
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO B AXIS SYSTEM '       )  
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO EFB COORD.         
C****                                                                   
    5 CONTINUE                                                          
      TC(1) =  -  XA                                                    
      TC(2) = YA                                                        
      TC(3) = ( A-ZA )                                                  
      TC(4) = - VXA                                                     
      TC(5) = VYA                                                       
      TC(6) = -VZA                                                      
      CALL PRNT5 ( T,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C****                                                                   
C****                                                                   
   20 TDT = ( TC(3) - Z11 ) /DABS( TC(6) )                              
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** IN DESIGNATES MAGNET REGIONS FOR BFUN                             
C****                                                                   
      IN = 1                                                            
      XC = RB                                                           
      ZC = 0.0                                                          
C****                                                                   
      C0   = DATA( 29,NO )                                              
      C1   = DATA( 30,NO )                                              
      C2   = DATA( 31,NO )                                              
      C3   = DATA( 32,NO )                                              
      C4   = DATA( 33,NO )                                              
      C5   = DATA( 34,NO )                                              
      IF( NP  .LE. 100) PRINT 104                                       
  104 FORMAT( 22H0FRINGING FIELD REGION    )                            
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, EDIP,  0    )            
      NSTEP = 0                                                         
    6 CALL PRNT5 ( T,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )
      DO 7 I = 1, NP                                                    
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, EDIP,  1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( Z12 .GE. TC(3) ) GO TO 8                                      
    7 CONTINUE                                                          
      GO TO 6                                                           
    8 CONTINUE                                                          
      XDTF1 =-( Z12 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, EDIP,  0    )            
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, EDIP,  1    )            
      CALL PRNT5 ( T,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )
      NUM = NUM + 1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
  105 FORMAT( 10H   NSTEPS=  I5 )                                       
      XSTC1 = TC(1)                                       !JDL 25-MAY-84
C***                                                                    
C***  UNIFORM FIELD REGION                                              
C**** TRANSFORM TO SECOND EFB COORD SYSTEM                              
C***                                                                    
      COPAB =DCOS( (PHI)/57.29578)                                      
      SIPAB =DSIN( (PHI)/57.29578)                                      
      COSPB =DCOS( (PHI/2.)/57.29578 )                                  
      SINPB =DSIN( (PHI/2.)/57.29578 )                                  
      SIP2 =DSIN( (PHI/2.)/57.29578 )                                   
      XT = TC(1)                                                        
      ZT = TC(3)                                                        
      VXT = TC(4)                                                       
      VZT = TC(6)                                                       
      TC(3) = - ZT  *COPAB +  XT  *SIPAB -2.*RB*SIP2*COSPB              
      TC(1) = - ZT  *SIPAB -  XT  *COPAB -2.*RB*SIP2*SINPB              
      TC(6) = - VZT *COPAB +  VXT *SIPAB                                
      TC(4) = - VZT *SIPAB -  VXT *COPAB                                
C****                                                                   
C****                                                                   
C**** UNIFORM FIELD INTEGRATION REGION                                  
C****                                                                   
C****                                                                   
      IN = 2                                                            
      XC = -RB                                                          
      ZC = 0.0                                                          
      IF( NP  .LE. 100) PRINT 106                                       
  106 FORMAT(   '0UNIFORM FIELD REGION IN C AXIS SYSTEM '  )            
      IF( TC(3)  .LT.  Z21 ) GO TO 15                                   
C****                                                                   
C**** THIS SECTION CORRECTS FOR MAGNETS WHOSE FRINGING FIELDS INTERSECT 
C****                                                                   
      IF( NP  .LE. 100) PRINT 102                                       
  102 FORMAT( / '   INTEGRATE BACKWARDS    '  )                         
      CALL FNMIRK( 6, T,-DTU ,TC, DTC, DS, ES, EDIP,  0    )            
      NSTEP = 0                                                         
   16 CALL PRNT5 ( T,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )
      DO 17  I =1, NP                                                   
      CALL FNMIRK( 6, T,-DTU, TC, DTC, DS, ES, EDIP,  1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( TC(3)  .LE.  Z21 )  GO TO 18                                  
   17 CONTINUE                                                          
      GO TO 16                                                          
   18 CONTINUE                                                          
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, EDIP,  0    )            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, EDIP,  1    )            
      CALL PRNT5 ( T,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
      IF( NP  .LE. 100) PRINT 107                                       
  107 FORMAT( / )                                                       
      GO TO 19                                                          
C****                                                                   
C****                                                                   
   15 CONTINUE                                                          
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, EDIP,  0    )            
      NSTEP = 0                                                         
    9 CALL PRNT5 ( T,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )
      DO 10  I =1, NP                                                   
      CALL FNMIRK( 6, T, DTU, TC, DTC, DS, ES, EDIP,  1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( TC(3)  .GE.  Z21 )  GO TO 11                                  
   10 CONTINUE                                                          
      GO TO 9                                                           
   11 CONTINUE                                                          
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, EDIP,  0    )            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, EDIP,  1    )            
      CALL PRNT5 ( T,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
C****                              !Changes from here... !JDL 25-MAY-84
C****
C**** FIND X-COORDINATES AT ENDS OF UNIFORM REGION AND INDICATE IF:
C****
C****   IXS =  0, NO REGION OF UNIFORM FIELD.
C****         -1, RAY OUTSIDE  (MORE POSITIVE THAN +D/2).
C****         +1, RAY PASSES   (BETWEEN ELECTRODES).
C****         -2, RAY INSIDE   (MORE NEGATIVE THAN -D/2).
C****
C****
      IF(IXS .NE. 0) GO TO 19
      IXS=1
      IF(XSTC1 .GT. XS2) IXS=-1
      IF(XSTC1 .LT. XS1) IXS=-2
      IF(IXS .LT. 0) GO TO 19
      XSTC1=TC(1)
      IF(XSTC1 .GT. XS2) IXS=-1
      IF(XSTC1 .LT. XS1) IXS=-2
C****                                  !...down to here. !JDL 25-MAY-84
   19 CONTINUE                                                          
C***                                                                    
C***                                                                    
C**** SETUP FOR SECOND FRINGE FIELD AND INTEGRATION                     
C****                                                                   
C****                                                                   
      C0   = DATA( 35,NO )                                              
      C1   = DATA( 36,NO )                                              
      C2   = DATA( 37,NO )                                              
      C3   = DATA( 38,NO )                                              
      C4   = DATA( 39,NO )                                              
      C5   = DATA( 40,NO )                                              
      IN = 3                                                            
      IF( NP  .LE. 100) PRINT 104                                       
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, EDIP,  0    )            
      NSTEP = 0                                                         
   12 CALL PRNT5 ( T,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )
      DO 13  I =1, NP                                                   
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, EDIP,  1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( TC(3) .GE. Z22 )  GO TO 14                                    
   13 CONTINUE                                                          
      GO TO 12                                                          
   14 CONTINUE                                                          
      XDTF2 = ( Z22 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, EDIP,  0    )            
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, EDIP,  1    )            
      CALL PRNT5 ( T,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
C****                                                                   
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 
C****                                                                   
      XT = TC(1)                                                        
      ZT = TC(3)                                                        
      VXT = TC(4)                                                       
      VZT = TC(6)                                                       
      TC(3) = ZT - B                                                    
      TC(1) = XT                                                        
      TC(6) = VZT                                                       
      TC(4) = VXT                                                       
      IF( NP  .LE. 100) PRINT 109                                       
      CALL PRNT5 ( T,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )
C****                                                                   
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           
C****                                                                   
C****                                                                   
C****                                                                   
C****                                                                   
   30 TDT = -TC(3) /DABS( TC(6) )                                       
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      TP = T * VEL                                                      
      EX = 0.                                                           
      EY = 0.                                                           
      EZ = 0.                                                           
      ET = 0.                                                           
      S  = 0.                                                           
      VXF    = 1000. *DATAN2( TC(4), TC(6)  )                           
      VYF    = 1000. *DASIN ( TC(5)/ VEL    )                           
      VZF    = TC(6) / VEL                                              
      IF( NP  .LE. 100) PRINT 115,TP,TC(1),TC(2),TC(3),VZF,VXF,VYF      
  115 FORMAT( F10.4, 10X, F10.3, 11X, F10.3, 11X, F10.3, 11X,           
     1   F13.5, F13.2, F11.2                   )                        
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** CALCULATE INTERCEPTS IN SYSTEM D                                  
C****                                                                   
      Z0X = -TC(1)/ ( TC(4) / TC(6)    + 1.E-10 )                       
      Z0Y = -TC(2)/ ( TC(5) / TC(6)    + 1.E-10 )                       
      IF( NP  .LE. 100) PRINT 111, VXF, VYF, Z0X, Z0Y                   
  111 FORMAT( / ' INTERSECTIONS WITH VER. AND HOR. PLANES '          ,  
     X       /15X, 5H  XP=F10.4, 10H MR    YP= F10.4, 3H MR   /         
     1        15X, 5H Z0X=F10.2, 10H CM   Z0Y= F10.2, 3H CM   /        )
      RETURN                                                            
99      CALL PRNT4(NO, IN)                                              
        RETURN                                                          
      END                                                               
      SUBROUTINE EDPP( EFLD, Z, X, Y, DRP )
C****
C**** CALCULATE S; DETERMINE E-FIELD IN FRINGE REGIONS
C****
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 K
      COMMON  /BLCK20/ EC2, EC4, WE, WC, dum
      COMMON  /BLCK22/  D, DG, S, EF, ET                                
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK24/  RB, XC, ZC                                      
      COMMON  /BLCK25/  IN, MTYP                                        
        FEF = -EF
        IF ( IN .EQ. 1 ) FEF = +EF
        W2 = WE * WE
        ZD1 = Z / D
        ZD2 = EC2 * ZD1 * Y * Y / W2
        W4 = W2 * W2
        ZD3 = EC4 * (Y**4) / W4
        S = ZD1 + ZD2 + ZD3
        CS = C0 + S * (C1 + S * (C2 + (S * (C3 + S * (C4 +S * C5)))))
        IF (DABS(CS) .GT. 70.) CS = DSIGN(70.D0, CS)
        E = DEXP(CS)
        P0 = 1.0 + E
        EFLD = (FEF / P0) * (RB / DRP)
      RETURN
      END
      SUBROUTINE FB01AD(C,  VK,VE)                                      
      IMPLICIT REAL*8(A-H,O-Z)                                          
C*IBM REAL*8 XLG/  Z7FFFFFFFFFFFFFFF /                                  
      REAL * 8 XLG/1E20/                                 
      D=1D0-C                                                           
      IF(D .GT. 0D0)E=-DLOG(D)                                          
C**** HARWELL VERSION OF FB01AD                                         
      IF(C .GE. 1D0)GO TO 2                                             
           VE=E*((((((((((                                              
     A     3.18591956555015718D-5*D  +.989833284622538479D-3)*D         
     B    +.643214658643830177D-2)*D +.16804023346363385D-1)*D          
     C    +.261450147003138789D-1)*D +.334789436657616262D-1)*D         
     D    +.427178905473830956D-1)*D +.585936612555314917D-1)*D         
     E    +.937499997212031407D-1)*D +.249999999999901772D0)*D)         
     F    +(((((((((                                                    
     G     .149466217571813268D-3*D  +.246850333046072273D-2)*D         
     H    +.863844217360407443D-2)*D+.107706350398664555D-1)*D          
     I    +.782040406095955417D-2)*D +.759509342255943228D-2)*D         
     J    +.115695957452954022D-1)*D +.218318116761304816D-1)*D         
     K    +.568051945675591566D-1)*D +.443147180560889526D0)*D          
     L    +1D0                                                          
C****                                                                   
C**** ROUTINE MODIFIED TO CALCULATE VK AND VE ALWAYS                    
C****                                                                   
C****                                                                   
           VK=E*((((((((((                                              
     A     .297002809665556121D-4*D   +.921554634963249846D-3)*D        
     B    +.597390429915542916D-2)*D  +.155309416319772039D-1)*D        
     C    +.239319133231107901D-1)*D  +.301248490128989303D-1)*D        
     D    +.373777397586236041D-1)*D  +.48828041906862398D-1)*D         
     E    +.703124997390383521D-1)*D  +.124999999999908081D0)*D         
     F    +.5D0)+(((((((((                                              
     G     .139308785700664673D-3*D   +.229663489839695869D-2)*D        
     H    +.800300398064998537D-2)*D  +.984892932217689377D-2)*D        
     I    +.684790928262450512D-2)*D  +.617962744605331761D-2)*D        
     J    +.878980187455506468D-2)*D  +.149380135326871652D-1)*D        
     K    +.308851462713051899D-1)*D  +.965735902808562554D-1)*D        
     L    +1.38629436111989062D0                                        
      RETURN                                                            
    2 VE=1D0                                                            
      VK=XLG                                                            
      RETURN                                                            
      END                                                               
      SUBROUTINE FB02AD(CAYSQ,SINP,COSP,E,F)                            
C                                                                       
      IMPLICIT REAL*8(A-H,O-Z)                                           
      PHI=DATAN(SINP/COSP)                                              
      IF(CAYSQ*SINP*SINP-0.5D0)1,1,5                                    
    1 H=1.0D0                                                           
      A=PHI                                                             
      N=0                                                               
      SIG1=0.D0                                                         
      SIG2=0.D0                                                         
      SIN2=SINP*SINP                                                    
      TERM=SINP*COSP*0.5D0                                              
      CRIT=PHI                                                          
    2 N=N+1                                                             
      RECIP=1.0D0/N                                                     
      FACT=(N-.5D0)*RECIP                                               
      H1=H                                                              
      H=FACT*CAYSQ*H                                                    
      A=FACT*A-TERM*RECIP                                               
      TERM=TERM*SIN2                                                    
      CRIT=CRIT*SIN2                                                    
      DEL1=H*A                                                          
      DEL2=-.5D0*RECIP*CAYSQ*H1*A                                       
      SIG1=SIG1+DEL1                                                    
      SIG2=SIG2+DEL2                                                    
      IF(DABS(DEL1)-4.0D-16)4,3,3                                       
   3  IF(DABS(CRIT)-DABS(A))4,2,2                                       
    4 F=PHI+SIG1                                                        
      E=PHI+SIG2                                                        
      GO TO 8                                                           
    5 CFI=1.D0                                                          
      CFJ=1.D0                                                          
      CFL=0.D0                                                          
      CFM=0.D0                                                          
      CFN=0.D0                                                          
      SIG1=0.D0                                                         
      SIG2=0.D0                                                         
      SIG3=0.D0                                                         
      SIG4=0.D0                                                         
      N=0                                                               
      FACT1=1.0D0-CAYSQ*SINP*SINP                                       
      FACTOR=.5D0*COSP*DSQRT(CAYSQ/FACT1)                               
      FACTRO=FACTOR+FACTOR                                              
      CAYDSQ=1.0D0-CAYSQ                                                
    6 N=N+1                                                             
      RECIP=1.0D0/N                                                     
      FACTN=RECIP*(N-.5D0)                                              
      FACTM=(N+.5D0)/(N+1.0D0)                                          
      FACTOR=FACTOR*FACT1                                               
      CFI1=CFI                                                          
      CFJ1=CFJ                                                          
      CFI=CFI*FACTN                                                     
      CFJ=CFJ*FACTN*FACTN*CAYDSQ                                        
      CFL=CFL+.5D0/(N*(N-.5D0))                                         
      CFM=(CFM-FACTOR*RECIP*CFI)*FACTM*FACTM*CAYDSQ                     
      CFN=(CFN-FACTOR*RECIP*CFI1)*FACTN*FACTM*CAYDSQ                    
      DEL1=CFM-CFJ*CFL                                                  
      DEL2=CFN-(FACTN*CFL-.25D0*RECIP*RECIP)*CAYDSQ     *CFJ1           
      DEL3=CFJ                                                          
      DEL4=FACTM*CFJ                                                    
      SIG1=SIG1+DEL1                                                    
      SIG2=SIG2+DEL2                                                    
      SIG3=SIG3+DEL3                                                    
      SIG4=SIG4+DEL4                                                    
      IF(DABS (DEL1)-4.0D-16)7,6,6                                      
    7 CAYMOD=DSQRT(CAYSQ)                                               
      FLOG1=DLOG(4.0D0/(DSQRT(FACT1)+CAYMOD*COSP))                      
      T1=(1.0D0+SIG3)*FLOG1+FACTRO*DLOG(.5D0+.5D0*CAYMOD*DABS (SINP))   
      T2=(.5D0+SIG4)*CAYDSQ*FLOG1+1.0D0-FACTRO*(1.0D0-CAYMOD*DABS(SINP))
      F=T1+SIG1                                                         
      E=T2+SIG2                                                         
    8 RETURN                                                            
      END                                                               
      SUBROUTINE FB03AD( GN,CACA,P )                                    
C====== 23/03/72 LAST LIBRARY UPDATE                                    
      IMPLICIT REAL*8(A-H,O-Z)                                           
      IF(GN)1,2,2                                                       
    1 IF(CACA)3,3,4                                                     
    3 P=1.5707963268/DSQRT(1.D0-GN)                                     
      RETURN                                                            
    4 STH=DSQRT(-GN/(CACA-GN))                                          
      CTH=DSQRT(1.D0-STH*STH)                                           
      CADA=1.D0-CACA                                                    
      CALLFB01AD(CACA,     CAPK,CAPE)                                   
      CALLFB02AD(CADA,STH,CTH,E,F)                                      
      BR=CAPE*F-CAPK*(F-E)                                              
      P=CAPK*CTH*CTH+STH*BR/DSQRT(1.D0-GN)                              
      RETURN                                                            
    2 IF(GN-CACA)10,30,20                                               
   10 STH=DSQRT(GN/CACA)                                                
      CTH=DSQRT(1.D0-STH*STH)                                           
      CALLFB01AD(CACA,     CAPK,CAPE)                                   
      CALLFB02AD(CACA,STH,CTH,E,F)                                      
      BR=CAPK*E-CAPE*F                                                  
      P=CAPK+BR*STH/(CTH*DSQRT(1.D0-GN))                                
      RETURN                                                            
   30 CALLFB01AD(CACA,     CAPK,CAPE)                                   
      P=CAPE/(1.D0-CACA)                                                
      RETURN                                                            
   20 CADA=1.D0-CACA                                                    
      PI=3.1415926536                                                   
      STH=DSQRT((1.D0-GN)/CADA)                                         
      CTH=DSQRT(1.D0-STH*STH)                                           
      CALLFB01AD(CACA,     CAPK,CAPE)                                   
      CALLFB02AD(CADA,STH,CTH,E,F)                                      
      BR=PI/2.+CAPK*(F-E)-CAPE*F                                        
      P=CAPK+BR*DSQRT(GN)/(CADA*STH*CTH)                                
      RETURN                                                            
      END                                                               
      FUNCTION FDEDX( QION )
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 stop,IZ,IM,IEIN,IZTGT                                      
      DIMENSION  SC(92)                                                 
      COMMON  /SHELLC/SC                                                
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK61/  DEDX,ALPHAK,TOLD2,DEDXQ
      COMMON  /BLCK62/  GASENE,GASVEL,TOLD,GASL
      COMMON  /BLCK63/  ISEED
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt
c      include 'rtcomm65.f'
      DATA VEL1/0./,DEDX2/1./,C/3.D10/,PROTM/938.211/
C       
C**** 
C**** 
C
      CALL SHLLCO                                                       
C
C          FDEDX IN MEV/(MG/CM2)
C
C      
      IZ = ZION
      IM = PMASS
      IEIN = GASENE
      IZTGT = ZGAS
      FDEDX = STOP(IZ,IM,IZTGT,IEIN)                                    
      RETURN
      END

C****************************************************
C      TO ADD ENELOSS.FOR;3 BY WFH JULY 26/1985	    *                   
C****************************************************
C 
      SUBROUTINE FNMIRK(N,X,DT,Y,DY,D,E,BFUN,  NDEX)                    
      IMPLICIT REAL*8(A-H,O-Z)                                          
      EXTERNAL BFUN                                                     
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK63/  ISEED
      COMMON  /BLCK66/  tfwopt,taufct,alffct,TBAR,THWHM,tau0
      DIMENSION Y(6),DY(6),D(6),E(6)                                    
      DIMENSION VTMP(3)
      real tprevb, tnextb, tfactor	! BSK 11/24/91
      parameter (tfactor=0.33)
      DATA PI2/6.28318/
      IQSW = 0
  5   H = DT
      IQSW = 0
      IF( NDEX.NE.0) GO TO 20                                           
      DO 10 I=1,N                                                       
      D(I)=Y(I)                                                         
 10   CONTINUE                                                          
      CALL DERIV ( BFUN,X,1 )                                           
      HALFH=0.5*H                                                       
      tnextb = x+tfactor*dt
      tprevb = x-tfactor*dt
      RETURN                                                            
 20   IF (JRAY*GAS.EQ.0.) GO TO 25
      IF (DT.LE.0.) GO TO 25 ! negative is ok for dis
c     IF (DT.LE.0.) GO TO 25 ! IW not for me....
      CALL DISCOL(DT,H,IQSW)
      HALFH = 0.5*H
 25   CONTINUE
      DO 30 I=1,N                                                       
      T=HALFH*DY(I)                                                     
      Y(I)=D(I)+T                                                       
      E(I)=T                                                            
 30   CONTINUE                                                          
      XZERO=X                                                           
      X=X+HALFH                                                         
      if (x.le.tprevb .or. x.ge.tnextb) then
         tprevb=x-dt*tfactor
         tnextb=x+dt*tfactor
         call deriv( bfun,x,1 )
      else
      CALL DERIV ( BFUN,X,0 )                                           
      endif
      DO 40 I=1,N                                                       
      T=HALFH*DY(I)                                                     
      Y(I)=D(I)+T                                                       
      E(I)=E(I)+2.0*T                                                   
 40   CONTINUE                                                          
      if (x.le.tprevb .or. x.ge.tnextb) then
         tprevb=x-dt*tfactor
         tnextb=x+dt*tfactor
         call deriv( bfun,x,1 )
      else
      CALL DERIV ( BFUN,X,0 )                                           
      endif
      DO 50 I=1,N                                                       
      T=H*DY(I)                                                         
      Y(I)=D(I)+T                                                       
      E(I)=E(I)+T                                                       
 50   CONTINUE                                                          
      X=XZERO+H                                                         
      if (x.le.tprevb .or. x.ge.tnextb) then
         tprevb=x-dt*tfactor
         tnextb=x+dt*tfactor
         call deriv( bfun,x,1 )
      else
      CALL DERIV ( BFUN,X,0 )                                           
      endif
      DO 60 I=1,N                                                       
      Y(I)=D(I)+(E(I)+HALFH*DY(I))*.333333333                           
      D(I)=Y(I)                                                         
 60   CONTINUE                                                          
      if (x.le.tprevb .or. x.ge.tnextb) then
         tprevb=x-dt*tfactor
         tnextb=x+dt*tfactor
         call deriv( bfun,x,1 )
      else
      CALL DERIV ( BFUN,X,0 )
      endif
      IF ( JRAY*GAS*IQSW*DT.LE.0. ) RETURN
      CALL GASINT
      DTETA =     (TBAR + THWHM * RANDOM(1,ISEED))
      DPSI =      (TBAR + THWHM * RANDOM(1,ISEED))
      DPHI =      (TBAR + THWHM * RANDOM(1,ISEED))
      VTMP(1) = Y(4) + (DPHI-DPSI)*Y(5) + DTETA*Y(6)
      VTMP(2) = Y(5) + (DPSI-DPHI)*Y(4) 
      VTMP(3) = Y(6) - DTETA*Y(4)
      DO 100 J=1,3
      Y(3+J) = VTMP(J)
      D(3+J) = VTMP(J)
      DY(J) = VTMP(J)
 100  CONTINUE
      RETURN                                                            
      END                                                               
      FUNCTION FSDEDX( ztgt,atgt )
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 stops,IZ,IM,IEIN,IZTGT,iatgt                       !MP 29-j
      DIMENSION  SC(92)                                                 
      COMMON  /SHELLC/SC                                                
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK61/  DEDX,ALPHAK,TOLD2,DEDXQ
      COMMON  /BLCK62/  GASENE,GASVEL,TOLD,GASL
      COMMON  /BLCK63/  ISEED
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt
c      include 'rtcomm65.f'
      DATA VEL1/0./,DEDX2/1./,C/3.D10/,PROTM/938.211/
C       
C**** 
C**** 
C
      CALL SHLLCO                                                       
C
C          FSDEDX IN MEV/(MG/CM2)
C
C      
      IZ = ZION
      IM = PMASS
      IEIN = energy
      IZTGT = ztgt
      iatgt = atgt                                              !MP 29-j
      FSDEDX = STOPS(IZ,IM,IZTGT,iatgt,IEIN)                    !MP 29-j
      RETURN
      END

C****************************************************
C      TO ADD ENELOSS.FOR;3 BY WFH JULY 26/1985	    *                   
C****************************************************
C 
      FUNCTION fwhmstg( ztgt,atgt,ttgt,zion )
      IMPLICIT REAL*8(A-H,O-Z)
      fw = dsqrt((ztgt/atgt)*ttgt)*zion*0.924d-3
      fwhmstg = fw
      RETURN
      END

      SUBROUTINE GASINT 
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 K,bx,by,bz,tc,dtc
      DIMENSION TC(6), DTC(6)
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC                           
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK61/  DEDX,ALPHAK,TOLD2,DEDXQ
      COMMON  /BLCK62/  GASENE,GASVEL,TOLD,GASL
      COMMON  /BLCK63/  ISEED
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt
c      include 'rtcomm65.f'
      COMMON  /BLCK66/  tfwopt,taufct,alffct,TBAR,THWHM,tau0
      common  /blck71/  gasopt,zgas125,zgas18,dreldee,enold,icalc
      data drel0,dee0/0.05,0.005/
C*** THERE WAS A CHARGE CHANGE DECISION
      CALL QCG (Q0,Q1)
      Q0 = Q1
      QMC = EMASS / (9.D10*Q0 )
      ETOT = EMASS+GASENE
      K = (Q0/ETOT)*9.D10
C*****
C*****
C*****
c
c     Capture cross sections and charge state distribution are updated
c     whenever the relative change in cross section is larger than drel0
c     Relative change in cross section is calculated from Schlachter et 
c     scaling law. See subroutine sigcap for Ref.    
c
c     Calculate now relative change in cross sections.
      dee = (enold - gasene)/enold
      drel = dabs(dreldee*dee)
c
c     29 october 1991 :
c     Modify criterion for updating de/dx. qdist, sigcap
c     Following statement was corrected on 8-jul-82 according to
c     Bernhard Schneck's comment
c 
      if (dee.lt.dee0.and.drel.le.drel0) go to 100
c
c     Update gassig,acapt,qbar,delsqr,small ang. scat.,dedx
      CALL QDIST
      if ( gasopt.le.0.and.drel.ge.drel0) call sigcap
      DQ = Q0 - QBAR
      CALL QSIG(DQ)
      if ( tfwopt.lt.0.) call smangsc
      dedx = fdedx(q0)
      dedxq = dedx*(q0/qbar)**2
      enold = gasene
c     NSK1 = 0
      RETURN
c
C **CALCULATE NEW SIGC,SIGL,SIGT for charge state Q0=Q1
c
  100 DQ = Q0 - QBAR
      CALL QSIG(DQ)
      dedxq = dedx*(q0/qbar)**2
      RETURN
      END
       FUNCTION IRND(X)
c-ddc 
      IMPLICIT REAL*8(A-H,O-Z)
C 
C***********************************      
C 

       IS=-1
       IF(X.GE.0.) IS=1         
       H=ABS(X)       
c-ddc       I=IFIX(H)      
       I=IDINT(H)      
       IF((H-FLOAT(I)).GE.0.5) I=I+1      
       IRND=IS*I      
       RETURN         
       END  
C 
C******************************************************       
C 
      SUBROUTINE  LENS ( NO,  NP,T, TP ,NUM )                           
C****                                                                   
C****                                                                   
C**** THIN LENS ROUTINE                                                 
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
c      REAL*4 DAET, TYME                                  !JDL 31-OCT-84
      include 'rtcomm0.f'
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       
C*JDL DIMENSION DATA(  75,30 ), ITITLE(30)               !JDL 17-NOV-83 
C**** DATA  C/ 3.D10/                                                   
C****                                                                   
  100 FORMAT( /  '   THIN LENS     ****  ', A4, '****************',//   
     1'      T CM', 18X, 'X CM', 7X, 'Y CM', 7X, 'Z CM' , '      VELZ/C'
     2   , '    THETA MR      PHI MR'  /  )                             
  103 FORMAT( F10.4, 11X, 3F11.3, F12.5, 2F12.3  )                      
C****                                                                   
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 1
      CALL PLT2 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 100, ITITLE(NO)                           
      VXP = 1000. *DATAN2( VXA,VZA )                                    
      VYP = 1000. *DASIN ( VYA/VEL )                                    
      VZP =  VZA  / VEL                                                 
      TP = T*VEL                                                        
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        
      XXA = XA                                                          
      YYA = YA                                                          
       XA =XXA*DATA(1,NO) + VXP*DATA(2,NO)                              
      VXP =XXA*DATA(3,NO) + VXP*DATA(4,NO)                              
       YA =YYA*DATA(5,NO) + VYP*DATA(6,NO)                              
      VYP =YYA*DATA(7,NO) + VYP*DATA(8,NO)                              
      VXA = VEL*DSIN( VXP/1000.D0 )                                     
      VYA = VEL*DSIN( VYP/1000.D0 )                                     
      VZA = DSQRT(VEL*VEL -VXA*VXA-VYA*VYA)                             
      VZP = VZA/VEL                                                     
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT2 ( NUM, NO, NBR, TPAR )
      RETURN                                                            
      END                                                               
      SUBROUTINE MATRIX( R, T2  )                                       
C****                                                                   
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      COMMON  /BLCK 1/  XI, YI, ZI, VXI, VYI, VZI, DELP, DELM      !JDL 
      COMMON  /BLCK 2/  XO, YO, ZO, VXO, VYO, VZO, RTL(100), RLL(100)   
      DIMENSION XI(100), YI(100), ZI(100), VXI(100), VYI(100), VZI(100),
     1        DELP(100)                                                 
      DIMENSION XO(100), YO(100), ZO(100), VXO(100), VYO(100), VZO(100) 
      DIMENSION  R(6,6) , T2(5,6,6), TT(5,6,6)                          
      DO 21  I1= 1,6                                                    
      DO 21  I2= 1,6                                                    
      R(I1,I2) = 0.                                                     
      DO 21 I3= 1,5                                                     
   21 T2(I3,I1,I2) = 0.                                                 
C****                                                                   
C****                                                                   
C**** CALCULATE COEFFICIENTS                                            
C****                                                                   
      R(1,1) =  ( XO(3) -  XO(4) ) / ( XI(3) -  XI(4) )                 
      R(1,2) =  ( XO(5) -  XO(6) ) / (VXI(5) - VXI(6) )                 
      R(1,3) =  ( XO(7) -  XO(8) ) / ( YI(7) -  YI(8) )                 
      R(1,4) =  ( XO(9) -  XO(10)) / (VYI(9) - VYI(10))                 
      R(1,6) =  ( XO(11)-  XO(12) )/ (DELP(11) - DELP(12) )             
      R(2,1) =  (VXO(3) - VXO(4) ) / ( XI(3) -  XI(4) )                 
      R(2,2) =  (VXO(5) - VXO(6) ) / (VXI(5) - VXI(6) )                 
      R(2,3) =  (VXO(7) - VXO(8) ) / ( YI(7) -  YI(8) )                 
      R(2,4) =  (VXO(9) - VXO(10)) / (VYI(9) - VYI(10))                 
      R(2,6) =  (VXO(11)- VXO(12) )/ (DELP(11) - DELP(12) )             
      R(3,1) =  ( YO(3) -  YO(4) ) / ( XI(3) -  XI(4) )                 
      R(3,2) =  ( YO(5) -  YO(6) ) / (VXI(5) - VXI(6) )                 
      R(3,3) =  ( YO(7) -  YO(8) ) / ( YI(7) -  YI(8) )                 
      R(3,4) =  ( YO(9) -  YO(10)) / (VYI(9) - VYI(10))                 
      R(3,6) =  ( YO(11)-  YO(12) )/ (DELP(11) - DELP(12) )             
      R(4,1) =  (VYO(3) - VYO(4) ) / ( XI(3) -  XI(4) )                 
      R(4,2) =  (VYO(5) - VYO(6) ) / (VXI(5) - VXI(6) )                 
      R(4,3) =  (VYO(7) - VYO(8) ) / ( YI(7) -  YI(8) )                 
      R(4,4) =  (VYO(9) - VYO(10)) / (VYI(9) - VYI(10))                 
      R(4,6) =  (VYO(11)- VYO(12) )/ (DELP(11) - DELP(12) )             
      R( 5,5 )  =  1.                                                   
      R( 6,6 )  =  1.                                                   
      R(5,1) =  (RTL(3) - RTL(4) ) / ( XI(3) -  XI(4) )                 
      R(5,2) =  (RTL(5) - RTL(6) ) / (VXI(5) - VXI(6) )                 
      R(5,6) =  (RTL(11)- RTL(12) )/ (DELP(11) - DELP(12) )             
C****                                                                   
C****                                                                   
      T2(1,1,1)= ( XO(3) + XO(4) ) /(2.*XI(3)**2 )                      
      T2(1,2,2)= ( XO(5) + XO(6) ) /(2.*VXI(5)**2)                      
      T2(1,3,3)= ( XO(7) + XO(8) ) /(2.*YI(7)**2 )                      
      T2(1,4,4)= ( XO(9) + XO(10) ) /(2.*VYI(9)**2 )                    
      T2(1,6,6)= ( XO(11) + XO(12) ) /(2.*DELP(11)**2 )                 
      T2(1,1,2)= ( XO(13)+XO(14)-2.*T2(1,1,1)*XI(13)**2-2.*T2(1,2,2)*   
     1   VXI(13)**2 ) /(2.*XI(13)*VXI(13) )                             
      T2(1,1,6)= ( XO(15) + XO(16) -2.*T2(1,1,1)*XI(15)**2 -            
     1  2.*T2(1,6,6)*DELP(15)**2 ) /(2.*XI(15)*DELP(15) )               
      T2(1,2,6)= ( XO(17) + XO(18) -2.*T2(1,2,2)*VXI(17)**2 -           
     1  2.*T2(1,6,6)*DELP(17)**2 ) /(2.*VXI(17)*DELP(17) )              
      T2(1,3,4)= ( XO(19)- XO(20) ) /(2.*YI(19)*VYI(19) )               
      T2(2,1,1)= (VXO(3) +VXO(4) ) /(2.*XI(3)**2 )                      
      T2(2,2,2)= (VXO(5) +VXO(6) ) /(2.*VXI(5)**2)                      
      T2(2,3,3)= (VXO(7) +VXO(8) ) /(2.*YI(7)**2 )                      
      T2(2,4,4)= (VXO(9) +VXO(10) ) /(2.*VYI(9)**2 )                    
      T2(2,6,6)= (VXO(11) +VXO(12) ) /(2.*DELP(11)**2 )                 
      T2(2,1,2)=(VXO(13)+VXO(14)-2.*T2(2,1,1)*XI(13)**2-2.*T2(2,2,2)*   
     1   VXI(13)**2 ) /(2.*XI(13)*VXI(13) )                             
      T2(2,1,6)= (VXO(15) +VXO(16) -2.*T2(2,1,1)*XI(15)**2 -            
     1  2.*T2(2,6,6)*DELP(15)**2 ) /(2.*XI(15)*DELP(15) )               
      T2(2,2,6)= (VXO(17) +VXO(18) -2.*T2(2,2,2)*VXI(17)**2 -           
     1  2.*T2(2,6,6)*DELP(17)**2 ) /(2.*VXI(17)*DELP(17) )              
      T2(2,3,4)= (VXO(19)-VXO(20) ) /(2.*YI(19)*VYI(19) )               
      T2(3,1,3)= ( YO(21) - YO(22) ) /(2.*XI(21)*YI(21) )               
      T2(3,1,4)= ( YO(23) - YO(24) ) /(2.*XI(23)*VYI(23) )              
      T2(3,2,3)= ( YO(25) - YO(26) ) /(2. *VXI(25)*YI(25) )             
      T2(3,2,4)= ( YO(27) - YO(28) ) /(2.*VXI(27)*VYI(27) )             
      T2(3,3,6)= ( YO(29) - YO(30) ) /(2.*YI(29)*DELP(29) )             
      T2(3,4,6)= ( YO(31) - YO(32) ) /(2.*VYI(31)*DELP(31)  )           
      T2(4,1,3)= (VYO(21) -VYO(22) ) /(2.*XI(21)*YI(21) )               
      T2(4,1,4)= (VYO(23) -VYO(24) ) /(2.*XI(23)*VYI(23) )              
      T2(4,2,3)= (VYO(25) -VYO(26) ) /(2. *VXI(25)*YI(25) )             
      T2(4,2,4)= (VYO(27) -VYO(28) ) /(2.*VXI(27)*VYI(27) )             
      T2(4,3,6)= (VYO(29) -VYO(30) ) /(2.*YI(29)*DELP(29) )             
      T2(4,4,6)= (VYO(31) -VYO(32) ) /(2.*VYI(31)*DELP(31)  )           
C****                                                                   
C**** PATH LENGTH TERMS                                                 
C****                                                                   
      T2(5,1,1) = ( RTL(3) + RTL(4) - 2*RTL(1) ) /( 2* XI(3)**2 )       
      T2(5,2,2) = ( RTL(5) + RTL(6) - 2*RTL(1) ) /( 2*VXI(5)**2 )       
      T2(5,3,3) = ( RTL(7) + RTL(8) - 2*RTL(1) ) /( 2* YI(7)**2 )       
      T2(5,4,4) = ( RTL(9) + RTL(10)- 2*RTL(1) ) /( 2*VYI(9)**2 )       
      T2(5,6,6) = ( RTL(11)+ RTL(12)- 2*RTL(1) ) /( 2*DELP(11)**2 )     
      T2(5,1,2) = ( RTL(13)+ RTL(14)- 2*RTL(1) - 2*T2(5,1,1)* XI(13)**2-
     1            2*T2(5,2,2)*VXI(13)**2 ) / ( 2* XI(13)*VXI(13) )      
      T2(5,1,6) = ( RTL(15)+ RTL(16)- 2*RTL(1) - 2*T2(5,1,1)* XI(15)**2-
     1            2*T2(5,6,6)*DELP(15)**2) / ( 2* XI(15)*DELP(15))      
      T2(5,2,6) = ( RTL(17)+ RTL(18)- 2*RTL(1) - 2*T2(5,2,2)*VXI(17)**2-
     1            2*T2(5,6,6)*DELP(17)**2) / ( 2*VXI(17)*DELP(17))      
      T2(5,3,4) = ( RTL(19)- RTL(20)           ) /( 2* YI(19)*VYI(19) ) 
C****                                                                   
C****                                                                   
      PRINT 22,  ( ( R(IR, IJ), IJ=1,6), IR=1,6)                        
   22 FORMAT(1H1, / 51X, 15H *TRANSFORM* 1  ,  / 6(25X, 6F10.5/)  )     
      PRINT 120                                                         
  120 FORMAT(   /46X, 25H  *2ND ORDER TRANSFORM*           )            
      DO 24 I1= 1,5                                                     
      DO 25 I2= 1,6                                                     
      PRINT 121, ( I1,I3,I2, T2(I1,I3,I2), I3=1,I2 )                    
  121 FORMAT( 6(I4,I2,I1, 1PE11.3)  )                                   
   25 CONTINUE                                                          
      PRINT 122                                                         
  122 FORMAT( 1H  )                                                     
   24 CONTINUE                                                          
      XTTT=((XO(33)- XO(34) )/2. - R(1,2)*VXI(33) )/VXI(33)**3          
      XTPP  = (XO(27) - XO(28) + XO(6) -XO(5))/(2.*VXI(27)*VYI(27)**2)  
      XXTT  = (XO(37) - XO(36) + XO(35)-XO(38)- 2.*(XO( 3) - XO( 4) ) )/
     1   (4.*XI(35) * VXI(35)**2 )                                      
      XXXT  = (XO(35) - XO(37) + XO(36)-XO(38)- 2.*(XO(33) - XO(34) ) )/
     1   (4.*XI(35)**2*VXI(35))                                         
      XTTD  = (XO(39) - XO(40) + XO(41)-XO(42)- 2.*(XO(11) - XO(12) ) )/
     1   (4.*VXI(39)**2*DELP(39))                                       
      XTDD  = (XO(39) - XO(41) + XO(40)-XO(42)- 2.*(XO(33) - XO(34) ) )/
     1   (4.*VXI(39)*DELP(39)**2)                                       
      XXPP  = (XO(23) - XO(24) + XO( 4)-XO( 3))/(2.*XI(23)*VYI(23)**2  )
      XPPD  = (XO(31) - XO(32) + XO(12)-XO(11))/(2.*VYI(31)**2*DELP(31))
      XTTTT=((XO(33)+XO(34) )/2. - T2(1,2,2)*VXI(33)**2)/ VXI(33)**4    
      XTTPP = (XO(27) - XO( 5) + XO(28)-XO( 6) - 2.*XO( 9) ) /          
     1   ( 2.*VXI(27)**2*VYI(27)**2 )                                   
      XPPDD = (XO(31) - XO(11) + XO(32)-XO(12) - 2.*XO( 9) ) /          
     1   ( 2.*VYI(31)**2 * DELP(31)**2 )                                
      XPPPP =(XO(43) -T2(1,4,4)*VYI(43)**2) / VYI(43)**4                
      ZDDD = ( (RTL(45) - RTL(46) )/2. - R(5,6)*DELP(45) )/DELP(45)**3  
      ZDDDD = ( (RTL(45)+RTL(46)-2*RTL(1) )/2. -T2(5,6,6)*DELP(45)**2)/ 
     1   DELP(45)**4                                                    
      XDDD = (( XO(45)- XO(46))/2. - R(1,6)*DELP(45) ) / DELP(45)**3    
      XDDDD= (( XO(45)+ XO(46))/2. - T2(1,6,6)*DELP(45)**2 )/DELP(45)**4
      TDDD = ((VXO(45)-VXO(46))/2. - R(2,6)*DELP(45) ) / DELP(45)**3    
      TDDDD= ((VXO(45)+VXO(46))/2. - T2(2,6,6)*DELP(45)**2 )/DELP(45)**4
      PRINT 26, XTTT, XTPP, XXTT, XXXT, XTTD, XTDD, XXPP, XPPD,         
     1   XTTTT, XTTPP, XPPDD, XPPPP,                                    
     2   XDDD, XDDDD, TDDD, TDDDD,      ZDDD, ZDDDD                     
   26 FORMAT('1',/15X, 'X/THETA**3       =' 1PE11.3   /                 
     1            15X, 'X/THETA.PHI**2   =' 1PE11.3   /                 
     2            15X, 'X/X.THETA**2     =' 1PE11.3   /                 
     3            15X, 'X/X**2.THETA     =' 1PE11.3   /                 
     4            15X, 'X/THETA**2.DELTA =' 1PE11.3   /                 
     5            15X, 'X/THETA.DELTA**2 =' 1PE11.3   /                 
     6            15X, 'X/X.PHI**2       =' 1PE11.3   /                 
     7            15X, 'X/PHI**2.DELTA   =' 1PE11.3   //                
     8            15X, 'X/THETA**4       =' 1PE11.3   /                 
     9            15X, 'X/THETA**2.PHI**2=' 1PE11.3   /                 
     A            15X, 'X/PHI**2.DELTA**2=' 1PE11.3   /                 
     B            15X, 'X/PHI**4         =' 1PE11.3   //                
     C            15X, 'X/DELTA*3        =' 1PE11.3   /                 
     D            15X, 'X/DELTA*4        =' 1PE11.3   /                 
     E            15X, 'THETA/DELTA*3    =' 1PE11.3   /                 
     F            15X, 'THETA/DELTA*4    =' 1PE11.3   /                 
     H            15X, 'Z/DELTA*3        =' 1PE11.3   /                 
     I            15X, 'Z/DELTA*4        =' 1PE11.3   )                 
      DO 1  I1=1,5                                                      
      DO 1  I2=1,6                                                      
      DO 1  I3=1,6                                                      
    1 TT(I1,I2,I3) = T2(I1,I2,I3)                                       
      DO 2 I=1,12                                                       
      PSI =  5. * FLOAT(I)                                              
      TPSI = .001*DTAN( PSI/57.29578 )                                  
      TT(1,1,1) = T2(1,1,1) + R(2,1) * R(1,1) * TPSI                    
      TT(1,1,2) = T2(1,1,2) + ( R(2,1)*R(1,2) + R(2,2)*R(1,1) ) * TPSI  
      TT(1,2,2) = T2(1,2,2) + R(2,2) * R(1,2) * TPSI                    
      TT(1,1,6) = T2(1,1,6) + ( R(2,1)*R(1,6) + R(2,6)*R(1,1) ) * TPSI  
      TT(1,2,6) = T2(1,2,6) + ( R(2,2)*R(1,6) + R(2,6)*R(1,2) ) * TPSI  
      TT(1,6,6) = T2(1,6,6) + R(2,6) * R(1,6) * TPSI                    
      TT(3,1,3) = T2(3,1,3) + R(1,1) * R(4,3) * TPSI                    
      TT(3,1,4) = T2(3,1,4) + R(1,1) * R(4,4) * TPSI                    
      TT(3,2,3) = T2(3,2,3) + R(1,2) * R(4,3) * TPSI                    
      TT(3,2,4) = T2(3,2,4) + R(1,2) * R(4,4) * TPSI                    
      TT(3,3,6) = T2(3,3,6) + R(1,6) * R(4,3) * TPSI                    
      TT(3,4,6) = T2(3,4,6) + R(1,6) * R(4,4) * TPSI                    
      CTTT=XTTT+ ( R(1,2)*T2(2,2,2) + R(2,2)*T2(1,2,2) ) * TPSI         
      CTPP=XTPP+ ( R(1,2)*T2(2,4,4) + R(2,2)*T2(1,4,4) ) * TPSI         
      CXTT=XXTT+ ( R(1,1)*T2(2,2,2) + R(1,2)*T2(2,1,2) +                
     1             R(2,1)*T2(1,2,2) + R(2,2)*T2(1,1,2) ) * TPSI         
      CXXT=XXXT+ ( R(1,1)*T2(2,1,2) + R(1,2)*T2(2,1,1) +                
     1             R(2,1)*T2(1,1,2) + R(2,2)*T2(1,1,1) ) * TPSI         
      CTTD=XTTD+ ( R(1,2)*T2(2,2,6) + R(1,6)*T2(2,2,2) +                
     1             R(2,2)*T2(1,2,6) + R(2,6)*T2(1,2,2) ) * TPSI         
      CTDD=XTDD+ ( R(1,2)*T2(2,6,6) + R(1,6)*T2(2,2,6) +                
     1             R(2,2)*T2(1,6,6) + R(2,6)*T2(1,2,6) ) * TPSI         
      CXPP=XXPP+ ( R(1,1)*T2(2,4,4) + R(2,1)*T2(1,4,4) ) * TPSI         
      CPPD=XPPD+ ( R(1,6)*T2(2,4,4) + R(2,2)*T2(1,4,4) ) * TPSI         
      PRINT 27, PSI                                                     
   27 FORMAT(1H1, 35X,'FOCAL PLANE TILT ANGLE= ' F07.2, '   DEGREES '  )
      PRINT 28,  ( ( R(IR, IJ), IJ=1,6), IR=1,6)                        
   28 FORMAT(     / 51X, 15H *TRANSFORM* 1  ,  / 6(25X, 6F10.5/)  )     
      PRINT 120                                                         
      DO 29 I1= 1,5                                                     
      DO 30 I2= 1,6                                                     
      PRINT 121, ( I1,I3,I2, TT(I1,I3,I2), I3=1,I2 )                    
   30 CONTINUE                                                          
      PRINT 122                                                         
   29 CONTINUE                                                          
      PRINT 26, CTTT, CTPP, CXTT, CXXT, CTTD, CTDD, CXPP, CPPD,         
     1   XTTTT, XTTPP, XPPDD, XPPPP,                                    
     2   XDDD, XDDDD, TDDD, TDDDD,      ZDDD, ZDDDD                     
    2 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE  MLTT ( BFLD, Z, X, Y )                                
C****                                                                   
C****                                                                   
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8  K, L,bbx,by,bz,tc,dtc    
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLK100/  W, L, D, DG, S, BF, BT                          
      COMMON  /BLK101/  C0, C1, C2, C3, C4, C5, C6, C7, C8              
      DIMENSION TC(6), DTC(6)                                           
      U = 2.*X/W                                                        
      S = 2.*Z/L                                                        
      DL2 = (L/D)**2                                                    
      W1 = C0 + C1*U + C2*U*U + C3*U**3 + C4*U**4 + C5*U**5             
      W2 = 1. + C7*( S**4 + DL2*C8*S**8 ) / ( 1. + DL2*C8 )             
      BFLD = BF*W1 / W2                                                 
      RETURN                                                            
      END                                                               
      SUBROUTINE MTRX1( M, JEN, NEN, NR, ENERGY, AMASS ) !JDL 15-NOV-83 
C****                                                                   
C****                              !Changes from here... !JDL 10-MAR-84 
C**** MM=14,  14 RAYS ARE USED TO EVALUATE THE ABERRATION COEFFICIENTS  
C****            FOR A POINT SOURCE OBJECT THROUGH 4'TH ORDER.          
C**** MM= 6,   6 RAYS ARE USED TO EVALUATE THE ABERRATION COEFFICIENTS  
C****            FOR A POINT SOURCE THROUGH 4'TH ORDER; MIDPLANE ONLY.  
C**** MM= 2,   2 RAYS ARE USED TO EVALUATE THE FOCAL PLANE ONLY.
C****            (FOR OTHER MM VALUES, THE NEXT LOWER NUMBER IS USED.)
C****                                  !...down to here. !JDL 10-MAR-84
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
c      REAL*4 DAET, TYME                                  !JDL 31-OCT-84
      character *4 nwd
      REAL*8 KT, LP                                                     
      character*8    L(2,36), LX(2,12), LD(6)                 !JDL 15-NOV-83 
      LOGICAL LPLT
      COMMON  /BLCK00/  LPLT
      include 'rtcomm0.f'
      COMMON  /BLCK 1/  XI, YI, ZI, VXI, VYI, VZI, DELP, DELM      !JDL 
      COMMON  /BLCK 2/  XO, YO, ZO, VXO, VYO, VZO, RTL(100), RLL(100)   
      COMMON  /BLCK 3/  XINT, YINT, ZINT, TH0, PH0, TL1                 
      COMMON  /BLCK16/  NLOOP,NPASS,IP,NCSV,KEEP(20),    !JDL 17-NOV-83
     1                  LOOPSV(5,30),HOOPSV(30),HSAVE(30),PMSV(3),
     2                  CXXSV(12,3),CSV(36,3),CDSV(6,4,3)!JDL 17-NOV-83
      DIMENSION XI(100), YI(100), ZI(100), VXI(100), VYI(100), VZI(100),
     1          DELP(100)                                               
      DIMENSION XO(100), YO(100), ZO(100), VXO(100), VYO(100), VZO(100) 
      DIMENSION CXX(12,10), IX(12), CD(6,4),   LFACT(50), C(50,10)      
      DIMENSION DXX(21,10), DXY(21,10)                                  
      DIMENSION PMASS(10)                                !JDL 29-NOV-83
      DATA KEEP / 3, 8,14,18,19,24,25,50,51,52,          !JDL 15-NOV-83
     1           54,55,58,66,99,99,99,99,99,99/          !JDL 17-NOV-83
      DATA IX/ 1,2,5,7,11,13,19,22,29,32,35,36  /                       
      DATA LFACT / 1,0,1,0,2*4,2*3,4,3,2*7,2*6,2*7,2*6,3*10,3*9,        
     1   2*10,2*9,3*13,3*12,4,7,14*0   /                                
      DATA  L  / 'X/TH    ','       =','T/TH    ','       =',           
     1           'Y/PH    ','       =','P/PH    ','       =',           
     2           'X/TH**2 ','       =','X/PH**2 ','       =',           
     3           'T/TH**2 ','       =','T/PH**2 ','       =',           
     4           'Y/TH*PH ','       =','P/TH*PH ','       =',           
     5           'X/TH**3 ','       =','X/TH*PH*','*2     =',           
     6           'T/TH**3 ','       =','T/TH*PH*','*2     =',           
     7           'Y/PH**3 ','       =','Y/TH**2*','PH     =',           
     8           'P/PH**3 ','       =','P/TH**2*','PH     =',           
     9           'X/TH**4 ','       =','X/TH**2*','PH**2  =',           
     A           'X/PH**4 ','       =','T/TH**4 ','       =',           
     B           'T/TH**2*','PH**2  =','T/PH**4 ','       =',           
     C           'Y/TH**3*','PH     =','Y/TH*PH*','*3     =',           
     D           'P/TH**3*','PH     =','P/TH*PH*','*3     =',           
     E           'X/TH**5 ','       =','X/TH**3*','PH**2  =',           
     F           'X/TH*PH*','*4     =','T/TH**5 ','       =',           
     G           'T/TH**3*','PH**2  =','T/TH*PH*','*4     =',           
     H           'X/T**2+T','2*X/T**4','X/T**3+T','2*X/T**5'/     
      DATA  LX / 'ENERGY(M','EV)    =','XINT(CM)','       =',           
     1           'YINT(CM)','       =','ZINT(CM)','       =',           
     2           'TH  (MR)','       =','PHI (MR)','       =',           
     3           '!XMAX!(C','M)     =','2!YMAX!(','CM)    =',           
     4           '!X-WAIST','!(cm)  =','X(X-WAIS','T)     =',           
     5           'Z(X-WAIS','T)     =','LENGTH(C','M)     ='  /         
      DATA  LD / 'X/      ','X/T     ','X/T**2  ','X/T**3  ',
     1           'X/P**2  ','X/T*P**2'/                  !JDL 15-NOV-83
C****                                                                   
        MM=M                                                            
C****                                                                   
      I   = JEN                                                         
      IF( I   .GT. 10 ) I   = 10                                        
C****                                                                   
C****                                                                   
      XMIN = XO(1)                                                      
      XMAX = XO(1)                                                      
      YMAX = DABS(YO(1))                                                
      DO 4 J=2,NR                                                       
      IF( XO(J) .GT. XMAX )  XMAX = XO(J)                               
      IF( XO(J) .LT. XMIN )  XMIN = XO(J)                               
      IF( DABS(YO(J) ) .GT. YMAX ) YMAX = DABS(YO(J))                   
    4 CONTINUE                                                          
      PMASS(I)   = AMASS                                 !JDL 10-NOV-83
      CXX(1,I  ) = ENERGY                                               
      CXX(2,I  ) = XINT                                                 
      CXX(3,I  ) = YINT                                                 
      CXX(4,I  ) = ZINT                                                 
      CXX(5,I  ) = TH0                                                  
      CXX(6,I  ) = PH0                                                  
      CXX(7,I  ) = DABS(XMAX-XMIN)                                      
      CXX(8,I  ) = 2.*YMAX                                              
C****                                                                   
C****   CALCULATE BEAM WIDTH AT TEN EQUALLY SPACED (5MM)                
C****   DISTANCES EACH SIDE OF ZINT                                     
C****                                                                   
        DO 20 JJ=1,21                                                   
        XMIN = XO(1) + 0.00050 * VXO(1) * (JJ-11)                       
        XMAX = XMIN                                                     
        DO 21 J = 2, 6                                                  
        XJJ = XO(J) + 0.00050 *VXO(J) * (JJ-11)                         
        IF (XJJ.GT.XMAX) XMAX = XJJ                                     
        IF (XJJ.LT.XMIN) XMIN = XJJ                                     
21      CONTINUE                                                        
        DXX(JJ,I) = DABS( XMAX - XMIN)                                  
        DXY(JJ,I) = 0.                                                  
        IF (NR.LE.6) GOTO 20                                            
        DO 22 J=7,NR                                                    
        XJJ = XO(J) + 0.00050* VXO(J) * (JJ-11)                         
        IF ( XJJ.GT.XMAX ) XMAX = XJJ                                   
        IF ( XJJ.LT.XMIN ) XMIN = XJJ                                   
22      CONTINUE                                                        
        DXY(JJ,I) = DABS( XMAX - XMIN)                                  
20        CONTINUE                                                      
C****                                                                   
C****     CALCULATE POSITION OF MINIMUM BEAM WIDTH                      
C****     WITHIN 10.0 CM OF ZINT                                        
        XMX = 1.0D20                                                    
        DO 25  JJ=1, 101                                                
        XMIN = XO(1) + 0.00020 * VXO(1) * (JJ-51)                       
        XMAX = XMIN                                                     
        DO 26  J=2, 6                                                   
        XJJ = XO(J) + 0.00020 * VXO(J) * (JJ-51)                        
        IF ( XJJ.GT.XMAX ) XMAX = XJJ                                   
        IF ( XJJ.LT.XMIN ) XMIN = XJJ                                   
26      CONTINUE                                                        
        DXMAX = DABS( XMAX - XMIN )                                     
        IF ( DXMAX.GE.XMX ) GO TO 25                                    
        XMX = DXMAX                                                     
        ZMX = 0.20 * (JJ - 51)                                          
25      CONTINUE                                                        
        IF ( DABS( ZMX ).GT.9.9 ) ZMX = 1.0D20                          
        CXX( 9, I) = XMX                                                
        CXX(10, I) = .001*TH0*ZMX + XINT                                
        CXX(11, I) = ZMX+ZINT                                           
        CXX(12, I) = TL1
C****                                                                   
C****                                     
      IF( MM .LT. 6 ) GO TO 1                            !JDL 10-MAR-84
      IF( VXI(2) .EQ. 0. )  VXI(2) = 1.D-30
      IF( VXI(3) .EQ. 0. )  VXI(3) = 1.D-30                             
      KT = VXI(5)/VXI(3)                                                
      DTH = VXI(3)                                                      
      TMAX = VXI(5)                                                     
      XT=XO(2)/VXI(2)                                                   
      TT=(KT**3*(VXO(3)-VXO(4))- VXO(5)+VXO(6))/(2.* (KT**3-KT)*DTH)    
      XTT = ( KT**4*(XO(3) + XO(4)) - (XO(5)+XO(6) )) /                 
     1   (2.*(KT**4-KT**2) *DTH*DTH)                                    
      TTT = ( KT**4*(VXO(3)+VXO(4)) -(VXO(5)+VXO(6))) /                 
     1   (2.*(KT**4-KT**2) *DTH*DTH)                                    
      XTTT  = ( KT**5 * ( XO(3) - XO(4) - 2.*XT*DTH ) -                 
     1 ( XO(5) - XO(6) -2.*KT*XT*DTH) ) / (2.*(KT**5 - KT**3) *DTH**3 ) 
      TTTT  = (-KT    * (VXO(3) -VXO(4)) + (VXO(5) -VXO(6) )  ) /       
     1   (2.*(KT**3 - KT   ) *DTH**3 )                                  
      XTTTT = ( (XO(5)+XO(6))-KT*KT*(XO(3)+XO(4) ) ) /                  
     1   (2.*(KT**4 - KT*KT)*DTH**4 )                                   
      TTTTT =((VXO(5)+VXO(6))-KT*KT*(VXO(3)+VXO(4))) /                  
     1   (2.*(KT**4 - KT*KT)*DTH**4 )                                   
      XTTTTT= ( XO(5) - XO(6) - 2.*KT*XT*DTH - KT**3*( XO(3) - XO(4) -  
     1   2.*XT*DTH) ) / ( 2.*(KT**5 - KT**3) *DTH**5 )                  
      TTTTTT= 0.                                                        
C****                                                                   
C****                                                                   
      C( 1,I)      = XT*10.                                             
      C( 2,I)      = TT                                                 
      C( 5,I)      = XTT*10.**4                                         
      C( 7,I)      = TTT*10.**3                                         
      C(11,I)      = XTTT*10.**7                                        
      C(13,I)      = TTTT*10.**6                                        
      C(19,I)      = XTTTT*10.**10                                      
      C(22,I)      = TTTTT*10.**09                                      
      C(29,I)      = XTTTTT*10.**13                                     
      C(32,I)      = TTTTTT*10.**12                                     
      C(35,I)      = (XTT + XTTTT*TMAX*TMAX)*10.**4                     
      C(36,I)      = (XTTT+XTTTTT*TMAX*TMAX)*10.**7                     
C****                                                                   
C****                                                                   
      IF( MM .LT. 14 ) GO TO 1                           !JDL 10-MAR-84 
      LP = VYI(12)/VYI(7)                                               
      DPH = VYI(7)                                                      
      XPP   = (LP**4*XO(7) - XO(12)) /((LP**4 - LP*LP)*DPH*DPH )        
      TPP   = (LP**4*VXO(7)-VXO(12)) /((LP**4 - LP*LP)*DPH*DPH )        
      XPPPP = (XO(12)-LP*LP*XO(7) ) /((LP**4-LP*LP)*DPH**4)             
      TPPPP =(VXO(12)-LP*LP*VXO(7)) /((LP**4-LP*LP)*DPH**4)             
      XTPP  = (LP**4*( XO(8) - XO(9)) - ( XO(13) - XO(14)) - (LP**4-1.)*
     1   ( XO(3) - XO(4)) -(( XO(10) - XO(11)) - KT*( XO(8) - XO(9) ) - 
     2   ( XO(5) - XO(6) ) + KT*( XO(3) - XO(4) ) ) *                   
     3   ( ( LP**4 - LP*LP) / (KT**3-KT) ))/(2.*(LP**4-LP*LP)*          
     4   DTH*DPH*DPH )                                                  
      TTPP  = 0.                                                        
      XTTPP = ( ( XO(8)+XO(9) ) - ( XO(3)+XO(4) ) - 2.*XO(7)) /         
     1   (2.*DTH*DTH*DPH*DPH)                                           
      TTTPP = ( (VXO(8)+VXO(9)) - (VXO(3)+VXO(4)) -2.*VXO(7)) /         
     1   (2.*DTH*DTH*DPH*DPH)                                           
      YP    = ( LP**3 * YO(7) - YO(12) ) / ( (LP**3 - LP)*DPH )         
      PP    = ( LP**3 *VYO(7) -VYO(12) ) / ( (LP**3 - LP)*DPH )         
      YPPP  = (YO(12) - LP*YO(7)) /((LP**3-LP)*DPH**3 )                 
      PPPP  =(VYO(12) -LP*VYO(7)) /((LP**3-LP)*DPH**3 )                 
      YTTP  = ( YO(8) + YO(9) - 2.*YO(7) ) / (2.*DTH*DTH*DPH )          
      PTTP  = (VYO(8) +VYO(9) - 2.*VYO(7)) / (2.*DTH*DTH*DPH )          
      YTPPP = ( YO(13) - LP*YO(8) - YO(12) + LP*YO(7) ) /               
     1   ((LP**3 - LP)*DTH*DPH**3 )                                     
      PTPPP = (VYO(13) - LP*VYO(8)-VYO(12) + LP*VYO(7)) /               
     1   ((LP**3 - LP)*DTH*DPH**3 )                                     
      YTTTP = ( YO(10) - YO(11) -KT*(YO(8)-YO(9) ) ) /                  
     1   (2.*(KT**3-KT) * DTH**3*DPH )                                  
      PTTTP = (VYO(10) -VYO(11) -KT*(VYO(8)-VYO(9))) /                  
     1   (2.*(KT**3-KT) * DTH**3*DPH )                                  
      YTP   = ( (YO(10)-YO(11) -KT**3*(YO(8)-YO(9) ) ) /(2.*(KT-KT**3))-
     1   YTPPP*DTH*DPH**3 ) /(DTH*DPH)                                  
      PTP   = ((VYO(10)-VYO(11)-KT**3*(VYO(8)-VYO(9))) /(2.*(KT-KT**3))-
     1   PTPPP*DTH*DPH**3 ) /(DTH*DPH)                                  
      XTTTPP= ( XO(10) - XO(11) - KT*( XO(8) - XO(9)) - ( XO(5) - XO(6))
     1   +KT*( XO(3) - XO(4) ) ) / (2.*(KT**3-KT) * DTH**3*DPH*DPH )    
      TTTTPP= 0.                                                        
      XTPPPP= ( XO(13) - XO(14) - LP*LP*( XO(8) - XO(9)) + (LP*LP-1.) * 
     1   ( XO(3) - XO(4) ) ) / (2.*(LP**4-LP*LP) * DTH*DPH**4 )         
      TTPPPP= 0.                                                        
      C( 3,I)      = YP*10.                                             
      C( 4,I)      = PP                                                 
      C( 6,I)      = XPP*10.**4                                         
      C( 8,I)      = TPP*10.**3                                         
      C( 9,I)      = YTP*10.**4                                         
      C(10,I)      = PTP*10.**3                                         
      C(12,I)      = XTPP*10.**7                                        
      C(14,I)      = TTPP*10.**6                                        
      C(15,I)      = YPPP*10.**7                                        
      C(16,I)      = YTTP*10.**7                                        
      C(17,I)      = PPPP*10.**6                                        
      C(18,I)      = PTTP*10.**6                                        
      C(20,I)      = XTTPP*10.**10                                      
      C(21,I)      = XPPPP*10.**10                                      
      C(23,I)      = TTTPP*10.**09                                      
      C(24,I)      = TPPPP*10.**09                                      
      C(25,I)      = YTTTP*10.**10                                      
      C(26,I)      = YTPPP*10.**10                                      
      C(27,I)      = PTTTP*10.**09                                      
      C(28,I)      = PTPPP*10.**09                                      
      C(30,I)      = XTTTPP*10.**13                                     
      C(31,I)      = XTPPPP*10.**13                                     
      C(33,I)      = TTTTPP*10.**12                                     
      C(34,I)      = TTPPPP*10.**12                                     
C****                                                                   
C****                                                                   
      IF((IP .GT. 500) .AND. (NEN .GT. 1)) RETURN        !JDL 19-NOV-83
   13 FORMAT( 2I5 )
   14 FORMAT(   )                                                       
  151 FORMAT( //, 15X, 2A8,  F9.4  )                     !JDL 11-MAR-84 
  152 FORMAT(     15X, 16HMASS(AMU)      =, F9.4, / )    !JDL 11-MAR-84
  153 FORMAT(  7( 15X, 2A8,  F9.4 /  ) /,3( 15X, 2A8, F8.3/))      !JDL 
   16 FORMAT(    15X, 2A8, 1PE12.3, 0PF15.4   )                         
      PRINT 151,  ( LX(J,1),J=1,2),  CXX(1,I)            !JDL 11-MAR-84
      PRINT 152,  AMASS                                  !JDL 11-MAR-84
      PRINT 153,( ( LX(J,K),J=1,2),  CXX(K,I), K=2,12)   !JDL 11-MAR-84 
      DO 2 JJ=1,36                                                      
      COEF = C(JJ,I)/ 10.**LFACT(JJ)                                    
      IF( (JJ.EQ. 5).OR.(JJ.EQ. 11).OR.(JJ.EQ.19).OR.(JJ.EQ.29))PRINT 14
    2 PRINT 16, (L(J,JJ), J=1,2), COEF, C(JJ,I)                         
      GO TO 23                                                          
C****                                                                   
C****                                                                   
    1 CONTINUE                                                          
      IF((IP .GT. 500) .AND. (NEN .GT. 1)) RETURN        !JDL 19-NOV-83
      PRINT 151,  ( LX(J,1),J=1,2),  CXX(1,I)            !JDL 11-MAR-84
      PRINT 152,  AMASS                                  !JDL 11-MAR-84
      PRINT 153,( ( LX(J,K),J=1,2),  CXX(K,I), K=2,12)   !JDL 11-MAR-84 
      IF( MM .LT. 6) RETURN                              !JDL 11-MAR-84
      DO 3 JJ=1,12                                                      
      K = IX(JJ)                                                        
      COEF = C(K,I)/10.**LFACT(K)                                       
    3 PRINT 16, ( L(J,K),J=1,2), COEF, C(K,I)                           
C****                                                                   
C****   PRINT OUT BEAM WIDTH                                            
C****                                                                   
23      CONTINUE                                                        
        IF(IP .GT. 500) RETURN                           !JDL 19-NOV-83
        PRINT 29                                                        
        DO 24 JJ=1, 21                                                  
        DZ = 0.50 * (JJ-11)                                             
        PRINT 30, DZ, DXX(JJ,I), DXY(JJ,I)                              
24      CONTINUE                                                        
29      FORMAT ('1', 3X, 'IMAGE SIZE !XMAX!(CM)', //2X, 'DZ (CM)',      
     1  2X, '  1-6  ', 2X, '  1-NR')                                    
30      FORMAT (F8.2, 2F9.3)                                            
        RETURN                                                          
C****                                                                   
C****                                                                   
      ENTRY MPRNT( NEN, WIDTH )                          !JDL  1-DEC-83 
C****                                                                   
      DO 44 J=1,NEN                                      !JDL 29-NOV-83
   44 CXX(7,J) = WIDTH                                   !JDL  1-DEC-83
      IF( NEN .EQ. 1 ) GO TO 200                         !JDL 17-NOV-83
      IF( LPLT) WRITE(2,13) NEN, MM
  181 FORMAT(   4X, 16HMASS(AMU)      =, 10F11.3 )       !JDL 10-NOV-83
   18 FORMAT(   4X, 2A8, 10F11.3      )                                 
      IF( NEN .GT. 10 )  NEN = 10                                       
      PRINT 14                                                          
      DO 8 K=1,8                                                        
      IF( LPLT ) WRITE(2,18)(LX(J,K),J=1,2),(CXX(K,I),I=1,NEN)
      PRINT 18,   ( LX(J,K),J=1,2),(CXX(K,I),I=1,NEN)    !JDL 10-MAR-84 
      IF( K .NE. 1 ) GO TO 8                             !JDL 10-MAR-84
      IF( LPLT ) WRITE(2,18) ( PMASS(I),I=1,NEN )        !JDL 10-MAR-84
      PRINT 181, ( PMASS(I),I=1,NEN )                    !JDL 10-MAR-84
      PRINT 14                                           !JDL 10-MAR-84
    8 CONTINUE                                           !JDL 10-MAR-84
      PRINT 14                                                          
C****                                                                   
      IF(MM .LT.  6 )  GO TO 200                         !JDL 10-MAR-84
      IF(MM .LT. 14 )  GO TO 5                           !JDL 10-MAR-84 
      DO 7 K=1,36                                                       
      IF( (K .EQ. 5).OR.(K .EQ. 11).OR.(K .EQ.19).OR.(K .EQ.29))PRINT 14
      IF( LPLT ) WRITE(2,18) (L(J,K),J=1,2),(C(K,I),I=1,NEN )
    7 PRINT 18,   ( L(J,K),J=1,2),(C(K,I),I=1,NEN )                     
      GO TO 19                                                          
    5 DO 6 JJ=1,12                                                      
      K = IX(JJ)                                                        
      IF( LPLT ) WRITE(2,18) ( L(J,K),J=1,2), ( C(K,I), I=1,NEN)
    6 PRINT 18, ( L(J,K),J=1,2),(C(K,I), I=1,NEN )                      
C****                                                                   
C**** CHROMATIC ABERRATION COEFFICIENTS                                 
C**** CALCULATED ONLY FOR CASE OF NEN= 5 ENERGIES                       
C****                                                                   
   19 CONTINUE                                                          
      IF( NEN .NE. 5 ) GO TO 200                         !JDL 17-NOV-83 
      DEL = CXX(1,4)/CXX(1,3) - 1.                                      
      DO 9 I=1,6                                                        
C**** IF( I .EQ. 1 ) K=2                !From here ...   !JDL 15-NOV-83 
C**** IF( I .EQ. 2 ) GO TO 10                                           
C**** IF( I .EQ. 3 ) K=5                                                
C**** IF( I .EQ. 4 ) K=11                                               
C**** IF( I .EQ. 5 ) K=19                                               
C**** IF( I .EQ. 6 ) K=29                                               
C**** IF( I .GT. 2 ) GO TO 11                                           
      IF( I .EQ. 1 ) K=2                                                
      IF( I .EQ. 2 ) K=1        
      IF( I .EQ. 3 ) K=5                                                
      IF( I .EQ. 4 ) K=11                                               
      IF( I .EQ. 5 ) K=6
      IF( I .EQ. 6 ) K=12
      IF( I .NE. 1 ) GO TO 11           ! ... to here.   !JDL 15-NOV-83
      X1 =(CXX(K,1) - CXX(K,3))/100.                                    
      X2 =(CXX(K,2) - CXX(K,3))/100.                                    
      X4 =(CXX(K,4) - CXX(K,3))/100.                                    
      X5 =(CXX(K,5) - CXX(K,3))/100.                                    
      GO TO 12                                                          
   11 X1 = C(K,1) - C(K,3)                                              
      X2 = C(K,2) - C(K,3)                                              
      X4 = C(K,4) - C(K,3)                                              
      X5 = C(K,5) - C(K,3)                                              
   12 CD(I,1) = (8. *(X4-X2) - (X5-X1) )/(12.  *DEL)                    
      CD(I,2) = (16.* (X4+X2) - (X5+X1) )/(24.  *DEL*DEL)               
      CD(I,3) = ( (X5-X1) - 2.*(X4-X2) )/(12.  *DEL**3)                 
      CD(I,4) = ( (X5+X1) - 4.*(X4+X2) )/(24.  *DEL**4)                 
      GO TO 9                                                           
   10 Z1 =(CXX(4,1) - CXX(4,3))/100.                                    
      Z2 =(CXX(4,2) - CXX(4,3))/100.                                    
      Z4 =(CXX(4,4) - CXX(4,3))/100.                                    
      Z5 =(CXX(4,5) - CXX(4,3))/100.                                    
      TPSI = (8.* (Z4-Z2) - (Z5-Z1) ) / (8.* (X4-X2) - (X5-X1) )        
      PSI = 57.29578D0 * DATAN(TPSI)                                    
      DZ1 = Z1 - X1*TPSI                                                
      DZ2 = Z2 - X2*TPSI                                                
      DZ4 = Z4 - X4*TPSI                                                
      DZ5 = Z5 - X5*TPSI                                                
      CD(I,1) = -C(2,3)*( 8.*(DZ4-DZ2) - (DZ5-DZ1) )/(12.  *DEL)        
      CD(I,2) = -C(2,3)*( 16.*(DZ4+DZ2) - (DZ5+DZ1) )/(24.  *DEL*DEL)   
      CD(I,3) = -C(2,3)*( (DZ5-DZ1) - 2.*(DZ4-DZ2) )/(12.  *DEL**3)     
      CD(I,4) = -C(2,3)*( (DZ5+DZ1) - 4.*(DZ4+DZ2) )/(24.  *DEL**4)     
    9 CONTINUE                                                          
      PRINT 14                                                          
      PRINT 17, PSI, (I,I=1,4), ( (CD(K,I),I=1,4), K=1,6 )              
      IF( LPLT ) WRITE(2,17) PSI, (I,I=1,4), ( ( CD(K,I),I=1,4), K=1,6 )
   17 FORMAT(4X,'PSI            =', F11.3,/4X,'N              =',4(I7,  
     1  4X),/4X,'X/D**N         =',4F11.3,/4X,'X/T*D**N       =',4F11.3,
     2      /4X,'X/T**2*D**N    =',4X,1P4E11.3,                         
     3      /4X,'X/T**3*D**N    =',4X,1P4E11.3,                         
     4      /4X,'X/P**2*D**N    =',4X,1P4E11.3,          !JDL 15-NOV-83 
     5      /4X,'X/T*P**2*D**N  =',4X,1P4E11.3  )        !JDL 15-NOV-83 
C**** New code added from here...                        !JDL 15-NOV-83
C****
C****
C**** STORE DATA FOR INITIAL CENTRAL ENERGY AND SUBSEQUENT PAIRS OF
C**** RUNS HAVING PARAMETER SHIFTS OF +STEP AND -STEP.  AFTER EVERY
C**** TWO SUCH PAIRS, PRINT CENTRAL VALUE PLUS FIRST AND SECOND
C**** DIFFERENTIAL CHANGES.
C****
  200 IF(NLOOP .EQ. 0) GO TO 260
      NCSV=NCSV+1
      IF(NCSV .NE. 3) GO TO 206
      IF(NEN .EQ. 5) PRINT 270, NTITLE
      PRINT 271
      KUPLE=LOOPSV(4,NPASS)
      DO 204 J=NPASS,NLOOP
      IF(LOOPSV(4,J) .NE. KUPLE) GO TO 204
      IF((KUPLE .EQ. 4    ) .AND. (J .NE. NPASS)) GO TO 204
      INO =LOOPSV(1,J)
      IROW=LOOPSV(2,J)
      ICOL=LOOPSV(3,J)
      IJ  =LOOPSV(5,J)
      NWD=NWORD(IDATA(INO))
      A0=HSAVE(J)
      A1=A0+HOOPSV(J)
      A2=A0-HOOPSV(J)
      PRINT 272, NWD, ITITLE(INO), IROW, ICOL, KUPLE, A2, A0, A1
  204 CONTINUE
      PRINT 273
      HDATA=HOOPSV(NPASS)
  206 JJ=1
      KK=1
      NCTR=(NEN+1)/2
      NP5=MOD(IP,10)
      PMSV(NCSV)=PMASS(NCTR)
      IF(NCSV .NE. 3) GO TO 210
      IF((KEEP(JJ) .GT. KK) .AND. (NP5 .EQ. 5)) GO TO 210
      JJ=JJ+1
      A0=PMSV(1)
      A1=(PMSV(2)-PMSV(3))/(2.0*HDATA)
      A2=(PMSV(2)+PMSV(3)-2.0*PMSV(1))/(HDATA**2)
      PRINT 274, A0,A1,A2
  210 CONTINUE
      DO 220 I=1,12
      KK=KK+1
      CXXSV(I,NCSV)=CXX(I,NCTR)
      IF(NCSV .NE. 3) GO TO 220
      IF((KEEP(JJ) .GT. KK) .AND. (NP5 .EQ. 5)) GO TO 220
      JJ=JJ+1
      A0=CXXSV(I,1)
      A1=(CXXSV(I,2)-CXXSV(I,3))/(2.0*HDATA)
      A2=(CXXSV(I,2)+CXXSV(I,3)-2.0*CXXSV(I,1))/(HDATA**2)
      PRINT 276, (LX(K,I),K=1,2),A0,A1,A2
  220 CONTINUE
      DO 230 I=1,36
      KK=KK+1
      CSV(I,NCSV)=C(I,NCTR)
      IF(NCSV .NE. 3) GO TO 230
      IF((KEEP(JJ) .GT. KK) .AND. (NP5 .EQ. 5)) GO TO 230
      JJ=JJ+1
      A0=CSV(I,1)
      A1=(CSV(I,2)-CSV(I,3))/(2.0*HDATA)
      A2=(CSV(I,2)+CSV(I,3)-2.0*CSV(I,1))/(HDATA**2)
      PRINT 276, (L(K,I),K=1,2),A0,A1,A2
  230 CONTINUE
      IF(NEN .NE. 5) GO TO 250
      DO 240 I=1,6
      DO 240 J=1,4
      KK=KK+1
      CDSV(I,J,NCSV)=CD(I,J)
      IF(NCSV .NE. 3) GO TO 240
      IF((KEEP(JJ) .GT. KK) .AND. (NP5 .EQ. 5)) GO TO 240
      JJ=JJ+1
      A0=CDSV(I,J,1)
      A1=(CDSV(I,J,2)-CDSV(I,J,3))/(2.0*HDATA)
      A2=(CDSV(I,J,2)+CDSV(I,J,3)-2.0*CDSV(I,J,1))/(HDATA**2)
      PRINT 278, LD(I),J,A0,A1,A2
  240 CONTINUE
  250 IF(NCSV .LT. 3) RETURN
      NCSV=1   !SEQUENCE: (1,2,3);(1,4,5);ETC.
C****
  260 CONTINUE
  270 FORMAT( 1H1, 10X, 20A4 )
  271 FORMAT( /,4X,'DIFFERENTIALS CALCULATED FROM THREE ',
     1       'DATA RUNS:  (CENTER-STEP, DATA CENTER, CENTER+STEP)',/)
  272 FORMAT( 4X,A4,' (',A4,')  [LINE',I2,', ENTRY',I2,
     1       ', GROUP (',A4,')]   ',3F13.6 )
  273 FORMAT( /,4X,24X,'A0',14X,'A1',14X,'A2',/)
  274 FORMAT( 4X, 'MASS(AMU)      =',1P,3E16.6 )
  276 FORMAT( 4X,2A8,1P,3E16.6 )
  278 FORMAT( 4X, A8,'D**',I1,'   =',1P,3E16.6 )
C****
C**** ...down to here.                                   !JDL 15-NOV-83
      RETURN                                                            
      END                                                               
      SUBROUTINE MULT   ( NO, NP, T, TP ,NUM )                          
C****                                                                   
C****                                                                   
C**** MULTIPOLE     RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIAL
C**** EQUATIONS OF MOTION.                                              
C     T = TIME                                                          
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
c      REAL*4 DAET, TYME                                  !JDL 31-OCT-84
      REAL*8  LF, K, L                                                  
      include 'rtcomm0.f'
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLK100/  W, L, D, DG, S, BF, BT                          
      COMMON  /BLK101/  C0, C1, C2, C3, C4, C5, C6, C7, C8              
C*JDL DIMENSION DATA(  75,30 ), ITITLE(30)               !JDL 17-NOV-83 
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             
      EXTERNAL BMULT                                                    
C**** DATA  C/ 3.D10/                                                   
C****                                                                   
      JRAYGAS = JRAY*GAS
      LF   = DATA(  1,NO )                                              
      DG   = DATA(  2,NO )                                              
      A    = DATA( 10,NO )                                              
      B    = DATA( 11,NO )                                              
      L    = DATA( 12,NO )                                              
      W    = DATA( 13,NO )                                              
      D    = DATA( 14,NO )                                              
      BF   = DATA( 15,NO )                                              
      Z1   = DATA( 16,NO )                                              
      Z2   = DATA( 17,NO )                                              
      C0   = DATA( 20,NO )                                              
      C1   = DATA( 21,NO )                                              
      C2   = DATA( 22,NO )                                              
      C3   = DATA( 23,NO )                                              
      C4   = DATA( 24,NO )                                              
      C5   = DATA( 25,NO )                                              
      C6   = DATA( 26,NO )                                              
      C7   = DATA( 27,NO )                                              
      C8   = DATA( 28,NO )                                              
      DTF = LF/VEL                                                      
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      S = 0.                                                            
C****                                                                   
      IF( NP  .GT. 100 ) GO TO 5                                        
      PRINT 100, ITITLE(NO)                                             
  100 FORMAT(  ' MULTIPOLE  ****  ', A4,'  *************************'/) 
      PRINT 101                                                         
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HBX, 8X, 4HY CM , 7X, 2HBY,
     1   8X, 4HZ CM, 7X, 2HBZ, 8X, 6HVELZ/C , 6X, 8HTHETA MR , 5X,      
     2   6HPHI MR , 6X, 1HB             )                               
      CALL PRNT2 ( T,S,XA   ,YA   ,ZA   ,BX,BY,BZ,BT,VXA  ,VYA  ,VZA   )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      PRINT 103                                                         
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO CENTERED AXIS SYSTEM ' ) 
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.         
C****                                                                   
    5 TC(1) =  XA                                                       
      TC(2) = YA                                                        
      TC(3) = ZA - (A+L/2.)                                             
      TC(4) =  VXA                                                      
      TC(5) =  VYA                                                      
      TC(6) =  VZA                                                      
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** TRANSLATE PARTICLE TO START OF FIRST FRINGE FIELD                 
C****                                                                   
      TDT = ( Z1 - TC(3)  ) /DABS( TC(6) )                              
C****                                                                   
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
      IF( NP  .LE. 100) PRINT 104                                       
  104 FORMAT( 24H0MULTIPOLE FIELD REGION  )                             
      CALL FNMIRK( 6, T, DTF ,TC, DTC, DS, ES, BMULT, 0    )            
      NSTEP = 0                                                         
    6 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 7 I = 1, NP                                                    
      CALL FNMIRK( 6, T, DTF ,TC, DTC, DS, ES, BMULT, 1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( Z2  .LE. TC(3) ) GO TO 8                                      
    7 CONTINUE                                                          
      GO TO 6                                                           
    8 CONTINUE                                                          
      XDTF  =-( TC(3) - Z2  ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTF ,TC, DTC, DS, ES,BMULT,  0    )            
      CALL FNMIRK( 6, T,XDTF ,TC, DTC, DS, ES,BMULT,  1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
  105 FORMAT( 10H   NSTEPS=  I5 )                                       
C****                                                                   
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 
C****                                                                   
      TC(3) = TC(3) - (B+L/2.)                                          
      IF( NP  .LE. 100) PRINT 109                                       
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
C****                                                                   
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           
C****                                                                   
      TDT = -TC(3) /DABS( TC(6) )                                       
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      TP = T * VEL                                                      
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      S  = 0.                                                           
      VXF    = 1000. *DATAN2( TC(4), TC(6)  )                           
      VYF    = 1000. *DASIN ( TC(5)/ VEL    )                           
      VZF    = TC(6) / VEL                                              
      IF( NP  .LE. 100) PRINT 115,TP,TC(1),TC(2),TC(3),VZF,VXF,VYF      
  115 FORMAT( F10.4, 10X, F10.3, 11X, F10.3, 11X, F10.3, 11X,           
     1   F13.5, F13.2, F11.2                   )                        
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** CALCULATE INTERCEPTS IN SYSTEM D                                  
C****                                                                   
C****                                                                   
C****                                                                   
      Z0X = -TC(1)/ ( TC(4) / TC(6)    + 1.E-10 )                       
      Z0Y = -TC(2)/ ( TC(5) / TC(6)    + 1.E-10 )                       
      IF( NP  .LE. 100) PRINT 111, VXF, VYF, Z0X, Z0Y                   
  111 FORMAT( / ' INTERSECTIONS WITH VER. AND HOR. PLANES '          ,  
     X       /15X, 5H  XP=F10.4, 10H MR    YP= F10.4, 3H MR   /         
     1        15X, 5H Z0X=F10.2, 10H CM   Z0Y= F10.2, 3H CM   /        )
      RETURN                                                            
99      CALL PRNT4(NO, IN)                                              
        RETURN                                                          
      END                                                               
      SUBROUTINE MULTPL ( NO, NP, T, TP ,NUM )                          
C****                                                                   
C****                                                                   
C**** QUADRUPOLE    RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIAL
C**** EQUATIONS OF MOTION.                                              
C     T = TIME                                                          
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
c      REAL*4 DAET, TYME                                  !JDL 31-OCT-84
      REAL*8  LF1, LF2, LU1, K, L                                       
      include 'rtcomm0.f'
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       
      COMMON  /BLCK 7/ NCODE                                            
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK50/  D,BGRAD, S, BT                                  
      COMMON  /BLCK51/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK52/  IN                                              
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
C*JDL DIMENSION DATA(  75,30 ), ITITLE(30)               !JDL 17-NOV-83 
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             
      EXTERNAL  BFLD                                                    
C**** DATA  C/ 3.D10/                                                   
C****                                                                   
      JRAYGAS = JRAY*GAS
      LF1  = DATA(  1,NO )                                              
      LU1  = DATA(  2,NO )                                              
      LF2  = DATA(  3,NO )                                              
      A    = DATA( 10,NO )                                              
      B    = DATA( 11,NO )                                              
      L    = DATA( 12,NO )                                              
      RAD  = DATA( 13,NO )                                              
      BF   = DATA( 14,NO )                                              
      Z11  = DATA( 15,NO )                                              
      Z12  = DATA( 16,NO )                                              
      Z21  = DATA( 17,NO )                                              
      Z22  = DATA( 18,NO )                                              
      DTF1= LF1/ VEL                                                    
      DTF2= LF2/ VEL                                                    
      DTU = LU1/ VEL                                                    
      D = 2. * RAD                                                      
      BGRAD = (-1)**NCODE * BF/RAD**NCODE                               
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      S = 0.                                                            
C****                                                                   
      IF( NP  .GT. 100 ) GO TO 5                                        
  201 FORMAT(  ' QUADRUPOLE  ****  ', A4, '  ***********************'/) 
  202 FORMAT(  ' HEXAPOLE    ****  ', A4, '  ***********************'/) 
  203 FORMAT(  ' OCTAPOLE    ****  ', A4, '  ***********************'/) 
  204 FORMAT(  ' DECAPOLE    ****  ', A4, '  ***********************'/) 
      GO TO ( 21, 22, 23, 24 ) , NCODE                                  
   21 PRINT 201, ITITLE(NO)                                             
      GO TO 25                                                          
   22 PRINT 202, ITITLE(NO)                                             
      GO TO 25                                                          
   23 PRINT 203, ITITLE(NO)                                             
      GO TO 25                                                          
   24 PRINT 204, ITITLE(NO)                                             
   25 PRINT 101                                                         
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HBX, 8X, 4HY CM , 7X, 2HBY,
     1   8X, 4HZ CM, 7X, 2HBZ, 8X, 6HVELZ/C , 6X, 8HTHETA MR , 5X,      
     2   6HPHI MR , 6X, 1HB             )                               
      CALL PRNT2 ( T,S,XA   ,YA   ,ZA   ,BX,BY,BZ,BT,VXA  ,VYA  ,VZA   )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      PRINT 103                                                         
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO B AXIS SYSTEM '       )  
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.         
C****                                                                   
    5 TC(1) = -XA                                                       
      TC(2) = YA                                                        
      TC(3) = A - ZA                                                    
      TC(4) = -VXA                                                      
      TC(5) = VYA                                                       
      TC(6) = -VZA                                                      
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** TRANSLATE PARTICLE TO START OF FIRST FRINGE FIELD                 
C****                                                                   
      TDT = ( TC(3) - Z11 ) /DABS( TC(6) )                              
C****                                                                   
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** IN DESIGNATES FIELD REGIONS FOR QUADRUPOLE                        
C****                                                                   
      IN = 1                                                            
      C0   = DATA( 19,NO )                                              
      C1   = DATA( 20,NO )                                              
      C2   = DATA( 21,NO )                                              
      C3   = DATA( 22,NO )                                              
      C4   = DATA( 23,NO )                                              
      C5   = DATA( 24,NO )                                              
      IF( NP  .LE. 100) PRINT 104                                       
  104 FORMAT( 22H0FRINGING FIELD REGION    )                            
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BFLD , 0    )            
      NSTEP = 0                                                         
    6 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 7 I = 1, NP                                                    
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BFLD , 1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( Z12 .GE. TC(3) ) GO TO 8                                      
    7 CONTINUE                                                          
      GO TO 6                                                           
    8 CONTINUE                                                          
      XDTF1 =-( Z12 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,BFLD ,  0    )            
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,BFLD ,  1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
  105 FORMAT( 10H   NSTEPS=  I5 )                                       
C***                                                                    
C***  UNIFORM FIELD REGION                                              
C**** TRANSFORM TO SECOND VFB COORD SYSTEM                              
C***                                                                    
      BGRAD = (-1)**NCODE *  BGRAD                                      
      TC(1) = -TC(1)                                                    
      TC(3) = -TC(3) - L                                                
      TC(4) = -TC(4)                                                    
      TC(6) = -TC(6)                                                    
C****                                                                   
C****                                                                   
C**** UNIFORM FIELD INTEGRATION REGION                                  
C****                                                                   
C****                                                                   
      IN = 2                                                            
      IF( NP  .LE. 100) PRINT 106                                       
  106 FORMAT(   '0UNIFORM FIELD REGION IN C AXIS SYSTEM '  )            
      IF( TC(3)  .LT.  Z21 ) GO TO 15                                   
C****                                                                   
C**** THIS SECTION CORRECTS FOR MAGNETS WHOSE FRINGING FIELDS INTERSECT 
C****                                                                   
      IF( NP  .LE. 100) PRINT 102                                       
  102 FORMAT( / '   INTEGRATE BACKWARDS    '  )                         
      CALL FNMIRK( 6, T,-DTU ,TC, DTC, DS, ES, BFLD,  0    )            
      NSTEP = 0                                                         
   16 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 17  I =1, NP                                                   
      CALL FNMIRK( 6, T,-DTU, TC, DTC, DS, ES, BFLD,  1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( TC(3)  .LE.  Z21 )  GO TO 18                                  
   17 CONTINUE                                                          
      GO TO 16                                                          
   18 CONTINUE                                                          
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, BFLD,  0    )            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, BFLD,  1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
      IF( NP  .LE. 100) PRINT 107                                       
  107 FORMAT( / )                                                       
      GO TO 19                                                          
C****                                                                   
C****                                                                   
   15 CONTINUE                                                          
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, BFLD , 0    )            
      NSTEP = 0                                                         
    9 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 10  I =1, NP                                                   
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, BFLD , 1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
   19 CONTINUE                                                          
      IF( TC(3)  .GE.  Z21 )  GO TO 11                                  
   10 CONTINUE                                                          
      GO TO 9                                                           
   11 CONTINUE                                                          
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,BFLD ,  0    )            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,BFLD ,  1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
C***                                                                    
C***                                                                    
C**** SETUP FOR SECOND FRINGE FIELD AND INTEGRATION                     
C****                                                                   
C****                                                                   
      C0   = DATA( 25,NO )                                              
      C1   = DATA( 26,NO )                                              
      C2   = DATA( 27,NO )                                              
      C3   = DATA( 28,NO )                                              
      C4   = DATA( 29,NO )                                              
      C5   = DATA( 30,NO )                                              
      IN = 3                                                            
      IF( NP  .LE. 100) PRINT 104                                       
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, BFLD , 0    )            
      NSTEP = 0                                                         
   12 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 13  I =1, NP                                                   
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, BFLD , 1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( TC(3) .GE. Z22 )  GO TO 14                                    
   13 CONTINUE                                                          
      GO TO 12                                                          
   14 CONTINUE                                                          
      XDTF2 = ( Z22 - TC(3) ) / TC(6)                                   
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BFLD , 0    )            
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BFLD , 1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
C****                                                                   
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 
C****                                                                   
      TC(3) = TC(3) - B                                                 
      IF( NP  .LE. 100) PRINT 109                                       
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
C****                                                                   
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           
C****                                                                   
      TDT = -TC(3) /DABS( TC(6) )                                       
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      TP = T * VEL                                                      
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      S  = 0.                                                           
      VXF    = 1000. *DATAN2( TC(4), TC(6)  )                           
      VYF    = 1000. *DASIN ( TC(5)/ VEL    )                           
      VZF    = TC(6) / VEL                                              
      IF( NP  .LE. 100) PRINT 115,TP,TC(1),TC(2),TC(3),VZF,VXF,VYF      
  115 FORMAT( F10.4, 10X, F10.3, 11X, F10.3, 11X, F10.3, 11X,           
     1   F13.5, F13.2, F11.2                   )                        
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** CALCULATE INTERCEPTS IN SYSTEM D                                  
C****                                                                   
C****                                                                   
C****                                                                   
      Z0X = -TC(1)/ ( TC(4) / TC(6)    + 1.E-10 )                       
      Z0Y = -TC(2)/ ( TC(5) / TC(6)    + 1.E-10 )                       
      IF( NP  .LE. 100) PRINT 111, VXF, VYF, Z0X, Z0Y                   
  111 FORMAT( / ' INTERSECTIONS WITH VER. AND HOR. PLANES '          ,  
     X       /15X, 5H  XP=F10.4, 10H MR    YP= F10.4, 3H MR   /         
     1        15X, 5H Z0X=F10.2, 10H CM   Z0Y= F10.2, 3H CM   /        )
      RETURN                                                            
99      CALL PRNT4 (NO, IN)                                             
        RETURN                                                          
      END                                                               
      SUBROUTINE NDIP                                                   
C****                                                                   
C****                                                                   
C**** MTYP = 3 OR 4                                                     
C**** THIS VERSION OF BFUN IS MAINLY FOR NONUNIFORM FIELD MAGNETS       
C**** THE CENTRAL FIELD REGION IS REPRESENTED TO 3'RD ORDER ON-AND-     
C**** OFF THE MIDPLANE BY ANALYTIC EXPRESSIONS. SEE SLAC NO. 75         
C**** FRINGE FIELD REGIONS REPRESENTED BY FERMI TYPE FALL-OFF           
C**** ALONG WITH RADIAL FALL-OFF                                        
C**** COMPONENTS OF 'B' IN FRINGE REGION EVALUATED BY NUMERICAL METHODS 
C****                                                                   
C****                                                                   
C**** THE RELATIONSHIP BETWEEN B0, ......... B12 AND B(I,J) RELATIVE TO 
C**** AXES (Z,X) IS GIVEN BY                                            
C****                                                                   
C****                                                                   
C**** B0  = B( 0, 0 )                                                   
C**** B1  = B( 1, 0 )                                                   
C**** B2  = B( 2, 0 )                                                   
C**** B3  = B( 1, 1 )                                                   
C**** B4  = B( 1,-1 )                                                   
C**** B5  = B( 0, 1 )                                                   
C**** B6  = B( 0, 2 )                                                   
C**** B7  = B( 0,-1 )                                                   
C**** B8  = B( 0,-2 )                                                   
C**** B9  = B(-1, 0 )                                                   
C**** B10 = B(-2, 0 )                                                   
C**** B11 = B(-1, 1 )                                                   
C**** B12 = B(-1,-1 )                                                   
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8  NDX, BX, BY, BZ, K, TC, DTC
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT,CSC                          
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8,SCOR           
      COMMON  /BLCK22/  D, DG, S, BF, BT                                
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK24/  RB, XC, ZC                                      
      COMMON  /BLCK25/  IN, MTYP                                        
      DIMENSION TC(6), DTC(6)                                           
      X = TC(1)                                                         
      Y = TC(2)                                                         
      Z = TC(3)                                                         
      DX = X - XC                                                       
      DZ = Z - ZC                                                       
      RP =DSQRT( DX**2 + DZ**2 )                                        
      DR = RP - RB                                                      
      GO TO ( 1, 2, 3, 14 ), IN                                         
    7 PRINT 8, IN, MTYP                                                 
      CALL EXIT                                                         
    8 FORMAT (    '0 ERROR -GO TO -  IN BFUN   IN=', I3, '   MTYP=',I4 )
    2 DRR1 = DR/RB                                                      
      DRR2 = DRR1*DRR1                                                  
      DRR3 = DRR2*DRR1                                                  
      DRR4 = DRR3*DRR1                                                  
      IF( Y .NE. 0. )  GO TO 4                                          
      BX = 0.                                                           
      BY = 0.                                                           
      IF( MTYP .EQ. 3) BY=                                              
     1     BF* ( 1. - NDX*DRR1 + BET1*DRR2 + GAMA*DRR3 + DELT*DRR4 )    
      IF( MTYP .EQ. 4) BY= BF/ (1. + NDX*DRR1 )                         
      BZ = 0.                                                           
      BT = BY                                                           
      RETURN                                                            
    4 YR1 = Y/RB                                                        
      YR2 = YR1*YR1                                                     
      YR3 = YR2*YR1                                                     
      YR4 = YR3*YR1                                                     
      RR1 = RB/RP                                                       
      RR2 = RR1*RR1                                                     
      RR3 = RR2*RR1                                                     
      IF( MTYP .EQ. 3 ) GO TO 11                                        
      IF( MTYP .EQ. 4 ) GO TO 12                                        
      GO TO 7                                                           
   11 BRR = BF*( ( -NDX + 2.*BET1*DRR1 + 3.*GAMA*DRR2 + 4.*DELT*DRR3 )  
     1   *YR1 - (NDX*RR2 + 2.*BET1*RR1*(1.-RR1*DRR1) +                  
     2   3.*GAMA*( 2. + 2.*RR1*DRR1 - RR2*DRR2 ) +                      
     3   4.*DELT*( 6.*DRR1 + 3.*RR1*DRR2 - RR2*DRR3 ))*YR3/6. )         
      BY = BF* ( 1. - NDX*DRR1 + BET1*DRR2 + GAMA*DRR3 + DELT*DRR4 -    
     1   .5*YR2*( -NDX*RR1 + 2.*BET1*( 1. + RR1*DRR1) +                 
     2   3.*GAMA*DRR1*( 2. + RR1*DRR1) + 4.*DELT*DRR2*(3. + RR1*DRR1) ) 
     3   + YR4*( -NDX*RR3 + 2.*BET1*( RR3*DRR1 - RR2) +                 
     4   3.*GAMA*( 4.*RR1 - 2.*RR2*DRR1 + RR3*DRR2 ) +                  
     5   4.*DELT*( 6. + 12.*RR1*DRR1 - 3.*RR2*DRR2 + RR3*DRR3 ) )/24. ) 
      GO TO 13                                                          
   12 DNR1 = 1. + NDX*DRR1                                              
      DNR2 = DNR1*DNR1                                                  
      DNR3 = DNR2*DNR1                                                  
      DNR4 = DNR3*DNR1                                                  
      DNR5 = DNR4*DNR1                                                  
      BRR = BF*NDX*( -YR1/DNR2 + YR3*( 6.*NDX*NDX/DNR4 -                
     1   2.*NDX*RR1/DNR3 - RR2/DNR2 ) /6.  )                            
      BY = BF*( 1./DNR1 + .5*YR2*NDX*( -2.*NDX/DNR3 + RR1/DNR2) +       
     2   YR4*NDX*( 24.*NDX**3 /DNR5 - 12.*NDX*NDX*RR1/DNR4 -            
     3   2.*NDX*RR2/DNR3 - RR3/DNR2 ) /24.  )                           
   13 BX = BRR*DX/RP                                                    
      BZ = BRR*DZ/RP                                                    
      BT  =DSQRT(BX*BX + BY*BY + BZ*BZ)                                 
      RETURN                                                            
C****                                                                   
C****                                                                   
    1 SINE = -1.                                                        
      GO TO 5                                                           
    3 SINE = 1.                                                         
    5 IF( Z  .GT. 0. ) DR = X * SINE*CSC                                
      CALL NDPP( B0, Z, X, Y, DR      )                                 
      IF( Y  .NE. 0. )  GO TO 6                                         
      BX = 0.                                                           
      BY = B0                                                           
      BZ = 0.                                                           
      BT   = B0                                                         
      RETURN                                                            
C****                                                                   
C****                                                                   
    6 IF( Z .GT. 0. )  GO TO 9                                          
      DR1  =       (DSQRT( DX**2 + (DZ+DG)**2 ) - RB )                  
      DR2  =       (DSQRT( DX**2 + (DZ+2.*DG)**2 ) - RB )               
      DR3  =       (DSQRT( (DX+DG)**2 + (DZ+DG)**2 )  - RB )            
      DR4  =       (DSQRT( (DX-DG)**2 + (DZ+DG)**2 )  - RB )            
      DR5  =       (DSQRT( (DX+DG)**2 + DZ**2 ) - RB )                  
      DR6  =       (DSQRT( (DX+ 2.*DG)**2 + DZ**2 ) - RB )              
      DR7  =       (DSQRT( (DX-DG)**2 + DZ**2 ) - RB )                  
      DR8  =       (DSQRT( (DX- 2.*DG)**2 + DZ**2 ) - RB )              
      DR9  =       (DSQRT( DX**2 + (DZ-DG)**2 ) - RB )                  
      DR10 =       (DSQRT( DX**2 + (DZ-2.*DG)**2 ) - RB )               
      DR11 =       (DSQRT( (DX+DG)**2 + (DZ-DG)**2 )  - RB )            
      DR12 =       (DSQRT( (DX-DG)**2 + (DZ-DG)**2 )  - RB )            
      GO TO 10                                                          
    9 DR1  = SINE* X*CSC                                                
      DR2  = DR1                                                        
      DR9  = DR1                                                        
      DR10 = DR1                                                        
      DR3  = SINE* ( X + DG )*CSC                                       
      DR5  = DR3                                                        
      DR11 = DR3                                                        
      DR4  = SINE*( X - DG )*CSC                                        
      DR7  = DR4                                                        
      DR12 = DR4                                                        
      DR6  = SINE* ( X + 2.*DG )*CSC                                    
      DR8  = SINE* ( X - 2.*DG )*CSC                                    
C****                                                                   
C****                                                                   
   10 CALL NDPP ( B1 , Z + DG, X , Y , DR1 )                            
      CALL NDPP ( B2 , Z + 2.*DG, X , Y , DR2 )                         
      CALL NDPP ( B3 , Z + DG, X + DG , Y , DR3 )                       
      CALL NDPP ( B4 , Z + DG, X - DG , Y , DR4 )                       
      CALL NDPP ( B5 , Z , X + DG , Y, DR5 )                            
      CALL NDPP ( B6 , Z , X + 2.*DG , Y , DR6 )                        
      CALL NDPP ( B7 , Z , X - DG , Y, DR7 )                            
      CALL NDPP ( B8 , Z , X - 2.*DG , Y , DR8 )                        
      CALL NDPP ( B9 , Z - DG, X , Y , DR9 )                            
      CALL NDPP ( B10, Z - 2.*DG, X, Y, DR10 )                          
      CALL NDPP ( B11, Z - DG, X + DG , Y , DR11 )                      
      CALL NDPP ( B12, Z - DG, X - DG , Y , DR12 )                      
      YG1 = Y/DG                                                        
      YG2 = YG1**2                                                      
      YG3 = YG1**3                                                      
      YG4 = YG1**4                                                      
      BX = YG1 * ( (B5-B7)*2./3. - (B6-B8)/12. )  +                     
     1     YG3*( (B5-B7)/6. - (B6-B8)/12. -                             
     2     (B3 + B11 - B4 - B12 - 2.*B5 + 2.*B7 ) / 12. )               
      BY = B0 - YG2*( ( B1 + B9 + B5 + B7 - 4.*B0 ) *2./3. -            
     1     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. ) +                     
     2     YG4* (-( B1 + B9 + B5 + B7 - 4.*B0 ) / 6. +                  
     3     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. +                       
     4     ( B3 + B11 + B4 + B12 - 2.*B1 - 2.*B9 -                      
     5     2.*B5 - 2.*B7 + 4.*B0 ) / 12. )                              
      BZ = YG1*( (B1 - B9 ) *2./3. - ( B2 - B10 ) /12. ) +              
     1     YG3*( ( B1 - B9 ) / 6. - ( B2 - B10 ) / 12. -                
     2     ( B3 + B4 - B11 - B12 - 2.*B1 + 2.*B9 ) / 12.  )             
      BT  =DSQRT(BX*BX + BY*BY + BZ*BZ)                                 
      RETURN                                                            
   14 BX = 0.                                                           
      BY = BR                                                           
      BZ = 0.                                                           
      BT = BR                                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE  NDPP ( BFLD, Z, X, Y , DR )                           
C****                                                                   
C****                                                                   
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      REAL*8  NDX, BX, BY, BZ, K, TC, DTC
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT,CSC                          
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8,SCOR           
      COMMON  /BLCK22/  D, DG, S, BF, BT                                
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK24/  RB, XC, ZC                                      
      COMMON  /BLCK25/  IN, MTYP                                        
      DIMENSION TC(6), DTC(6)                                           
      DRR1 = DR/RB                                                      
      DRR2 = DRR1*DRR1                                                  
      DRR3 = DRR2*DRR1                                                  
      DRR4 = DRR3*DRR1                                                  
C****                                                                   
C**** MTYP    :                  MODIFIED ITERATIVE PROCEDURE           
C****                                                                   
      XP = X                                                            
      XP2 = XP*XP                                                       
      XP3 = XP2*XP                                                      
      XP4 = XP3 * XP                                                    
      ZP = -(S2*XP2 + S3*XP3 + S4*XP4 + S5*XP4*XP + S6*XP4*XP2 +        
     1       S7*XP4*XP3 + S8*XP4*XP4 )                                  
      AZ = (Z-ZP)/10.D0                                                 
      AZMAX = DSQRT(  X*X + Z*Z  )
      IF( AZ  .GT.  AZMAX  )  AZ = AZMAX
      ZSIGN = Z-ZP
      RINV4 = 0.
      DO 11 I=1,21                                                      
      XP   = X + AZ*(I-11)                                              
      XP2 = XP*XP                                                       
      XP3 = XP2*XP                                                      
      XP4 = XP3*XP                                                      
      ZP = -(S2*XP2 + S3*XP3 + S4*XP4 + S5*XP4*XP + S6*XP4*XP2 +        
     1       S7*XP4*XP3 + S8*XP4*XP4 )                                  
      XXP = X-XP                                                        
      ZZP = Z-ZP                                                        
      DD =            XXP*XXP + ZZP*ZZP
      IF( DD  .LT.  1.D-15 )  DD = 1.D-15
      IF( DD  .GT.  1.D15  )  DD = 1.D15
      RINV4 = RINV4 + 1.0D0 / (DD*DD )
   11 CONTINUE                                                          
      DP = DSQRT( 1.D0/RINV4 )
      DP = DSQRT( DP )
      S = 1.9023D0* DSIGN( 1.D0, ZSIGN ) * DP/D - DELS
C****                                                                   
C**** FIRST GUESS FOR CLOSEST POINT IS                                  
C****                                                                   
C*    XP = X                                                            
C*    XP2 = XP*XP                                                       
C*    XP3 = XP2*XP                                                      
C*    XP4 = XP3*XP                                                      
C****                                                                   
C**** CALCULATE ZP ON CURVE FOR CORRESPONDING XP                        
C****                                                                   
C*    ZP = -( S2*XP2 + S3*XP3 + S4*XP4 + S5*XP4*XP + S6*XP4*XP2 +       
C*   1   S7*XP4*XP3 + S8*XP4*XP4 )                                      
C*    ZSIGN = Z-ZP                                                      
C****                                                                   
C**** SLOPE OF CURVE AT XP, ZP                                          
C****                                                                   
C*    DO 4 I=1,3                                                        
C*    DZDXC = -(2.*S2*XP + 3.*S3*XP2+ 4.*S4*XP3 + 5.*S5*XP4 +           
C*   1   6.*S6*XP4*XP + 7.*S7*XP4*XP2 + 8.*S8*XP4*XP3 )                 
C****                                                                   
C**** NEXT APPROXIMATION TO CLOSEST POINT IS                            
C****                                                                   
C*    XP = ( DZDXC*(Z-ZP)  +  DZDXC*DZDXC*XP + X ) / (1.+DZDXC*DZDXC)   
C*    IF( I  .EQ.  1 )  XP = (3.*XP +  X ) / 4.                         
C*    XP2 = XP*XP                                                       
C*    XP3 = XP2*XP                                                      
C*    XP4 = XP3*XP                                                      
C*    ZP = -( S2*XP2 + S3*XP3 + S4*XP4 + S5*XP4*XP + S6*XP4*XP2 +       
C*   1   S7*XP4*XP3 + S8*XP4*XP4 )                                      
C*  4 CONTINUE                                                          
C*    XXP = X-XP                                                        
C*    ZZP = Z-ZP                                                        
C*    S = DSIGN( 1.D0,ZSIGN) * DSQRT( XXP*XXP + ZZP*ZZP) / D - DELS     
C****
C****
C****
C****
      CS=C0+S*(C1+S*(C2+S*(C3+S*(C4+S*C5))))                            
      IF( DABS(CS)  .GT.  70.  )  CS =DSIGN( 70.D0 ,CS  )               
      E=DEXP(CS)                                                        
      P0 = 1.0 + E                                                      
      DB=BF-BR                                                          
      BFLD = 0.                                                         
      IF( MTYP .EQ. 3 ) BFLD =                                          
     1       BR +( 1. - NDX*DRR1 + BET1*DRR2+GAMA*DRR3+DELT*DRR4)*DB/P0 
      IF( MTYP .EQ. 4 ) BFLD = BR + ( 1./(1. +NDX*DRR1) )*DB/P0         
C****
C**** PRINT 100, X, Y, Z,  DR, S, BFLD
C*100 FORMAT( 1P6D15.4 )
C****
      RETURN                                                            
      END                                                               
      SUBROUTINE OPTIC( J, JFOCAL, NP, T, TP )                          
C****                                                                   
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
      COMMON  /BLCK 2/  XO, YO, ZO, VXO, VYO, VZO, RTL(100), RLL(100)   
      COMMON /BLCK 3/ XINT, YINT, ZINT, TH0, PH0, TL1                   
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       
      DIMENSION XO(100), YO(100), ZO(100), VXO(100), VYO(100), VZO(100) 
C**** DATA  C/ 3.D10/                                                   
C****                                                                   
C****                                                                   
  100 FORMAT( /  ' INTERSECTION POINT IN XZ-PLANE OF CENTRAL RAY AND THI
     1S RAY '      )                                                    
  101 FORMAT(  ' (IN D AXIS SYSTEM                 )       '         )  
  102 FORMAT(  ' (IN OPTIC AXIS SYSTEM             )        '        )  
  103 FORMAT( / ' RAY PARAMETERS AT THE FOCAL AXIS SYSTEM  '         )  
  104 FORMAT( / ' COORDINATE TRANSFORMATION TO OPTIC AXIS SYSTEM  '  )  
C****                                                                   
C****                                                                   
C****                                                                   
  105 FORMAT( / '  *****************************************************
     1************************************************************'/  ) 
      IF( NP  .LE. 100) PRINT 105                                       
      IF( J  .GT.  2  )  GO TO 19                                       
      IF( J  .EQ.  1 )  GO TO 15                                        
      IF( J  .EQ. 2)  GO TO 18                                          
      CALL EXIT                                                         
   15 B1X = XA                                                          
      B1Y = YA                                                          
      S1X = VXA/VZA                                                     
      S1Y = VYA/VZA                                                     
      TT = T
      VEL1 = VEL
c-ddc vza1 is only used ONCE, and THIS was UNDONE in j=2 case!      VZA1 = VZA
      S1XP = DATAN2( VXA,VZA )                                          
      COS1 =DCOS(S1XP)                                                  
      SIN1 =DSIN(S1XP)                                                  
      ZZZZ = 0.                                                         
      TT1 = TT*1.0D+09
      TL1 = TT*VEL
        TH0 = 1000. * S1XP                                              
        PH0 = 1000. * DASIN (VYA/VEL)                                   
      GO TO 17                                                          
   18 B2X = XA                                                          
      B2Y = YA                                                          
      S2X = VXA/VZA                                                     
      S2Y = VYA/VZA                                                     
C****                                                                   
C**** CALCULATE CENTRAL AND PARAXIAL RAY INTERCEPTS IN SYSTEM - D       
C****                                                                   
      DSX = S1X-S2X                                                     
      IF( DSX .EQ. 0. )   DSX = 1.D-30                                  
      ZINT =  ( B2X-B1X) /  DSX                                         
      XINT = ( B2X*S1X - B1X*S2X ) /  DSX                               
      YINT = S2Y*ZINT + B2Y                                             
      IF( NP  .GT. 100 ) GO TO 5                                        
      PRINT 100                                                         
      PRINT 101                                                         
      PRINT 114, XINT, YINT, ZINT                                       
  114 FORMAT(  14X, 6HXXINT=   F11.4,  3H CM ,  /                       
     1         14X, 6HYYINT=   F11.4,  3H CM ,  /                       
     2         14X, 6HZZINT=   F11.4,  3H CM ,  /          )            
  115 FORMAT( F10.4, 10X, F10.3, 11X, F10.3, 11X, F10.3, 11X,           
     1   F13.5, F13.2, F11.2                   )                        
C****                                                                   
C**** ALTERATION OF INTERCEPTS TO OPTIC AXIS SYSTEM                     
C****                                                                   
    5 ZINTZ = ZINT*COS1 + (XINT-B1X) *SIN1                              
      XINTX =-ZINT*SIN1 + (XINT-B1X) *COS1                              
      ZZZZ = ZINTZ                                                      
      IF( JFOCAL  .NE.  0 )  ZZZZ = 0.                                  
C****
C**** FLIGHT PATH AND TIME FOR RAY-1 IN FOCAL AXIS SYSTEM
C****
c-ddc vza1 is only used HERE, and was uninitialized in j=2 case!
c      VZA1 = VZA      TT = TT + ZZZZ/DABS(VZA1)
      TT = TT + ZZZZ/DABS(VZA)
      TT1 = TT*1.0D+09
      TL1 = TT*VEL1
      IF( NP  .GT. 100 ) GO TO 17                                       
      PRINT 102                                                         
      PRINT 114, XINTX, YINT, ZINTZ                                     
      GO TO 17                                                          
C****
C**** GENERAL RAY INTERCEPTS IN D-AXIS SYSTEM
C****
   19 BJX = XA                                                          
      BJY = YA                                                          
      SJX = VXA/VZA                                                     
      SJY = VYA/VZA                                                     
      DSX = S1X-SJX                                                     
      IF( DSX .EQ. 0. )   DSX = 1.D-30                                  
      XINT1 = ( BJX*S1X - B1X*SJX ) /  DSX                              
      ZINT1 = ( BJX - B1X ) /  DSX                                      
      YINT1 = SJY*ZINT1 + BJY                                           
      IF( NP  .GT. 100 ) GO TO 17                                       
      PRINT 100                                                         
      PRINT 101                                                         
      PRINT 114, XINT1, YINT1, ZINT1                                    
C****                                                                   
C**** TRANSFORM SYSTEM-D TO OPTIC AXIS SYSTEM                           
C**** TRANSLATE TO (B1X,0) AND ROTATE BY (S1X,0)                        
C****                                                                   
   17 XT = XA                                                           
      ZT = ZA                                                           
      VXT = VXA                                                         
      VZT = VZA                                                         
      IF(JFOCAL .EQ. 3) GO TO 217   !NO CHANGE IN COORDS.!JDL  6-MAR-84
      XA = XT - B1X                                      !JDL  6-MAR-84
      IF(JFOCAL .EQ. 2) GO TO 217   !TRANSLATE IN X ONLY !JDL  6-MAR-84
      ZA    = ZT*COS1 + ( XT-B1X ) *SIN1                                
      XA    =-ZT*SIN1 + ( XT-B1X ) *COS1                                
      VZA   = VZT*COS1 + VXT*SIN1                                       
      VXA   =-VZT*SIN1 + VXT*COS1                                       
  217 CONTINUE                                           !JDL  6-MAR-84
      VXP = 1000. *DATAN2( VXA,VZA )                                    
      VYP = 1000. * DASIN( VYA/VEL )                                    
      VZP = VZA   / VEL                                                 
      TP = T * VEL                                                      
      IF( NP  .GT. 100 ) GO TO 16                                       
      PRINT 104                                                         
C****                                                                   
      PRINT 115, TP, XA,  YA,  ZA,        VZP, VXP, VYP                 
   16 TDT = -ZA    /DABS( VZA   )                                       
      XA = XA       + TDT * VXA                                         
      YA = YA       + TDT * VYA                                         
      ZA = ZA       + TDT * VZA                                         
      T = T + TDT                                                       
      VXP = 1000. *DATAN2( VXA,VZA )                                    
      VYP = 1000. * DASIN( VYA/VEL )                                    
      VZP = VZA   / VEL                                                 
      TP = T * VEL                                                      
C****
C**** TRANSLATE PARTICLE TO FOCAL AXIS SYSTEM
C****
      XINT2= XA    + ZZZZ* VXA/VZA                                      
      YINT2= YA    + ZZZZ* VYA/VZA                                      
      ZINT2 = 0.
C****
C****
      TT = T + ZZZZ/DABS(VZA)
      TTJ = TT*1.0D+09
      TLJ = TT*VEL
C****
C**** PATH LENGTHS AND TIMES RELATIVE TO RAY-1
C****
      TTJ1 = TTJ - TT1
      TLJ1 = TLJ - TL1
C****
C****
      XO(J) = XINT2                                                     
      YO(J) = YINT2                                                     
      ZO(J) = ZA                                                        
      VXO(J) = VXP                                                      
      VYO(J) = VYP                                                      
      VZO(J) = VZP                                                      
C****
C**** SAVE TIME DIFFERENCES IN UNITS OF VELOCITY OF RAY-1
C****
      RTL(J) = TTJ1*VEL1*1.0D-09                                        
      RLL(J) = TLJ1
      IF( NP  .GT. 100 ) RETURN                                         
      PRINT 115, TP, XA,  YA,  ZA,        VZP, VXP, VYP                 
      PRINT 103                                                         
      PRINT 116, XINT2,VXP, YINT2,VYP,ZINT2,TLJ,TLJ1,TTJ,TTJ1           
  116 FORMAT( / 20X, 'X=', F10.4, ' CM', 5X, 'VX=',F10.4,' MR',    /    
     1          20X, 'Y=', F10.4, ' CM', 5X, 'VY=',F10.4,' MR',    /    
     2          20X, 'Z=', F10.4, ' CM'          /                      
     3          20X, 'L=', F10.4, ' CM', 5X,'DL=',F10.4, ' CM' /
     4          20X, 'T=', F10.4, ' NS', 5X,'DT=',F10.4, ' NS' )
      IF( JFOCAL  .NE.  0 )  PRINT 99                                   
   99 FORMAT( / '   FOCAL POS FIXED BY INPUT DATA = IMAGE DISTANCE  '/ )
      RETURN                                                            
      END                                                               
      SUBROUTINE PLTOUT ( JEN, J, NUM )
c
c-ddc clearly repeatedly enters, and changes a arrays (graph,icor,..)
c     which are neither in common blocks or 'save'd 
C****
C****
C**** THIS ROUTINE STORES STEP-BY-STEP POSITION INFORMATION FOR EACH
C**** RAY FOR USE BY PLOTTING ROUTINES.
C****
C****
      IMPLICIT REAL*8 (A-H,O-Z)
      integer maxpoint
      parameter (maxpoint=5000)
      REAL*8 BX, BY, BZ, K, TC, DTC
      LOGICAL LPLT
      COMMON  /BLCK00/ LPLT
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC
      DIMENSION TC(6), DTC(6)
      DIMENSION GRAPH(4,maxpoint), ICOR(maxpoint,2)
C****
C****
C****
      IF( NUM .GT. maxpoint ) NUM = maxpoint
      WRITE (11)   JEN, J, NUM
      WRITE (11)   ( GRAPH(1,IK),IK=1,NUM), ( GRAPH(2,IK),IK=1,NUM),
     1            ( GRAPH(3,IK),IK=1,NUM), ( GRAPH(4,IK),IK=1,NUM)
      WRITE (11)   (  ICOR(IK,1),IK=1,NUM), (  ICOR(IK,2),IK=1,NUM)
      RETURN
C****
C****
      ENTRY   PLT1( NUM, NO, NBR, TPAR )
C****
C****
      IF( .NOT. LPLT ) RETURN
      IF( NUM .GT. maxpoint ) RETURN
      GRAPH( 1,NUM) = TC(1)
      GRAPH( 2,NUM) = TC(2)
      GRAPH( 3,NUM) = TC(3)
      GRAPH( 4,NUM) = TPAR
      ICOR ( NUM,1) = NO
      ICOR ( NUM,2) = NBR
      RETURN
C****
C****
      ENTRY   PLT2( NUM, NO, NBR, TPAR )
C****
C****
      IF( .NOT. LPLT ) RETURN
      IF( NUM .GT. maxpoint ) RETURN
      GRAPH( 1,NUM) = XA
      GRAPH( 2,NUM) = YA
      GRAPH( 3,NUM) = ZA
      GRAPH( 4,NUM) = TPAR
      ICOR ( NUM,1) = NO
      ICOR ( NUM,2) = NBR
      RETURN
      END
      SUBROUTINE POLES  ( NO, NP, T, TP ,NUM )                          
C****                                                                   
C****                                                                   
C**** MULTIPOLE     RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIAL
C**** EQUATIONS OF MOTION.                                              
C     T = TIME                                                          
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
c      REAL*4 DAET, TYME                                  !JDL 31-OCT-84
      REAL*8  LF1, LF2, LU1, L, BX, BY, BZ, K, TC, DTC
      include 'rtcomm0.f'
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK90/  D, S, BT, GRAD1,GRAD2,GRAD3,GRAD4,GRAD5         
      COMMON  /BLCK91/  C0, C1, C2, C3, C4, C5                          
      COMMON  /BLCK92/  IN                                              
C*JDL DIMENSION DATA(  75,30 ), ITITLE(30)               !JDL 17-NOV-83 
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             
      EXTERNAL  BPOLES                                                  
C**** DATA  C/ 3.D10/                                                   
C****                                                                   
      JRAYGAS = JRAY*GAS
      LF1  = DATA(  1,NO )                                              
      LU1  = DATA(  2,NO )                                              
      LF2  = DATA(  3,NO )                                              
      A    = DATA( 10,NO )                                              
      B    = DATA( 11,NO )                                              
      L    = DATA( 12,NO )                                              
      RAD  = DATA( 13,NO )                                              
      BQD  = DATA( 14,NO )                                              
      BHX  = DATA( 15,NO )                                              
      BOC  = DATA( 16,NO )                                              
      BDC  = DATA( 17,NO )                                              
      BDD  = DATA( 18,NO )                                              
      Z11  = DATA( 19,NO )                                              
      Z12  = DATA( 20,NO )                                              
      Z21  = DATA( 21,NO )                                              
      Z22  = DATA( 22,NO )                                              
      DTF1= LF1/ VEL                                                    
      DTF2= LF2/ VEL                                                    
      DTU = LU1/ VEL                                                    
      D = 2. * RAD                                                      
      GRAD1 = -BQD/RAD                                                  
      GRAD2 =  BHX/RAD**2                                               
      GRAD3 = -BOC/RAD**3                                               
      GRAD4 =  BDC/RAD**4                                               
      GRAD5 = -BDD/RAD**5                                               
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      S = 0.                                                            
C****                                                                   
      IF( NP  .GT. 100 ) GO TO 5                                        
      PRINT 100, ITITLE(NO)                                             
  100 FORMAT(  ' MULTIPOLE(POLES)  ****  ', A4,'  ******************'/) 
C****                                                                   
      PRINT 101                                                         
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HBX, 8X, 4HY CM , 7X, 2HBY,
     1   8X, 4HZ CM, 7X, 2HBZ, 8X, 6HVELZ/C , 6X, 8HTHETA MR , 5X,      
     2   6HPHI MR , 6X, 1HB             )                               
      CALL PRNT2 ( T,S,XA   ,YA   ,ZA   ,BX,BY,BZ,BT,VXA  ,VYA  ,VZA   )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      PRINT 103                                                         
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO B AXIS SYSTEM '       )  
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.         
C****                                                                   
    5 TC(1) = -XA                                                       
      TC(2) = YA                                                        
      TC(3) = A - ZA                                                    
      TC(4) = -VXA                                                      
      TC(5) = VYA                                                       
      TC(6) = -VZA                                                      
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** TRANSLATE PARTICLE TO START OF FIRST FRINGE FIELD                 
C****                                                                   
      TDT = ( TC(3) - Z11 ) /DABS( TC(6) )                              
C****                                                                   
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** IN DESIGNATES FIELD REGIONS FOR MULTIPOLE                         
C****                                                                   
      IN = 1                                                            
      C0   = DATA( 23,NO )                                              
      C1   = DATA( 24,NO )                                              
      C2   = DATA( 25,NO )                                              
      C3   = DATA( 26,NO )                                              
      C4   = DATA( 27,NO )                                              
      C5   = DATA( 28,NO )                                              
      IF( NP  .LE. 100) PRINT 104                                       
  104 FORMAT( 22H0FRINGING FIELD REGION    )                            
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BPOLES,0    )            
      NSTEP = 0                                                         
    6 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 7 I = 1, NP                                                    
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BPOLES,1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( Z12 .GE. TC(3) ) GO TO 8                                      
    7 CONTINUE                                                          
      GO TO 6                                                           
    8 CONTINUE                                                          
      XDTF1 =-( Z12 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,BPOLES, 0    )            
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,BPOLES, 1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
  105 FORMAT( 10H   NSTEPS=  I5 )                                       
C***                                                                    
C***  UNIFORM FIELD REGION                                              
C**** TRANSFORM TO SECOND VFB COORD SYSTEM                              
C***                                                                    
      GRAD1 = -GRAD1                                                    
      GRAD2 =  GRAD2                                                    
      GRAD3 = -GRAD3                                                    
      GRAD4 =  GRAD4                                                    
      GRAD5 = -GRAD5                                                    
      TC(1) = -TC(1)                                                    
      TC(3) = -TC(3) - L                                                
      TC(4) = -TC(4)                                                    
      TC(6) = -TC(6)                                                    
C****                                                                   
C****                                                                   
C**** UNIFORM FIELD INTEGRATION REGION                                  
C****                                                                   
C****                                                                   
      IN = 2                                                            
      IF( NP  .LE. 100) PRINT 106                                       
  106 FORMAT(   '0UNIFORM FIELD REGION IN C AXIS SYSTEM '  )            
      IF( TC(3)  .LT.  Z21 ) GO TO 15                                   
C****                                                                   
C**** THIS SECTION CORRECTS FOR MAGNETS WHOSE FRINGING FIELDS INTERSECT 
C****                                                                   
      IF( NP  .LE. 100) PRINT 102                                       
  102 FORMAT( / '   INTEGRATE BACKWARDS    '  )                         
      CALL FNMIRK( 6, T,-DTU ,TC, DTC, DS, ES,BPOLES, 0    )            
      NSTEP = 0                                                         
   16 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 17  I =1, NP                                                   
      CALL FNMIRK( 6, T,-DTU, TC, DTC, DS, ES,BPOLES, 1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( TC(3)  .LE.  Z21 )  GO TO 18                                  
   17 CONTINUE                                                          
      GO TO 16                                                          
   18 CONTINUE                                                          
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,BPOLES, 0    )            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,BPOLES, 1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
      IF( NP  .LE. 100) PRINT 107                                       
  107 FORMAT( / )                                                       
      GO TO 19                                                          
C****                                                                   
C****                                                                   
   15 CONTINUE                                                          
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, BPOLES,0    )            
      NSTEP = 0                                                         
    9 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 10  I =1, NP                                                   
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, BPOLES,1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( TC(3)  .GE.  Z21 )  GO TO 11                                  
   10 CONTINUE                                                          
      GO TO 9                                                           
   11 CONTINUE                                                          
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,BPOLES, 0    )            
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,BPOLES, 1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
   19 CONTINUE                                                          
C***                                                                    
C***                                                                    
C**** SETUP FOR SECOND FRINGE FIELD AND INTEGRATION                     
C****                                                                   
C****                                                                   
      C0   = DATA( 29,NO )                                              
      C1   = DATA( 30,NO )                                              
      C2   = DATA( 31,NO )                                              
      C3   = DATA( 32,NO )                                              
      C4   = DATA( 33,NO )                                              
      C5   = DATA( 34,NO )                                              
      IN = 3                                                            
      IF( NP  .LE. 100) PRINT 104                                       
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, BPOLES,0    )            
      NSTEP = 0                                                         
   12 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 13  I =1, NP                                                   
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, BPOLES,1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( TC(3) .GE. Z22 )  GO TO 14                                    
   13 CONTINUE                                                          
      GO TO 12                                                          
   14 CONTINUE                                                          
      XDTF2 = ( Z22 - TC(3) ) / TC(6)                                   
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BPOLES,0    )            
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BPOLES,1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
C****                                                                   
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 
C****                                                                   
      TC(3) = TC(3) - B                                                 
      IF( NP  .LE. 100) PRINT 109                                       
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
C****                                                                   
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           
C****                                                                   
      TDT = -TC(3) /DABS( TC(6) )                                       
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      TP = T * VEL                                                      
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      S  = 0.                                                           
      VXF    = 1000. *DATAN2( TC(4), TC(6)  )                           
      VYF    = 1000. *DASIN ( TC(5)/ VEL    )                           
      VZF    = TC(6) / VEL                                              
      IF( NP  .LE. 100) PRINT 115,TP,TC(1),TC(2),TC(3),VZF,VXF,VYF      
  115 FORMAT( F10.4, 10X, F10.3, 11X, F10.3, 11X, F10.3, 11X,           
     1   F13.5, F13.2, F11.2                   )                        
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** CALCULATE INTERCEPTS IN SYSTEM D                                  
C****                                                                   
C****                                                                   
C****                                                                   
C****                                                                   
      Z0X = -TC(1)/ ( TC(4) / TC(6)    + 1.E-10 )                       
      Z0Y = -TC(2)/ ( TC(5) / TC(6)    + 1.E-10 )                       
      IF( NP  .LE. 100) PRINT 111, VXF, VYF, Z0X, Z0Y                   
  111 FORMAT( / ' INTERSECTIONS WITH VER. AND HOR. PLANES '          ,  
     X       /15X, 5H  XP=F10.4, 10H MR    YP= F10.4, 3H MR   /         
     1        15X, 5H Z0X=F10.2, 10H CM   Z0Y= F10.2, 3H CM   /        )
      RETURN                                                            
99      CALL PRNT4 (NO, IN)                                             
        RETURN                                                          
      END                                                               
      SUBROUTINE PPLOT (X,Y1,Y2,NFOLD1,NFOLD2,N,XLABEL,NPLOT,D)    !JDL
C
C     PPLOT PREPARES A GRAPHICAL DISPLAY OF N SETS OF DATA POINTS
C     IN 55 ROWS AND 120 COLUMNS ON ONE PRINTED PAGE.  DATA NEED NOT
C     BE ARRANGED IN NUMERICAL ORDER.  SCALES ARE SELECTED AUTOMATICALLY
C     IF D(1) THRU D(4) ARE ZERO; OTHERWISE, D(1) THRU D(4) SET SCALES.
C     THREE KINDS OF DISPLAY ARE PERMITTED:
C        NPLOT      DISPLAY         CHARACTER
C
C          1     X(I) VS. Y1(I)         +
C          2     X(I) VS. Y2(I)         *
C          3     X(I) VS. Y1(I),Y2(I)   +,*
C
C     WHEN NFOLD1 AND/OR NFOLD2 ARE NOT ZERO (OR ONE), +/* CHARACTERS
C     ARE REPLACED BY SUCCESSIVE LETTERS OF THE ALPHABET (UPPER CASE
C     FOR Y(1), LOWER CASE FOR Y2(I)), A NEW CHARACTER BEING USED
C     FOR EACH FOLDING OF THE DATA IN MULTIPLES OF NFOLD.
C 
      IMPLICIT REAL*8(A-H,O-Z)                                          
      INTEGER XLABEL,A
      DIMENSION X(N),Y1(N),Y2(N),XLABEL(16),LCHAR1(27),LCHAR2(27)
      DIMENSION A(119),XNL(13),YNL(13),D(4),LABL(4)
      DATA LABL/1H ,1H.,1H+,1H*/
C
      DATA LCHAR1/1HA,1HB,1HC,1HD,1HE,1HF,1HG,1HH,1HI,1HJ,
     1            1HK,1HL,1HM,1HN,1HO,1HP,1HQ,1HR,1HS,1HT,
     2            1HU,1HV,1HW,1HX,1HY,1HZ,1H+/
      DATA LCHAR2/1Ha,1Hb,1Hc,1Hd,1He,1Hf,1Hg,1Hh,1Hi,1Hj,
     1            1Hk,1Hl,1Hm,1Hn,1Ho,1Hp,1Hq,1Hr,1Hs,1Ht,
     2            1Hu,1Hv,1Hw,1Hx,1Hy,1Hz,1H*/
C
C         (1). FIND XMIN,XMAX,YMIN, AND YMAX.
C
      IF((D(1) .EQ. 0.0) .AND. (D(2) .EQ. 0.0) .AND.
     1   (D(3) .EQ. 0.0) .AND. (D(4) .EQ. 0.0)) GO TO 10
      XMAX=AMAX1(D(1),D(3))
      XMIN=AMIN1(D(1),D(3))
      YMAX=AMAX1(D(2),D(4))
      YMIN=AMIN1(D(2),D(4))
      GO TO 60
C
   10 XMIN=X(1)
      YMIN=Y1(1)
      IF(NPLOT .EQ. 2) YMIN=Y2(1)
      XMAX=XMIN
      YMAX=YMIN
      DO 50 J=1,N
      IF( X(J) .LT. XMIN) XMIN=X(J)
      IF( X(J) .GT. XMAX) XMAX=X(J)
      IF(NPLOT .EQ. 2) GO TO 20
      IF(Y1(J) .LT. YMIN) YMIN=Y1(J)
      IF(Y1(J) .GT. YMAX) YMAX=Y1(J)
      IF(NPLOT .NE. 3) GO TO 50
   20 IF(Y2(J) .LT. YMIN) YMIN=Y2(J)
      IF(Y2(J) .GT. YMAX) YMAX=Y2(J)
   50 CONTINUE
C
C         (2). COMPUTE NUMBER OF X AND Y UNITS PER SCALE DIVISION.
C
   60 DMIN=ABS(XMAX-XMIN)/120.0
      L=1
      GO TO 130
  120 DMIN=ABS(YMAX-YMIN)/55.0
      L=2
  130 EX=1.0E+30
      DO 140 I=1,60
      IF(DMIN .LT. EX) GO TO 140
      DM=DMIN/EX
      GO TO 150
  140 EX=EX/10.0
      DR=EX
      GO TO 160
  150   J=12
      IF(DM .GT. 1.2) J=15
      IF(DM .GT. 1.5)J=20
      IF(DM .GT. 2.0) J=25
      IF(DM .GT. 2.5) J=INT(DM+1.0)*10
      IF(J .EQ. 70) J=80
      IF(J .EQ. 90) J=100
      DR=FLOAT(J)*EX/10.0
  160 IF(L .NE. 1) GO TO 170
C
C         (2A). COMPUTE SLOPE BX, MAJOR DIVISION BIGDX AND FIDUCIAL
C
      DXR=DR
      XEX=EX
      BX=1.0/DXR
      BIGDX=10.0*DXR
      UNX=120.0*DXR-ABS(XMAX-XMIN)
      FXM=(XMIN-UNX/2.0)/BIGDX
      FX=FXM*BIGDX
      IF(ABS(FXM) .LT. 1.0E6)
     1  FX=FLOAT(INT(FXM+SIGN(5D-1,FXM)))*BIGDX
      IF(INT(BX*(XMIN-FX)+0.5) .LT. 1) FX=FX-BIGDX
      IF(INT(BX*(XMAX-FX)+0.5) .LT. 119) GO TO 120
      GO TO 180
C
C         (2B). COMPUTE SLOPE BY, MAJOR DIVISION BIGDY AND FIDUCIAL
C
  170 DYR=DR
      YEX=EX
      BY=1.0/DYR
      BIGDY=5.0*DYR
      UNY= 55.0*DYR-ABS(YMAX-YMIN)
      FYM=(YMIN-UNY/2.0)/BIGDY
      FY=FYM*BIGDY
      IF(ABS(FYM) .LT. 1.0E6)
     1  FY=FLOAT(INT(FYM+SIGN(5D-1,FYM)))*BIGDY
      IF(INT(BY*(YMAX-FY)+0.5) .LT. 55) GO TO 200
C
C         (2C). PLOT EXCEEDS SPACE, CHANGE SCALE AND REPEAT.
C
  180 DM=1.10*DM
      GO TO 150
C
C         (4). DETERMINE SCALE FACTORS FOR X AND Y.
C
  200 CONTINUE
      AFX=ABS(FX+12.0*BIGDX)
      IF(ABS(FX) .GT. AFX) AFX=ABS(FX)
  210 IF((AFX/XEX) .LT. 100.0) GO TO 220
      XEX=XEX*10.0
      GO TO 210
  220 AFY=ABS(FY+11.0*BIGDY)
      IF(ABS(FY) .GT. AFY) AFY=ABS(FY)
  230 IF((AFY/YEX) .LT. 100.0) GO TO 240
      YEX=YEX*10.0
      GO TO 230
  240 CONTINUE
C
C         (5). TRANSFORM X AND Y ARRAYS TO SCALE DIVISIONS.
C
      DO 260 I=1,N
      NX=BX*(X(I)-FX)+.5
      X(I)=NX
      IF(NPLOT .EQ. 2) GO TO 250
      NY=BY*(Y1(I)-FY)+.5
      Y1(I)=NY
      IF(NPLOT .NE. 3) GO TO 260
  250 NY=BY*(Y2(I)-FY)+.5
      Y2(I)=NY
  260 CONTINUE
C
C         (7). PREPARE NUMERICAL LABELS FOR AXES.
C
      JDOT=0
      DO 360 I=1,13
      XNL(I)=(FX+BIGDX*FLOAT(I-1))/XEX
      YNL(I)=(FY+BIGDY*FLOAT(I-1))/YEX
      IF(XNL(I) .EQ. 0.0) JDOT=10*(I-1)
  360 CONTINUE
C
C         (8). PRINT TOP BORDER OF GRAPH.
C
      PRINT 450, YNL(12)
  450 FORMAT( 2H   ,F6.2, 12(10HX+++++++++), 1HX )
C
C         (9). PRINT 55 LINES OF DATA AND Y-AXIS LABELS.
C
      XI=0
      DO 600 I=1,54
      XI=XI+1
      JJ=12-(I/5)
      DO 460 J=1,119
      A(J)=LABL(1)
      IF((MOD(I,5) .EQ. 0) .AND. (YNL(JJ) .EQ. 0.0)) A(J)=LABL(2)
  460 CONTINUE
      IF(JDOT .NE. 0) A(JDOT)=LABL(2)
      IF(NPLOT .EQ. 2) GO TO 480
      DO 470 L=1,N
      IF(ABS(Y1(L)-(55.0-XI)) .GT. 1.0E-3) GO TO 470
      KK=27
      IF(NFOLD1 .GT. 1) KK=MOD(((L-1)/NFOLD1+1),26)
      LL=X(L)+.01
      IF((LL .GE. 1) .AND. (LL .LE. 119)) A(LL)=LCHAR1(KK)
  470 CONTINUE
      IF(NPLOT .NE. 3) GO TO 500
  480 DO 490 L=1,N
      IF(ABS(Y2(L)-(55.0-XI)) .GT. 1.0E-3) GO TO 490
      KK=27
      IF(NFOLD2 .GT. 1) KK=MOD(((L-1)/NFOLD2+1),26)
      LL=X(L)+.01
      IF((LL .GE. 1) .AND. (LL .LE. 119)) A(LL)=LCHAR2(KK)
  490 CONTINUE
  500 CONTINUE
C*JDL IF(I .EQ. 2) GO TO 530    !REMOVE "RADIANS" LABEL ON Y-AXIS.
      IF(MOD(I,5) .EQ. 0) GO TO 580
      IF(I .NE. 1) GO TO 550
      YEX=DLOG10(YEX)
      J=INT(YEX+SIGN(1D-1,YEX))
      PRINT 520, J,A
  520 FORMAT(1H ,2X,2H E,I3,1H+,119A1,1H+)
      GO TO 600
C*530 PRINT 540, A
C*540 FORMAT(1H ,8HRADIANS+,119A1,1H+)
C*JDL GO TO 600
  550 PRINT 560, A
  560 FORMAT(1H ,6X,2H +,119A1,1H+)
      GO TO 600
  580 CONTINUE
      PRINT 590, YNL(JJ),A
  590 FORMAT( 1H ,1X,F6.2,1HX,119A1,1HX)
  600 CONTINUE
C
C         (10). PRINT LOWER BORDER OF GRAPH AND X-AXIS LABEL.
C
      PRINT 450, YNL(1)
      PRINT 690, (XNL(L),L=1,13)
  690 FORMAT (1H ,13(4X,F6.2))
      XEX=DLOG10(XEX)
      J=INT(XEX+SIGN(1D-1,XEX))
  700 PRINT 710, XLABEL,J
  710 FORMAT(1H ,20X,16A4,41X,2H E,I3)
      RETURN
      END
C
C
      SUBROUTINE  PRNT( J,NO )                                          
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
c      REAL*4   DAET, TYME                                !JDL 31-OCT-84
      character *4 nwd
      REAL*8   HORSV(1000), VERSV(1000), DD(4)           !JDL  1-DEC-83
      include 'rtcomm0.f'
      COMMON  /BLCK 1/  XI, YI, ZI, VXI, VYI, VZI, DELP, DELM      !JDL 
      COMMON  /BLCK 2/  XO, YO, ZO, VXO, VYO, VZO, RTL(100),RLL(100)    
      COMMON  /BLCK 3/  XINT, YINT, ZINT, TH0, PH0, TL1  !JDL  1-DEC-83
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 6/  NR,  NP,   NSKIP, JFOCAL, JMTRX, !JDL 16-MAR-84 
     1                  JNR, NPLT, NRXS,  LPAX,          !JDL 31-OCT-84
     2                  NCAX,NHAX, NVAX, MEL, MCS, MCP,  !JDL 31-OCT-84
     3                  DHAX,DVAX                        !JDL 31-0CT-84
      COMMON  /BLCK15/  TMIN,PMIN,XMAX,TMAX,YMAX,PMAX,DMAX,        !JDL
     1                  STMN,SPMN,SXMX,STMX,SYMX,SPMX,SDMX,SUMX,   !JDL
     2                  SEED,SEEP,DXHW,DTHW,DYHW,DPHW,DEHW,DMHW,   !JDL
     3                  SEC1,SEC2,SEC3,SEC4,SEC5,SEC6,SEC7,SEC8    !JDL
      COMMON  /BLCK16/  NLOOP,NPASS,IP,NCSV,KEEP(20),    !JDL 17-NOV-83
     1                  LOOPSV(5,30),HOOPSV(30),HSAVE(30),PMSV(3),
     2                  CXXSV(12,3),CSV(36,3),CDSV(6,4,3)!JDL 17-NOV-83
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK62/  GASENE,GASVEL,TOLD,GASL
      COMMON  /BLCK61/  DEDX,ALPHAK,TOLD2,DEDXQ
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt
c      include 'rtcomm65.f'
C*JDL DIMENSION DATA(  75,30 ), ITITLE(30)               !JDL 17-NOV-83 
      DIMENSION VECK(10), JV(8), NBIN(26)                !JDL 31-OCT-84
      DIMENSION LABEL(48), NLNS(9)                       !JDL 31-OCT-84
      DATA LABEL/ 4HRAY ,4HPLOT,4H:   ,4H X(C,           !JDL 31-OCT-84
     1            4HM)  ,4H    ,4H    ,4H    ,
     2            4H^^  ,4H TH(,4HMR) ,4H VER,
     3            4HSES ,4H X(C,4HM)  ,4H  >>,
     4            4H X(C,4HM)  ,4H TH(,4HMR) ,
     5            4H Y(C,4HM)  ,4H PHI,4H(MR),
     6            4H DL(,4HCM) ,4H DT(,4HNS) ,
     7            4H DE(,4HPCT),4H DM(,4HPCT),
     8            4H ENE,4HRGY ,4H  MA,4HSS  ,
     9            4H COU,4HNTS ,4HALL-,4HCNTS,
     X            4HRAY ,4HPLOT,4HELLI,4HPSES,
     1            4HSPEC,4HTRUM,4HCONT,4HOURS/           !JDL 31-OCT-84
      DATA  NLNS/  1,  3,  5,  7,  9, 11, 15, 21, 25 /
      DIMENSION XO(100), YO(100), ZO(100), VXO(100), VYO(100), VZO(100) 
      DIMENSION XI(100), YI(100), ZI(100), VXI(100), VYI(100), VZI(100),
     1        DELP(100)                                                 
      character*8 LX(14)                                                     
      character*4 LCM                                                        
C****                              !Changes from here... !JDL 10-MAR-84
      INTEGER ID2(54), ID3(21), ID4(41), ID5(25), ID6(17),ID7(7),ID8(26)
      DATA ID2 / 11, 19, 29, 41, 51, 12, 20, 30, 42, 52, 13, 21, 31,    
     1   43, 53, 14, 22, 32, 44, 54, 15, 23, 33, 45, 55, 16, 24, 34,    
     2   46, 56, 17, 25, 35, 47, 57, 18, 26, 36, 48, 58,     27, 37,    
     3   49, 59,     28, 38, 50, 60,         39,     61,         40,
     4       62,                 63,                 64            /    
C****                                  !...down to here. !JDL 10-MAR-84
      DATA ID3 / 10, 15, 19, 25, 11, 16, 20, 26, 12, 17, 21, 27, 13,    
     1   18, 22, 28, 14, 23, 29, 24, 30                              /  
      DATA ID4 / 11, 20, 28, 34, 12, 21, 29, 35, 13, 22, 30, 36, 14,    
     1   23, 31, 37, 15, 24, 32, 38, 16, 25, 33, 39, 17, 26, 40, 46,    
     2   18, 27, 41, 47, 19, 42, 48, 43, 49, 44, 50, 45, 51          /  
      DATA ID5 / 10, 14, 19, 23, 29, 11, 15, 20, 24, 30, 12, 16, 21,    
     1   25, 31, 13, 17, 22, 26, 32, 18, 27, 33, 28, 34              /  
      DATA ID6 / 10, 16, 20, 26, 11, 17, 21, 27, 12, 22, 28, 13, 23,    
     1   14, 24, 15, 25                                              /  
      DATA ID7 / 10, 15, 11, 16, 12, 13, 14                          /  
      DATA ID8 / 11, 16, 25, 29, 35, 12, 17, 26, 30, 36, 13, 18, 27,
     1   31, 37, 14, 19, 28, 32, 38, 15, 20, 33, 39, 34, 40          /
      DATA LCM / ' CM ' /                                               
      DATA LX/ ' ENTR FL','D STEP =',' UNIF FL','D STEP =',             
     1         ' EXIT FL','D STEP =',' DIFF/MI','D STEP =',             
     2         '        ','   RHO =','        ','  MTYP =',             
     3         '   FIELD','  STEP ='                                 /  
C****                                                                   
C****                                                                   
      GO TO ( 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 ),J !JDL 17-NOV-83 
      PRINT 109, J                                                      
  109 FORMAT(// ' GO TO FELL THROUGH IN ROUTINE PRNT  J= ' I5  ///     )
      CALL EXIT                                                         
C****                                                                   
    1 RETURN                                                            
   13 RETURN                                                            
C****                                                                   
C**** DIPOLE DATA                                                       
C****                                                                   
  100 FORMAT( // 20X,  '***DIPOLE MAGNET    ***',   A4  /  )            
C****                              !Changes from here... !JDL 10-MAR-84
  101 FORMAT(                                                           
     1   5X,'  A =',  F9.4, 5X,'NDX =',  F9.4, 5X,'C01 =',  F9.4,       
     2   5X,'BR1 =',  F9.4, 5X,'S02 =',1PE12.3,5X,       2A8,0PF8.3,A4,/
     3   5X,'  B =',  F9.4, 5X,'BET1=',  F9.4, 5X,'C02 =',  F9.4,       
     4   5X,'BR2 =',  F9.4, 5X,'S03 =',1PE12.3,5X,       2A8,0PF8.3,A4,/
     5   5X,'  D =',  F9.4, 5X,'GAMA=',  F9.4, 5X,'C03 =',  F9.4,       
     6   5X,'XCR1=',  F9.4, 5X,'S04 =',1PE12.3,5X,       2A8,0PF8.3,A4,/
     7   5X,'  R =',  F9.4, 5X,'DELT=',  F9.4, 5X,'C04 =',  F9.4,       
     8   5X,'XCR2=',  F9.4, 5X,'S05 =',1PE12.3,5X,       2A8,0PF8.3,A4,/
     9   5X,' BF =',  F9.4, 5X,'XS1 =',  F9.4, 5X,'C05 =',  F9.4,       
     A   5X,'DLS1=',  F9.4, 5X,'S06 =',1PE12.3,5X,       2A8,  I4     ,/
     B   5X,'PHI =',0PF9.4, 5X,'XS2 =',  F9.4, 5X,'C06 =',  F9.4,       
     C   5X,'DLS2=',  F9.4, 5X,'S07 =',1PE12.3,5X,       2A8,0PF8.3,A4 )
  102 FORMAT(                                                           
     1   5X,'ALPH=',  F9.4,   5X,'Z11 =',  F9.4,  5X,'C11 =',  F9.4,    
     2   5X,'RAP1=',  F9.4,   5X,'S08 ='1PE12.3/, 5X,'BETA=',0PF9.4,    
     3   5X,'Z12 =',  F9.4,   5X,'C12 =',  F9.4,  5X,'RAP2=',  F9.4,    
     4   5X,'S12 =',1PE12.3/ 24X,'Z21 =',0PF9.4,  5X,'C13 =',0PF9.4,    
     X                        5X,'SCR1=',  F9.4,  5X,'S13 =',1PE12.3/,  
     5  24X,'Z22 =',0PF9.4,   5X,'C14 =',0PF9.4,  5X,'SCR2=',  F9.4,    
     Y                        5X,'S14 ='1PE12.3,/43X,'C15 =',0PF9.4,    
     6  24X,'S15 =',1PE12.3/ 43X,'C16 =' 0PF9.4, 24X,'S16 =',1PE12.3/,  
     7  81X,'S17 =',1PE12.3/ 81X,'S18 =' 1PE12.3                     )  
C****                                  !...down to here. !JDL 10-MAR-84
C****                                                                   
C****                                                                   
    2 RHO = 1.D30                                                       
      IF( DATA(15,NO) .NE. 0 )                                          
     1RHO = DSQRT( (2.*931.48*PMASS+ENERGY)*ENERGY)/(3.*DATA(15,NO)*Q0) 
      MTYP = DATA(5,NO)                                                 
      PRINT 100, ITITLE(NO)                                             
      PRINT 101,(DATA(ID2(I),NO),I= 1,5 ),LX( 1),LX( 2),DATA(1,NO),LCM, 
     1          (DATA(ID2(I),NO),I= 6,10),LX( 3),LX( 4),DATA(2,NO),LCM, 
     2          (DATA(ID2(I),NO),I=11,15),LX( 5),LX( 6),DATA(3,NO),LCM, 
     3          (DATA(ID2(I),NO),I=16,20),LX( 7),LX( 8),DATA(4,NO),LCM, 
     4          (DATA(ID2(I),NO),I=21,25),LX(11),LX(12),MTYP      ,     
     5          (DATA(ID2(I),NO),I=26,30),LX( 9),LX(10),RHO,       LCM  
      PRINT 102,(DATA(ID2(I),NO),I=31,54)                               
      RETURN                                                            
C****                                                                   
C**** QUADRUPOLE, HEXAPOLE, OCTAPOLE, DECAPOLE DATA                     
C****                                                                   
  200 FORMAT( // 20X,  '***QUADRUPOLE       ***',   A4  /  )            
  400 FORMAT( // 20X,  '***SEXTUPOLE        ***',   A4  /  )            
  500 FORMAT( // 20X,  '***OCTUPOLE         ***',   A4  /  )            
  600 FORMAT( // 20X,  '***DECAPOLE         ***',   A4  /  )            
C**** FORMAT at 120 made compatible with FORMAT at 101.  !JDL 11-MAR-84
C****       (F9.4, 8X, 2A8 to agree with E12.3, 5X, 2A8) !JDL 11-MAR-84
  120 FORMAT(                                                           
     1   5X,'  A =',  F9.4, 5X,'Z11 =',  F9.4, 5X,'C01 =',  F9.4,       
     2   5X,'C11 =',  F9.4, 8X, 2A8,0PF8.3,A4,/                         
     3   5X,'  B =',  F9.4, 5X,'Z12 =',  F9.4, 5X,'C02 =',  F9.4,       
     4   5X,'C12 =',  F9.4, 8X, 2A8,0PF8.3,A4,/                         
     5   5X,'  L =',  F9.4, 5X,'Z21 =',  F9.4, 5X,'C03 =',  F9.4,       
     6   5X,'C13 =',  F9.4, 8X, 2A8,0PF8.3,A4,/                         
     7   5X,'RAD =',  F9.4, 5X,'Z22 =',  F9.4, 5X,'C04 =',  F9.4,       
     8   5X,'C14 =',  F9.4,/5X,' BF =',  F9.4,24X,'C05 =',  F9.4,       
     9   5X,'C15 =',  F9.4,/                  43X,'C06 =',  F9.4,       
     A   5X,'C16 =',  F9.4                                           )  
C****                                                                   
C****                                                                   
    3 PRINT 200, ITITLE(NO)                                             
      GO TO 21                                                          
    4 PRINT 400, ITITLE(NO)                                             
      GO TO 21                                                          
    5 PRINT 500, ITITLE(NO)                                             
      GO TO 21                                                          
    6 PRINT 600, ITITLE(NO)                                             
   21 PRINT 120,(DATA(ID3(I),NO),I= 1,4 ),LX( 1),LX( 2),DATA(1,NO),LCM, 
     1          (DATA(ID3(I),NO),I= 5,8 ),LX( 3),LX( 4),DATA(2,NO),LCM, 
     2          (DATA(ID3(I),NO),I= 9,12),LX( 5),LX( 6),DATA(3,NO),LCM, 
     3          (DATA(ID3(I),NO),I=13,21)                               
      RETURN                                                            
C****                                                                   
C**** ELECTROSTATIC DEFLECTOR DATA                                      
C****                                                                   
  190 FORMAT( // 20X,  '***ELECTROSTATIC DEF***',   A4  /  )       !JDL 
C**** FORMAT at 191 made compatible with FORMAT at 101.  !JDL 11-MAR-84
  191 FORMAT(                                                           
     1   5X,'  A =',  F9.4, 5X,'PHI =',  F9.4, 5X,'Z11 =',  F9.4,       
     2   5X,'C01 =',  F9.4, 5X,'C11 =',  F9.4, 8X,       2A8,0PF8.3,A4,/
     3   5X,'  B =',  F9.4, 5X,'EC2 =',  F9.4, 5X,'Z12 =',  F9.4,       
     4   5X,'C02 =',  F9.4, 5X,'C12 =',  F9.4, 8X,       2A8,0PF8.3,A4,/
     5   5X,'  D =',  F9.4, 5X,'EC4 =',  F9.4, 5X,'Z21 =',  F9.4,       
     6   5X,'C03 =',  F9.4, 5X,'C13 =',  F9.4, 8X,       2A8,0PF8.3,A4,/
     7   5X,'  R =',  F9.4, 5X,'WE  =',  F9.4, 5X,'Z22 =',  F9.4,       
     8   5X,'C04 =',  F9.4, 5X,'C14 =',  F9.4, 8X,       2A8,0PF8.3,A4,/
     9   5X,' EF =',  F9.4, 5X,'WC  =',  F9.4,24X,'C05 =',  F9.4,       
     A   5X,'C15 =',  F9.4,                    8X,       2A8,0PF8.3,A4,/
     B  62X,'C06 =',0PF9.4, 5X,'C16 =',  F9.4                   )       
C****                                                                   
C****                                                                   
    7 RHO = 1.D30                                                       
        EMASS = PMASS * 931.48
        ETOT = EMASS + ENERGY
        VC2 = (2.*EMASS + ENERGY)*ENERGY / (ETOT*ETOT)
        GAMMA = 1. / DSQRT(1. - VC2)
      IF( DATA(15,NO) .NE. 0 )                                          
     1RHO = GAMMA * EMASS * VC2 * 1000. / (DATA(15,NO) * Q0)            
      PRINT 190, ITITLE(NO)                                             
      PRINT 191,(DATA(ID8(I),NO),I= 1,5 ),LX( 1),LX( 2),DATA(1,NO),LCM, 
     1          (DATA(ID8(I),NO),I= 6,10),LX( 3),LX( 4),DATA(2,NO),LCM, 
     2          (DATA(ID8(I),NO),I=11,15),LX( 5),LX( 6),DATA(3,NO),LCM, 
     3          (DATA(ID8(I),NO),I=16,20),LX( 7),LX( 8),DATA(4,NO),LCM, 
     4          (DATA(ID8(I),NO),I=21,24),LX( 9),LX(10),RHO,LCM   ,     
     5          (DATA(ID8(I),NO),I=25,26)                               
      RETURN                                                            
C****                                                                   
C****    VELOCITY SELECTOR DATA                                         
C****                                                                   
  132 FORMAT( // 20X,  '***VELOCITY SELECTOR***',   A4  /  )            
C**** FORMAT at 130 made compatible with FORMAT at 101.  !JDL 11-MAR-84
  130 FORMAT(                                                           
     1   5X,'  A =',  F9.4, 5X,'Z11 =',  F9.4, 5X,'CB00=',  F9.4,       
     2   5X,'CE00=',  F9.4, 8X, 2A8,0PF8.3,A4,/                         
     3   5X,'  B =',  F9.4, 5X,'Z12 =',  F9.4, 5X,'CB01=',  F9.4,       
     4   5X,'CE01=',  F9.4, 8X, 2A8,0PF8.3,A4,/                         
     5   5X,'  L =',  F9.4, 5X,'Z21 =',  F9.4, 5X,'CB02=',  F9.4,       
     6   5X,'CE02=',  F9.4, 8X, 2A8,0PF8.3,A4,/                         
     7   5X,' BF =',  F9.4, 5X,'Z22 =',  F9.4, 5X,'CB03=',  F9.4,       
     8   5X,'CE03=',  F9.4, 8X, 2A8,0PF8.3,A4,/                         
     9   5X,' BE =',  F9.4, 5X,'CB2 =',  F9.4, 5X,'CB04=',  F9.4,       
     A   5X,'CE04=',  F9.4, 8X, 2A8,0PF8.3,A4                        )  
  131 FORMAT(                                                           
     1   5X,' DB =',  F9.4, 5X,'CB4 =',  F9.4, 5X,'CB05=',  F9.4,       
     2   5X,'CE05=',  F9.4,/5X,' DE =',  F9.4, 5X,'CE2 =',  F9.4,       
     3   5X,'CB10=',  F9.4, 5X,'CE10=',  F9.4,/5X,' WB =',  F9.4,       
     4   5X,'CE4 =',  F9.4, 5X,'CB11=',  F9.4, 5X,'CE11=',  F9.4,/      
     5   5X,' WE =',  F9.4,24X,'CB12=',  F9.4, 5X,'CE12=',  F9.4,/      
     6  43X,'CB13=',  F9.4, 5X,'CE13=',  F9.4,/                         
     7  43X,'CB14=',  F9.4, 5X,'CE14=',  F9.4,/                         
     8  43X,'CB15=',  F9.4, 5X,'CE15=',  F9.4                        )  
C****                                                                   
C****                                                                   
    8 RHO = 1.D30                                                       
      IF( DATA(14,NO)  .NE.  0.   )                                     
     1RHO = DSQRT( (2.*931.48*PMASS+ENERGY)*ENERGY)/(3.*DATA(14,NO)*Q0) 
      PRINT 132,ITITLE(NO)                                              
      PRINT 130,(DATA(ID4(I),NO),I= 1,4 ),LX( 1),LX( 2),DATA(1,NO),LCM, 
     1          (DATA(ID4(I),NO),I= 5,8 ),LX( 3),LX( 4),DATA(2,NO),LCM, 
     2          (DATA(ID4(I),NO),I= 9,12),LX( 5),LX( 6),DATA(3,NO),LCM, 
     3          (DATA(ID4(I),NO),I=13,16),LX( 7),LX( 8),DATA(4,NO),LCM, 
     4          (DATA(ID4(I),NO),I=17,20),LX( 9),LX(10),RHO,LCM         
      PRINT 131,(DATA(ID4(I),NO),I=21,41)                               
      RETURN                                                            
C****                                                                   
C**** MULTIPOLE (POLES)      DATA                                       
C****                                                                   
  141 FORMAT( // 20X,  '***MULTIPOLES       ***',   A4  /  )            
C**** FORMAT at 140 was  compatible with FORMAT at 101.  !JDL 11-MAR-84
  140 FORMAT(                                                           
     1   5X,'  A =',  F9.4, 3X,'BQUAD =',F9.4, 5X,'Z11 =',  F9.4,       
     2   5X,'C01 =',  F9.4, 5X,'C11 =',  F9.4, 8X, 2A8,0PF8.3,A4,/      
     3   5X,'  B =',  F9.4, 3X,'BHEX  =',F9.4, 5X,'Z12 =',  F9.4,       
     4   5X,'C02 =',  F9.4, 5X,'C12 =',  F9.4, 8X, 2A8,0PF8.3,A4,/      
     5   5X,'  L =',  F9.4, 3X,'BOCT  =',F9.4, 5X,'Z21 =',  F9.4,       
     6   5X,'C03 =',  F9.4, 5X,'C13 =',  F9.4, 8X, 2A8,0PF8.3,A4,/      
     7   5X,'RAD =',  F9.4, 3X,'BDEC  =',F9.4, 5X,'Z22 =',  F9.4,       
     8   5X,'C04 =',  F9.4, 5X,'C14 =',  F9.4,/                         
     9                     22X,'BDDEC =',F9.4,24X,'C05 =',  F9.4,       
     A   5X,'C15 =',  F9.4/62X,'C06 =',  F9.4, 5X,'C16 =',  F9.4     )  
C****                                                                   
C****                                                                   
    9 PRINT 141, ITITLE(NO)                                             
      PRINT 140,(DATA(ID5(I),NO),I= 1,5 ),LX( 1),LX( 2),DATA(1,NO),LCM, 
     1          (DATA(ID5(I),NO),I= 6,10),LX( 3),LX( 4),DATA(2,NO),LCM, 
     2          (DATA(ID5(I),NO),I=11,15),LX( 5),LX( 6),DATA(3,NO),LCM, 
     3          (DATA(ID5(I),NO),I=16,25)                               
      RETURN                                                            
C****                                                                   
C**** MULTIPOLE DATA                                                    
C****                                                                   
  151 FORMAT( // 20X,  '***MULTIPOLE(HE)    ***',   A4  /  )            
C**** FORMAT at 150 made compatible with FORMAT at 101.  !JDL 11-MAR-84
  150 FORMAT(                                                           
     1   5X,'  A =',  F9.4, 5X,' Z1 =',  F9.4, 5X,' C0 =',  F9.4,       
     2   5X,' C6 =',  F9.4, 8X, 2A8,0PF8.3,A4,/                         
     3   5X,'  B =',  F9.4, 5X,' Z2 =',  F9.4, 5X,' C1 =',  F9.4,       
     4   5X,' C7 =',  F9.4, 8X, 2A8,0PF8.3,A4,/                         
     5   5X,'  L =',  F9.4,24X,' C2 =',  F9.4, 5X,' C8 =',  F9.4/       
     6   5X,'  W =',  F9.4,24X,' C3 =',  F9.4,/                         
     7   5X,'  D =',  F9.4,24X,' C4 =',  F9.4,/                         
     8   5X,' BF =',  F9.4,24X,' C5 =',  F9.4                        )  
C****                                                                   
C****                                                                   
   10 PRINT 151, ITITLE(NO)                                             
      PRINT 150,(DATA(ID6(I),NO),I= 1,4 ),LX( 1),LX( 2),DATA(1,NO),LCM, 
     1          (DATA(ID6(I),NO),I= 5,8 ),LX( 7),LX( 8),DATA(2,NO),LCM, 
     2          (DATA(ID6(I),NO),I= 9,17)                               
      RETURN                                                            
C****                                                                   
C**** TRANSLATE - ROTATE DATA                                           
C****                                                                   
  170 FORMAT( // 20X,  '***TRANSLATE-ROTATE ***',   A4  /  )            
  171 FORMAT(   5X, 5H X0 = F9.4,        5X,5H Y0 = F9.4,               
     1          5X, 5H Z0 = F9.4,     / 1X,9HTHETA X = F9.4,            
     2        1X,9HTHETA Y =F9.4,       1X,9HTHETA Z = F9.4            )
C****                                                                   
C****                                                                   
   11 PRINT 170, ITITLE(NO)                                             
      PRINT 171, ( DATA(I,NO) , I=1,6 )                                 
      RETURN                                                            
C****                                                                   
C**** DRIFT SECTION DATA                                                
C****                                                                   
   12 PRINT 175, ITITLE(NO)                                             
      PRINT 176, ( DATA(I,NO) , I=1,1 )                                 
  175 FORMAT( // 20X,  '***DRIFT            ***',   A4  /  )            
  176 FORMAT(  19X,  ' Z-DRIFT ='    F9.4,  ' CM'        )              
      RETURN                                                            
C****                                                                   
C**** SOLENOID DATA                                                     
C****                                                                   
  161 FORMAT( // 20X,  '***SOLENOID         ***',   A4  /  )            
C**** FORMAT at 160 made compatible with FORMAT at 101.  !JDL 11-MAR-84
  160 FORMAT(                                                           
     1   5X,'  A =',  F9.4, 5X,'Z11 =',  F9.4, 8X,2A8,0PF8.3,A4,/       
     2   5X,'  B =',  F9.4, 5X,'Z22 =',  F9.4,/5X,'  L =',  F9.4,/      
     3   5X,'DIA =',  F9.4,/5X,' BF =',  F9.4                        )  
C****                                                                   
C****                                                                   
   14 PRINT 161, ITITLE(NO)                                             
      PRINT 160,(DATA(ID7(I),NO),I= 1,2 ),LX(13),LX(14),DATA(1,NO),LCM, 
     1          (DATA(ID7(I),NO),I= 3, 7)                               
      RETURN                                                            
C****                                                                   
C**** LENS DATA                                                         
C****                                                                   
  180 FORMAT( // 20X,  '***LENS             ***',   A4  /  )            
  181 FORMAT(  3X, 7H(X/X) =  ,F9.4,   6H CM/CM                         
     1        16X, 7H(X/T) =  ,F9.4,   6H CM/MR  /                      
     2         3X, 7H(T/X) =  ,F9.4,   6H MR/CM                         
     3        16X, 7H(T/T) =  ,F9.4,   6H MR/MR  /                      
     4         3X, 7H(Y/Y) =  ,F9.4,   6H CM/CM                         
     5        16X, 7H(Y/P) =  ,F9.4,   6H CM/MR  /                      
     6         3X, 7H(P/Y) =  ,F9.4,   6H MR/CM                         
     7        16X, 7H(P/P) =  ,F9.4,   6H MR/MR  /         )            
C****                                                                   
C****                                                                   
   15 PRINT 180, ITITLE(NO)                                             
      PRINT 181, ( DATA(I,NO) , I=1,8 )                                 
      RETURN                                                            
C****
C**** Changes from here ...                              !JDL 17-NOV-83
C****
C**** CHANGE DATA
C****
  183 FORMAT( // 20X,  '***CHANGE AND REPEAT***',   A4  /  )
  184 FORMAT( 5X,A4,' (',A4,')  [LINE',I2,', ENTRY',I2,
     1       ', GROUP (',A4,')]   STEP =',F13.6 )
   16 PRINT 183, ITITLE(NO)
      DO 185 J=1,NLOOP
      INO=LOOPSV(1,J)
      NWD=NWORD(IDATA(INO))
      PRINT 184, NWD, ITITLE(INO), (LOOPSV(I,J),I=2,4), HOOPSV(J)
  185 CONTINUE
      RETURN
C****
C**** AUTOMATIC RAY GENERATION
C****
      ENTRY PRNTA
      IF( JNR .EQ. 0 ) RETURN
  196 FORMAT( //, 20X, '***AUTORAY GENERATOR***', //,
     1    9X, 4X, 'TMIN', 6X, 'PMIN', 9X, 'XMAX', 6X, 'TMAX',
     2        6X, 'YMAX', 6X, 'PMAX', 9X, 'DMAX', 6X, 'UMAX',  / )
  197 FORMAT( 1X, 'JNR=',  I4, 2F10.4, 3X, 4F10.4, 3X, 2F10.4 )
  198 FORMAT( 1X, 'NRXS=', I3, 2F10.4, 3X, 4F10.4, 3X, 2F10.4,
     1        1X, 8X, 'STARTING SEED = ',   F11.0 )
  199 FORMAT( 1X, 8X, F11.0,   9X,     3X, 4F10.4, 3X, 2F10.4, /,
     1        1X, 8X,          2F10.4, 3X, 4F10.4, 3X, 2F10.4 )
      PRINT 196
      PRINT 197, JNR,  TMIN, PMIN, XMAX, TMAX, YMAX, PMAX, DMAX
      IF( NRXS .EQ. 0 ) RETURN
      PRINT 198, NRXS, STMN, SPMN, SXMX, STMX, SYMX, SPMX, SDMX, SUMX,
     1           SEEP
      IF( NRXS .NE. 13 ) RETURN
      PRINT 199, SEED,       DXHW, DTHW, DYHW, DPHW, DEHW, DMHW,
     1           SEC1, SEC2, SEC3, SEC4, SEC5, SEC6, SEC7, SEC8
      RETURN
C****
C**** ...down to here.                                   !JDL 13-OCT-84
C****                                                                   
C****                                                                   
      ENTRY PRNT1 ( N1, NN, JEN, NEN, WIDTH )            !JDL  1-NOV-84 
C****                                                                   
C****                                                                   
      IF( IP .GT. 500 ) GO TO 1190                       !JDL  4-NOV-84
      IF((MCS .EQ. 0) .AND. (MCP .EQ. 0)) PRINT 110      !JDL  4 NOV-84 
      IF( MCS .NE. 0 )  PRINT 1110                       !JDL  4-NOV-84
      IF( MCP .NE. 0 )  PRINT 1120                       !JDL  4-NOV-84
  110 FORMAT( 1H1, 15X, '****COORDINATES OPTIC AXIS SYSTEM****' //      
     1   10X, 45HX     THETA     Y      PHI      ZI    DELE     ,5X,    
     2   12HXO        XS  , 11X, 12HYO        YS , 6X, 'L(CM)', 5X,     
     3   'T(NS)'                         /)                             
 1110 FORMAT( 1H1, 15X, '****AUTORAY INPUT COORDINATES****', 12X,  !JDL
     1     '****MULTI-CHANNEL SPECTRA FROM RANDOM RAYS****',  //,
     2   10X, 45HX     THETA     Y      PHI    XSCTR   DELE     ,
     3   'X(CM)/T(NS)', 2X, 'X-COUNTS', 6X, 'Y(CM)', 3X, 'Y-COUNTS',
     4   4X,   'ALL-X', 4X, 'T-COUNTS', / )
 1120 FORMAT( 1H1, 15X, '****CONTOUR INPUT COORDINATES****', 12X,  !JDL
     1     '****CENTRAL CONTOUR AND GRID VALUES****',         //,
     2   10X, 45HX     THETA     Y      PHI    XSCTR   DELE     ,
     3    4X, 'HORZ-CENTER-VERT',       6X, 'QUAD1', 5X, 'QUAD2',
     4    6X, 'QUAD3', 5X, 'QUAD4', / )
C****                                      !From here... !JDL 31-OCT-84
 1190 CONTINUE
      N = NN
      NRSV = N
      IF( JEN .EQ. 1 ) XMAXSV = XO(1)
      IF( JEN .EQ. 1 ) XMINSV = XMAXSV
      KK = N*(JEN-1)
      NRMAX = N + KK
      IF( MCP .EQ. 0 ) GO TO 890
C****
C**** CONTOUR MAP INTERPOLATED FROM RECTANGULAR GRID.
C****
      IF( JEN .GT. 1 ) GO TO 804
      DO 802 II = 1, 26
  802 NBIN(I) = 0    !CLEAR BIN COUNTERS
      NLINES = NLNS( NRXS - 20 )
      LIMIT = NLINES/2
      NBMAX = ( 1000 - JNR - 1 ) / NLINES
      IF( NBMAX .GT. 100*NEN ) NBMAX = 100*NEN
      MCP1  = MCP + 1
  804 JV(5) = 3         !CORNERS OF MESH (SECOND NODE OF PAIR)
      JV(6) = 2
      JV(7) = 3
      JV(8) = 4
      LCT   = 0
C****
      DO 850 NM = 1, 4        !NM = MESH INDEX (DIAG, HOR, VER, DIAG)
      DO 840 NJ = 2, MCP1     !NJ = VERT INDEX (NCVER)
      DO 830 NI = 2, MCP1     !NI = HORZ INDEX (NCHOR)
      DO 820 NK = 1, 4        !NK = QUAD INDEX (NCQAD)
      DHAX = DABS( DHAX )
      DVAX = DABS( DVAX )
      IF(( NK .EQ. 2 ) .OR. ( NK .EQ. 3 )) DHAX = -DHAX
      IF(( NK .EQ. 3 ) .OR. ( NK .EQ. 4 )) DVAX = -DVAX
C**** LOCATE FOUR CORNERS ON RECTANGULAR MESH(I,J) IN QUADRANT(K).
      JV(1) = (NJ-1)*MCP1 + NI + JNR + 1    !REFERENCE NODE
      JV(2) = JV(1) - 1                     !BACK 1 IN HORZ
      JV(3) = JV(1) - MCP1                  !BACK 1 IN VERT
      JV(4) = JV(1) - MCP1 - 1              !BACK 1 IN EACH
      DO 806 II = 1, 4
      JJ = JV(II)
      JV(II) = 1        !CORNERS OF MESH (FIRST NODE)
      VECK(5) = YO(JJ)
      VECK(6) = VYO(JJ)
      VECK(7) = RLL(JJ)
      VECK(8) = RTL(JJ)
  806 VECK(II) = VECK(NK+4)    !GET 4 NODES FROM ONE QUADRANT.
      JV(1) = 2         !CORNER FOR DOWN-DIAGONAL
C****
C**** FIND INTERSECTIONS OF CONTOURS WITH FOUR LINE SEGMENTS
C**** BETWEEN NODES (TWO DIAGONALS, OUTER ROW, OUTER COLUMN).
C**** USE AVERAGE BETWEEN NODE VALUES AS A TEMPORARY COORDINATE
C**** REFERENCE.  NOTE THAT NODE VALUES ALREADY ARE NORMALIZED
C**** TO CONTOUR SEPARATION AND CENTERING.
C****
      II = JV(NM)    !SELECT A PAIR OF NODES (FIRST AND SECOND).
      JJ = JV(NM + 4)
      IF( VECK(II) .EQ. VECK(JJ) ) GO TO 820
        AVG = 0.5*(VECK(II) + VECK(JJ))
       NAVG = IDNINT(AVG)
      ANAVG = DFLOAT(NAVG)      !NEXT, TEST FOR CONTOURS BETWEEN NODES.
      IF(DABS(ANAVG - AVG) .GT. DABS(VECK(II) - AVG)) GO TO 820  !NONE
      L1 = IDINT(VECK(II) - ANAVG) + NAVG    !L1 THRU L2 INCLUDES ALL
      L2 = IDINT(VECK(JJ) - ANAVG) + NAVG    !CONTOURS IN BETWEEN NODES
      IF((L1 .GT. +LIMIT) .AND. (L2 .GT. +LIMIT)) GO TO 820  !ALL HIGH
      IF((L1 .LT. -LIMIT) .AND. (L2 .LT. -LIMIT)) GO TO 820  !ALL LOW
      IF( L1 .GT. +LIMIT) L1 = +LIMIT    !RESTRICT CONTOURS TO LIMITS
      IF( L2 .GT. +LIMIT) L2 = +LIMIT
      IF( L1 .LT. -LIMIT) L1 = -LIMIT
      IF( L2 .LT. -LIMIT) L2 = -LIMIT
      NL = L1
      IF( L2 .LT. NL ) L1 = L2    !POSITIVE-GOING LOOP INDEX
      IF( L2 .LT. NL ) L2 = NL
      DO 810 NL = L1, L2          !SCAN CONTOURS L1 THRU L2
      DV = ( DFLOAT(NL) - VECK(II) ) / ( VECK(JJ) - VECK(II) )
      DH = DV
      IF( NM .EQ. 1 ) DH = 1.0 - DV    !FALLING DIAGONAL
      IF( NM .EQ. 2 ) DV = 0.0         !HORIZIOTAL LINE
      IF( NM .EQ. 3 ) DH = 0.0         !VERTICAL   LINE
C**** IF( NM .EQ. 4 ) DH = DV          !RISING  DIAGONAL
      IT = NL + LIMIT + 1
      IF( NBIN(IT) .GE. NBMAX ) GO TO 810       !BIN IS FULL
      NBIN(IT) = NBIN(IT) + 1
      JT = (IT-1)*NBMAX + NBIN(IT)
      HORSV(JT) = ( DFLOAT(NI-1) - DH ) * DHAX    !STORE INTERCEPT
      VERSV(JT) = ( DFLOAT(NJ-1) - DV ) * DVAX    !COORDINATES
      IF( NL .NE. 0 ) GO TO 810
      LCT = LCT + 1
      IF( LCT .GT. 100 ) GO TO 810
      XO(LCT)  = HORSV(JT)     !COORDINATES OF CENTRAL CONTOUR
      VXO(LCT) = VERSV(JT)     !SENT TO PRINT LIST.
  810 CONTINUE      !END NL -- CONTOURS LOOP
  820 CONTINUE      !END NK -- QUADRANT LOOP
  830 CONTINUE      !END NI -- HORIZONTAL LOOP
  840 CONTINUE      !END NJ -- VERTICAL LOOP
  850 CONTINUE      !END NM -- MESH LOOP
      N = MIN( MAX( MCP1*MCP1, LCT ), 100 - JNR - 1 )
      DO 854 II = 1, N
      JJ = II + JNR + 1
      YO(II)  = YO(JJ)
      VYO(II) = VYO(JJ)
      RLL(II) = RLL(JJ)
  854 RTL(II) = RTL(JJ)
      IF( JEN .LT. NEN ) GO TO 890
C**** FILL BINS TO TOP WITH REDUNDANT COORDINATES.
      IT = 0
      NRMAX = NLINES*NBMAX
      DO 870 II = 1, NLINES    !SCAN BIN COUNTERS
      JJ = NLINES - II + 1
      IF(( NBIN(JJ) .NE. 0 ) .OR. ( IT .NE. 0 )) GO TO 864
      NRMAX = NRMAX - NBMAX    !DISCARD EMPTY OUTER BINS
      GO TO 870
  864 IT = 1
      NL = (JJ-1)*NBMAX + NBIN(JJ)
      IF( NBIN(JJ) .EQ. 0 ) GO TO 866    !INTERIOR BIN IS EMPTY
      DH = HORSV(NL)    !PICK UP SOME VALID VALUES
      DV = VERSV(NL)
  866 L1 = NL + 1
      L2 = NL + NBMAX - NBIN(JJ)
      IF( L1 .GT. L2 ) GO TO 870
      DO 868 NL = L1, L2
      HORSV(NL) = DH    !FILL UP EMPTY LOCATIONS (FOR CLEAN PLOT)
  868 VERSV(NL) = DV
  870 CONTINUE
C**** GO TO 890
  890 DO 20  I=1,N                                                      
      X1 = XO(I)
      IF( X1 .GT. XMAXSV ) XMAXSV = X1
      IF( X1 .LT. XMINSV ) XMINSV = X1
      IF( MCP .NE. 0 ) GO TO 22
      II = N - I +1
      JJ = II + KK
      IF( II .LT. N1 ) GO TO 23     !FILL FROM I=1 TO N1-1 WITH RAY NL.
      VECK(1)  = XO(II)
      VECK(2)  = VXO(II)
      VECK(3)  = YO(II)
      VECK(4)  = VYO(II)
      VECK(5)  = RLL(II)
      VECK(6)  = RTL(II)
      VECK(7)  = DELP(II)
      VECK(8)  = DELM
      VECK(9)  = ENERGY
      VECK(10) = MASS
      IF((MCS .EQ. 0) .AND. (MCP .EQ. 0)) VECK(6) = RTL(II)*1.0D+09/VEL
   23 HORSV(JJ) = VECK(NHAX)
      VERSV(JJ) = VECK(NVAX)
   22 IF( IP .GT. 500 ) GO TO 20            !...to here. !JDL 31-OCT-84
C****
C**** CALCULATE TIME IN (NS)
C****
      TLJ1 = RTL(I)                                      !JDL  4-NOV-84
      IF((MCS .EQ. 0) .AND. (MCP .EQ. 0)) TLJ1 = RTL(I)*1.0D+09/VEL!JDL
      PRINT 111,  I, XI(I), VXI(I), YI(I), VYI(I), ZI(I), DELP(I),      
     1    XO(I), VXO(I), YO(I), VYO(I), RLL(I), TLJ1                    
  111 FORMAT(       I5,    6F8.2, 2X,  F10.4,   F10.4, 2X,   F10.4,     
     1     F10.4 ,   F10.3, F10.3               /)                      
   20 CONTINUE                                                          
      FNPLT = N
      WRITE (2,2220) FNPLT
 2220 FORMAT(///1F8.0)
      DXPLT = XO(2) - XO(1)
      WRITE(2,2222) XO(1),DXPLT
 2222 FORMAT(10F8.0)
      WRITE(2,2222) (RLL(I),I=1,N)
      WIDTH = DABS( XMAXSV - XMINSV )                    !JDL  1-DEC-83
      RETURN                                                            
C****                                      !From here... !JDL  1-DEC-83
C****
      ENTRY PRNT1A ( NEN )
C****
      IF(MOD(IP/10,10) .EQ. 5) RETURN    !TEMPORARY COMPATIBILITY
      IF(( NCAX .EQ. NHAX) .AND. ( NHAX .EQ. NVAX )) RETURN
      DD(1)=0.0
      DD(2)=0.0
      DD(3)=0.0
      DD(4)=0.0
      IF( MCP .EQ. 0 ) GO TO 898
      DD(1) = DHAX * DFLOAT( MCP )    !CORNERS OF GRID
      DD(2) = DVAX * DFLOAT( MCP )
      DD(3) = -DD(1)
      DD(4) = -DD(2)
  898 CONTINUE
      KK = 2*(( NRXS + 9 ) / 10 ) + 40
      LABEL(1)  = LABEL(KK+1)
      LABEL(2)  = LABEL(KK+2)
      LABEL(4)  = LABEL(8)     !BLANK
      LABEL(5)  = LABEL(8)
      IF( NCAX .NE. 0 ) LABEL(4)  = LABEL(2*NCAX+15)
      IF( NCAX .NE. 0 ) LABEL(5)  = LABEL(2*NCAX+16)
      LABEL(10) = LABEL(2*NVAX+15)
      LABEL(11) = LABEL(2*NVAX+16)
      LABEL(14) = LABEL(2*NHAX+15)
      LABEL(15) = LABEL(2*NHAX+16)
C      J=2
      NFOLD=NRSV
      IF( NEN .EQ. 1 ) NFOLD = 1
      IF( MCP .NE. 0 ) NFOLD = NBMAX
      PRINT 188, NTITLE, DAET, TYME
      CALL PPLOT( HORSV,VERSV,VERSV, NFOLD,NFOLD, NRMAX, LABEL, 2, DD )
  188 FORMAT( 1H1, 10X, 20A4, 1X, 12H JDL/84.11  ,3X, 3A4, 2X, 2A4 )
      RETURN                                !...to here. !JDL  1-DEC-83
C****                                                                   
C****                                                                   
      ENTRY PRNT2 ( T, S, X, Y, Z, BX, BY, BZ, BT, VX, VY, VZ          )
C****                                                                   
      IF( NP  .GT. 100 ) RETURN                                         
      VXP = 1000. *DATAN2( VX ,VZ  )                                    
      VYP = 1000. * DASIN( VY /VEL )                                    
      VZP = VZ  / VEL                                                   
      TP = T * VEL                                                      
      PRINT 112,TP,S,X, BX, Y, BY, Z, BZ, VZP, VXP, VYP, BT             
  112 FORMAT(2F10.4,     F10.3, F11.4, F10.3, F11.4, F10.3, F11.4,      
     1        F13.5, F13.2, F11.2, F10.4         )                      
      RETURN                                                            
C****
C****
      ENTRY PRNT2A
      IF (NP.GE.500) RETURN
      PRINT 1121, Q0,GASENE,QBAR,DELSQR,GASMFP,SIGC,SIGT,ACAPT,ALOS
     1           , DEDXQ
 1121 FORMAT(9X,F10.4,F10.3,F11.4,F10.3,4X,F10.4,F11.5,F11.5,F10.4,5X,
     1      F10.4,F10.3)
      RETURN
C****
C****                                                                   
      ENTRY PRNT3 (TDIST,X,Y,Z,BX,BY,BZ,EX,EY,EZ,VX,VY,VZ)              
C****                                                                   
  114 FORMAT( 2F9.3, 2F10.4,F9.3, 2F10.4,F9.3, 2F10.4,2F11.3, -9PF9.5 ) 
C****                                                                   
C****                                                                   
      IF( NP  .GT. 100 ) RETURN                                         
      VXP = 1000. *DATAN2( VX ,VZ  )                                    
      VYP = 1000. * DASIN( VY /VEL )                                    
      VZP = VZ  / VEL                                                   
      TP = T * VEL                                                      
      PRINT 114, TDIST,X,BX,EX,Y,BY,EY,Z,BZ,EZ,VXP,VYP,VEL              
      RETURN                                                            
C****                                                                   
C****                                                                   
C****                                                                   
        ENTRY  PRNT4(NO, IN)                                            
C****                                                                   
115     FORMAT (///, 10X, 'MAXIMUM STEPS EXCEEDED', /10X,               
     1   'ELEMENT = ', I4, /10X, 'REGION = ', I4 ///)                   
C        PRINT 115, NO, IN                                               
        RETURN                                                          
C****                                                                   
C****                                                                   
      ENTRY PRNT5 ( T, S, X, Y, Z, EX, EY, EZ, ET, VX, VY, VZ          )
C****                                                                   
      IF( NP  .GT. 100 ) RETURN                                         
      VXP = 1000. *DATAN2( VX ,VZ  )                                    
      VYP = 1000. * DASIN( VY /VEL )                                    
      VZP = VZ  / VEL                                                   
      TP = T * VEL                                                      
      PRINT 112,TP,S,X, EX, Y, EY, Z, EZ, VZP, VXP, VYP, ET             
      RETURN                                                            
      END                                                               
      SUBROUTINE QCG(Q0,Q1)
      IMPLICIT REAL*8 (A-H,O-Z)
c
      common  /blck60/  gas,agas,zgas,zion,press,gassig,qaver,  !***mp 1
     1                  qfwhm,rhogas,gask,emass,gasmfp,jray,q00,npasg   
      COMMON  /BLCK63/  ISEED
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt
c      include 'rtcomm65.f'
C*****
C*****
C*****
      P1 = SIGC/SIGT
      R = RAN(0)
C
      Q1 = Q0 - 1.
      IF (R.GT.P1) Q1 = Q0 + 1.
c
C****
C****THERE IS A BUG HERE : MINIMUM CHARGE STATE IS SET TO 0  !MP AUG-2-9
C****SHOULD BE MODIFIED TOFORGET ABOUT IONS WHICH GET NEUTRAL!MP AUG-2-9
C****
      if (q1.le.0.)  q1=1.			! no negative ions
      if (q1.gt.zion)  q1=zion		! no 'positronic' ions
      RETURN
      END
      SUBROUTINE QDIST
c
c     calculate charge distribution in gas
c
c 911204 MP/BS	add new input parameter 'qopt' to select different 
c               formulas for calculating qbar in gas
c               qopt = 0. Dimitriev
c               qopt = 1. Betz
c               qopt = 2. Rehm (NIM 95)
c               qopt = 3. ref. Reiner Kruecken (Yale), V. Ninov (Berkele
c                         expression for low velocities
c               qopt = 4. Schiwietz (NIM B 175-177 (2001) 125)
c
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt
c      include 'rtcomm65.f'
c
      DATA V0/2.189D8/
      DATA A/0.555/,DEL/1.175/,GAM/0.607/
      DATA Ar/1.571/,DELr/0.6/,GAMr/0.61/
c
      if (qopt .eq. 0.) then
c
c     Dimitriev and Nikolaev (1964) (from Betz-Paper)
c
         VBV0 = VEL/V0
         qbar=zion*dlog(2.43*vbv0/(zion**0.4))/dlog(7.*(zion**0.3))
         tmp=0.32*(zion**0.45)
         delsqr=tmp*tmp
c
      else if (qopt .eq. 1.) then
c
c     From: H.D. Betz, Heavy Ion Charge States,
c           in: Applied Atomic Collision Physics, Vol. 4 (Acad. Press 19
c
         VBV0 = VEL/V0
         TMP = -A*(VBV0**DEL)/(ZION**GAM)
         QBAR = ZION*(1.-DEXP(TMP))
         DELSQR = .0729 * ZION
c
      else if (qopt .eq. 2.) then
c
c     From: E. Rehm : based on Betz's expression but fit of parameters
c                     to position of 18O,15N, etc... groups in 18F + CH2
c           
c
         VBV0 = VEL/V0
         TMP = -Ar*(VBV0**DELr)/(ZION**GAMr)
         QBAR = ZION*(1.-DEXP(TMP))
         DELSQR = .0729 * ZION
c
c
      else if (qopt .eq. 3.) then
c***** ref. Reiner Kruecken : expressions for low velocities
c                             from Ninov
c
         vbv0 = vel/v0
         Q1=ZION*(1.0-1.04*exp(-0.91*vbv0*ZION**(-0.66667)))
         Q2=(0.394*vbv0*ZION**0.33333) +1.65       
         qbar = Q1  
         if (vbv0.le.4. and. Q2.gt.Q1) qbar = Q2
c***** delsqr taken from Betz
         DELSQR = .0729 * ZION
c
c
      else if (qopt .eq. 4.) then
c
c     From: G. Schiwietz : reference: G.Schiwietz, P.L.Grande; NIM B 175-177 (2001) 125.
c	JMF, August 2007
c           
         VBV0 = VEL/V0
	 BASE = VBV0*(ZION**(-0.52))*(ZGAS**(0.03-0.017*(ZION**(-0.52))*VBV0))
	   POW = 1+0.4/ZION
	   x = BASE**POW
	   NUMER = ZION*(376*x+(x**6))
	   DENOM = (1428 -1206*(x**0.5) +690*x +(x**6))
	   QBAR = NUMER/DENOM
c***** DELSQR = Variance = 
c
	   W = 0.6
	   F1 = ((QBAR + 0.37*(ZION**0.6))/QBAR)**0.5
	   F2 = ((ZION - QBAR + 0.37*(ZION**0.6))/(ZION - QBAR))**0.5
	   DENOM2 = (ZION**(-0.27))*(ZGAS**(0.035-0.0009*ZION))*F1*F2
	   DELSQR = (W/DENOM2)**2
c
c
c***** put other formulas here ... 
c
c
      endif
c
      RETURN
      END
      SUBROUTINE QSIG (DQ)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0 
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK62/  GASENE,GASVEL,TOLD,GASL
      COMMON  /BLCK63/  ISEED
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt
      common  /blck70/  vel0,en0,pm0
c-ddc
      integer*8 icalc

      common  /blck71/  gasopt,zgas125,zgas18,dreldee,enold,icalc
c     data v0/2.188d8/
c*****************************************************************
c** Velocity dependence of capture cross section is taken as v**-7
c   which is valid in the high velocity regime ( v > v0 ).
c   See Knudsen, Haugen and Hvelplund, Phys. Rev. A 23 (1981) 597.
c
c     if (vel.le.v0) go to 10             !! THIS VERSION !!
c     sgsig = gassig / ((vel/vel0)**7)    !! IS CANCELLED !!
c*****************************************************************
c
c     Capture cross section are updated as function of energy
c     using scaling law of Schlachter et al. See Ref in subroutine sigca
c     Scaling law is used to calculate relative change in sigma(capture)
c     for q = qbar. 
c     Capture cross sections are updated whenever this relative change i
c     larger than drel. drel presently taken as 15%.
c     Check for updating condition is done in subroutine gasint
c
C** CALCULATE SIGL FROM QBAR,DELSQR AND SIGC
C
C** ATBCC = NB. ATOMS BY CC * 1.E-16 
      ALOS = 1./DELSQR - ACAPT
      ACL1 = (ACAPT-ALOS)/2.0-ALOS*DQ
      SIGL = GASSIG*DEXP(ACL1)
      SIGC = GASSIG*DEXP(ACAPT*DQ)
      SIGT = SIGC + SIGL
      GASMFP = 1./(SIGT*ATBCC)
      RETURN
      END
C
      FUNCTION RANDOM(K,ISEED)
C
C     RANDOM DISTRIBUTION SELECTION
C     K=0, FLAT DISTRIBUTION BETWEEN -1.0 AND +1.0
C     K=1, GAUSSIAN DISTRIBUTION WITH HALF-MAX AT -1.0 AND +1.0
C          (CONSTANT  A1 = 0.5*SQRT(PI/(2.0*LOG(2.0)))
C
      PARAMETER (A1=0.75269185)
      PARAMETER (A2=-0.15625983,A4=+0.021262254,A6=-0.024505330)
      PARAMETER (B0=1.0-(A2+A4+A6),B2=-0.14234502,B4=-0.016316285)
      REAL*8    RANDOM,RND
C
      RANDOM=2.0*RAN(0)-1.0
      IF(K .EQ. 0) RETURN
      RND=RANDOM
      Y=RND**2
      S1=SQRT(1.0-Y)
      X=RND*((((A6*Y+A4)*Y+A2)*Y)+B0+B2*S1+B4*SQRT(S1))
      RANDOM=A1*LOG((1.0+X)/(1.0-X))
      RETURN
      END
C
C
        SUBROUTINE  RAYS( JNR, NR, NRXS )                !JDL 10-MAR-84 
C****                                                                   
        IMPLICIT REAL*8(A-H,O-Z)                                        
      COMMON  /BLCK 1/  XI, YI, ZI, VXI, VYI, VZI, DELP, DELM      !JDL 
      COMMON  /BLCK15/  TMIN,PMIN,XMAX,TMAX,YMAX,PMAX,DMAX,        !JDL 
     1                  STMN,SPMN,SXMX,STMX,SYMX,SPMX,SDMX,SUMX,   !JDL
     2                  SEED,SEEP,DXHW,DTHW,DYHW,DPHW,DEHW,DMHW,   !JDL
     3                  SEC1,SEC2,SEC3,SEC4,SEC5,SEC6,SEC7,SEC8    !JDL
      DIMENSION XI(100), YI(100), ZI(100), VXI(100), VYI(100), VZI(100),
     1        DELP(100)                                                 
100     FORMAT (///10X, 'JNR = ', I10 ///)                              
C****                                                                   
C****                                                                   
        DO 1 I=1,100                                                    
        XI(I)=0.                                                        
        YI(I)=0.                                                        
        ZI(I)=0.                                                        
        VXI(I)=0.                                                       
        VYI(I)=0.                                                       
        VZI(I)=0.                                                       
        DELP(I)=0.                                                      
1       CONTINUE                                                        
        IF (TMIN.EQ.0.) TMIN=1.0                                        
        IF (PMIN.EQ.0.) PMIN=1.0                                        
        TMAX2 = TMAX/2.0                                                
        TMAX3 = TMAX/3.0                                                
        PMAX2 = PMAX/2.0                                                
        PMAX3 = 2.*PMAX/3.0                                             
        IF (JNR .EQ.  2) GO TO 2                         !JDL 10-MAR-84
        IF (JNR .EQ.  6) GO TO 2                         !JDL 10-MAR-84 
        IF (JNR .EQ. 14) GO TO 2                         !JDL 10-MAR-84 
        IF (JNR .EQ. 46) GO TO 3                         !JDL 10-MAR-84 
        PRINT 100, JNR                                   !JDL 10-MAR-84 
        CALL EXIT                                                       
2       VXI(2)=TMIN                                                     
        VYI(2)=PMIN                                                     
        IF (JNR .EQ. 2) GO TO 5                          !JDL 10-MAR-84 
        VXI(3)=TMAX2                                                    
        VXI(4)=-TMAX2                                                   
        VXI(5)=TMAX                                                     
        VXI(6)=-TMAX                                                    
        IF (JNR .EQ. 6) GO TO 5                          !JDL-10-MAR-84 
        VYI(7)=PMAX2                                                    
        VXI(8)=TMAX2                                                    
        VYI(8)=PMAX2                                                    
        VXI(9)=-TMAX2                                                   
        VYI(9)=PMAX2                                                    
        VXI(10)=TMAX                                                    
        VYI(10)=PMAX2                                                   
        VXI(11)=-TMAX                                                   
        VYI(11)=PMAX2                                                   
        VYI(12)=PMAX                                                    
        VXI(13)=TMAX2                                                   
        VYI(13)=PMAX                                                    
        VXI(14)=-TMAX2                                                  
        VYI(14)=PMAX                                                    
C****
C****
C****
    5   DO 4 I=1,JNR                                     !JDL 10-MAR-84
        XI(I) = XMAX
        YI(I) = YMAX
    4   DELP(I) = DMAX
        GO TO 10                                         !JDL 10-MAR-84 
C****                                                                   
C****                                                                   
C****                                                                   
3       VXI(2)=TMIN                                                     
        VYI(2)=PMIN                                                     
        XI(3)=XMAX                                                      
        XI(4)=-XMAX                                                     
        VXI(5)=TMAX3                                                    
        VXI(6)=-TMAX3                                                   
        YI(7)=YMAX                                                      
        YI(8)=-YMAX                                                     
        VYI(9)=PMAX3                                                    
        VYI(10)=-PMAX3                                                  
        DELP(11)=DMAX                                                   
        DELP(12)=-DMAX                                                  
        XI(13)=XMAX                                                     
        VXI(13)=TMAX3                                                   
        XI(14)=-XMAX                                                    
        VXI(14)=-TMAX3                                                  
        XI(15)=XMAX                                                     
        DELP(15)=DMAX                                                   
        XI(16)=-XMAX                                                    
        DELP(16)=-DMAX                                                  
        VXI(17)=TMAX3                                                   
        DELP(17)=DMAX                                                   
        VXI(18)=-TMAX3                                                  
        DELP(18)=-DMAX                                                  
        YI(19)=YMAX                                                     
        VYI(19)=PMAX3                                                   
        YI(20)=-YMAX                                                    
        VYI(20)=PMAX3                                                   
        XI(21)=XMAX                                                     
        YI(21)=YMAX                                                     
        XI(22)=-XMAX                                                    
        YI(22)=YMAX                                                     
        XI(23)=XMAX                                                     
        VYI(23)=PMAX3                                                   
        XI(24)=-XMAX                                                    
        VYI(24)=PMAX3                                                   
        VXI(25)=TMAX3                                                   
        YI(25)=YMAX                                                     
        YI(26)=YMAX                                                     
        VXI(27)=TMAX3                                                   
        VYI(27)=PMAX3                                                   
        VXI(28)=-TMAX3                                                  
        VYI(28)=PMAX3                                                   
        YI(29)=YMAX                                                     
        DELP(29)=DMAX                                                   
        YI(30)=YMAX                                                     
        DELP(30)=-DMAX                                                  
        VYI(31)=PMAX3                                                   
        DELP(31)=DMAX                                                   
        VYI(32)=PMAX3                                                   
        DELP(32)=-DMAX                                                  
        VXI(33)=TMAX                                                    
        VXI(34)=-TMAX                                                   
        XI(35)=XMAX                                                     
        VXI(35)=TMAX                                                    
        XI(36)=-XMAX                                                    
        VXI(36)=TMAX                                                    
        XI(37)=XMAX                                                     
        VXI(37)=-TMAX                                                   
        XI(38)=-XMAX                                                    
        VXI(38)=-TMAX                                                   
        VXI(39)=TMAX                                                    
        DELP(39)=DMAX                                                   
        VXI(40)=TMAX                                                    
        DELP(40)=-DMAX                                                  
        VXI(41)=-TMAX                                                   
        DELP(41)=DMAX                                                   
        VXI(42)=-TMAX                                                   
        DELP(42)=-DMAX                                                  
        VYI(43)=PMAX                                                    
        VXI(44)=TMAX                                                    
        VYI(44)=PMAX                                                    
        DELP(45)=3.*DMAX                                                
        DELP(46)=-3.*DMAX                                               
C****                              !Changes from here... !JDL 10-MAR-84
   10   IF(NR .LE. JNR) RETURN
        IF((NRXS .LT. 1) .OR. (NRXS .GT. 9)) RETURN
C****
C****   PHASE-SPACE ELLIPSE GENERATOR
C****   ADDS (NR-JNR) RAYS DISTRIBUTED UNIFORMLY AROUND ELLIPSES
C****   IN REAL AND PHASE SPACE.
C****      NRXS = 1, "DIAGONAL SCAN"  (X AND Y IN PHASE).
C****             2, "CIRCLE SCAN"    (X LEADS Y BY 90-DEGREES).
C****             3, "ELLIPSOID SCAN" (X ELLIPSE AT EACH Y POINT).
C****
C****   ELLIPSES MAY BE DIVIDED INTO 3 ENERGY OR MASS GROUPS.
C****
        NE = 1
        IF((SDMX .NE. 0.0) .OR. (SUMX .NE. 0.0)) NE = 3
        NS = (NR-JNR+NE-1)/NE
        TWOPI = 6.283185307D0
        DAX = TWOPI/FLOAT(NS)
        IF(NRXS .NE. 3) GO TO 15
        NY = 2
        IF(NS .GE. 32) NY = 4
        IF(NS .GE. 64) NY = 8
        DAX = DAX*FLOAT(NY)
        DAY = (TWOPI/FLOAT(NY))/2.0
   15   AAX = -DAX
        N1  = JNR+1
        DO 20 I=N1,NR
        AAX = AAX + DAX
        SAX = DSIN(AAX)
        CAX = DCOS(AAX)
        XI(I)   =  SXMX*CAX
        YI(I)   =  SYMX*CAX
        VXI(I)  =  STMX*SAX
        VYI(I)  =  SPMX*SAX
        DELP(I) =  SDMX*FLOAT((I-JNR-1)/NS-1)
        IF(NRXS .EQ. 1) GO TO 20    !DIAGONAL SCAN (IN PHASE)
        YI(I)   = +SYMX*SAX
        VYI(I)  = -SPMX*CAX
        IF(NRXS .EQ. 2) GO TO 20    !CIRCULAR SCAN (X LEADS Y)
        AAY    =  DAY*AINT((AAX+0.001)/TWOPI)
        YI(I)  = -SYMX*DSIN(AAY)    !ELLIPSOID SCAN (ONE DISCRETE STEP
        VYI(I) = +SPMX*DCOS(AAY)    !IN Y PER 2-PI REVOLUTION IN X.)
   20   CONTINUE
C****                                  !...down to here. !JDL 10-MAR-84
        RETURN                                                          
        END                                                             
       SUBROUTINE SHLLCO        
C 
C      SHELL CORRECTIONS FOR BETHE FORMULA
C      FOR HYDROGEN AS PROJECTILE.        
C 
C*****************************************
C 
c--ddc for compatability with shellc in other functions! 
c(note.. compiler did not complain!)
       IMPLICIT REAL*8(A-H,O-Z)

       DIMENSION SC(92)         
       COMMON /SHELLC/ SC       
       SC(1)=4.       
       SC(2)=-3.      
       SC(3)=-9.      
       SC(4)=-8.      
       SC(5)=-7.      
       SC(6)=-9.      
       SC(7)=-6.      
       SC(8)=-9.      
       SC(9)=-10.     
       SC(10)=-11.    
       DO 10 I=11,50  
       Z=FLOAT(I)     
       Z2=Z*Z         
       Z3=Z*Z2        
       Z4=Z2*Z2       
       Z5=Z*Z4        
 10    SC(I)=-21.258+2.1282*Z-0.099659*Z2+0.0011623*Z3        
     1 +0.000024822*Z4-0.0000004405*Z5    
       DO 20 I=51,92  
       Z=FLOAT(I)     
       Z2=Z*Z         
       Z3=Z2*Z        
       Z4=Z2*Z2       
       Z5=Z4*Z        
 20    SC(I)=1089.1-85.686*Z+2.6457*Z2-0.040082*Z3  
     1 +0.0002976*Z4-0.00000086478*Z5     
       RETURN         
       END  
C 
C***************************************  
C 
       BLOCK DATA     
C 
C      TABLES OF COEFFICIENTS   
C 
C***************************************  
C 
c--ddc for datad in other units
      IMPLICIT REAL*8(A-H,O-Z)

       DIMENSION AH1(92),AH2(92),AH3(92),AH4(92),   
     1 AH5(92),AH6(92),AH7(92)  
       DIMENSION A1(92),A2(92),A3(92),A4(92),A5(92),A6(92),A7(92),      
     1 A8(92),A9(92)  
       DIMENSION B1(92),B2(92),B3(92),B4(92),B5(92),B6(92),B7(92),      
     1 B8(92),B9(92)  
       COMMON /DATAD/ AH1,AH2,AH3,AH4,AH5,AH6,AH7,  
     1 A1,A2,A3,A4,A5,A6,A7,A8,A9,        
     1 B1,B2,B3,B4,B5,B6,B7,B8,B9         
C 
C      COEFFICIENTS FOR H IN SOLID AND GASEOUS MATTER         
C 
       DATA AH1( 1),AH2( 1),AH3( 1),AH4( 1),AH5( 1),AH6( 1),AH7( 1)     
     1 ,AH1( 2),AH2( 2),AH3( 2),AH4( 2),AH5( 2),AH6( 2),AH7( 2)         
     1 ,AH1( 3),AH2( 3),AH3( 3),AH4( 3),AH5( 3),AH6( 3),AH7( 3)         
     1 ,AH1( 4),AH2( 4),AH3( 4),AH4( 4),AH5( 4),AH6( 4),AH7( 4)         
     1 ,AH1( 5),AH2( 5),AH3( 5),AH4( 5),AH5( 5),AH6( 5),AH7( 5)         
     1 ,AH1( 6),AH2( 6),AH3( 6),AH4( 6),AH5( 6),AH6( 6),AH7( 6)         
     1 ,AH1( 7),AH2( 7),AH3( 7),AH4( 7),AH5( 7),AH6( 7),AH7( 7)         
     1 ,AH1( 8),AH2( 8),AH3( 8),AH4( 8),AH5( 8),AH6( 8),AH7( 8)         
     1 ,AH1( 9),AH2( 9),AH3( 9),AH4( 9),AH5( 9),AH6( 9),AH7( 9)         
     1 ,AH1(10),AH2(10),AH3(10),AH4(10),AH5(10),AH6(10),AH7(10)         
     1  /1.262, 1.440,  242.6,12000.0,0.115900,0.000510,54360.0,        
     1  1.229, 1.397,  484.5, 5873.0,0.052250,0.001020,24510.0,         
     1  1.411, 1.600,  725.6, 3013.0,0.045780,0.001530,21470.0,         
     1  2.248, 2.590,  966.0,  153.8,0.034750,0.002039,16300.0,         
     1  2.474, 2.815, 1206.0, 1060.0,0.028550,0.002549,13450.0,         
     1  2.631, 2.989, 1445.0,  957.2,0.028190,0.003059,13220.0,         
     1  2.954, 3.350, 1683.0, 1900.0,0.025130,0.003569,11790.0,         
     1  2.652, 3.000, 1920.0, 2000.0,0.022300,0.004079,10460.0,         
     1  2.085, 2.352, 2157.0, 2634.0,0.018160,0.004589, 8517.0,         
     1  1.951, 2.199, 2393.0, 2699.0,0.015680,0.005099, 7353.0/         
       DATA AH1(11),AH2(11),AH3(11),AH4(11),AH5(11),AH6(11),AH7(11)     
     1 ,AH1(12),AH2(12),AH3(12),AH4(12),AH5(12),AH6(12),AH7(12)         
     1 ,AH1(13),AH2(13),AH3(13),AH4(13),AH5(13),AH6(13),AH7(13)         
     1 ,AH1(14),AH2(14),AH3(14),AH4(14),AH5(14),AH6(14),AH7(14)         
     1 ,AH1(15),AH2(15),AH3(15),AH4(15),AH5(15),AH6(15),AH7(15)         
     1 ,AH1(16),AH2(16),AH3(16),AH4(16),AH5(16),AH6(16),AH7(16)         
     1 ,AH1(17),AH2(17),AH3(17),AH4(17),AH5(17),AH6(17),AH7(17)         
     1 ,AH1(18),AH2(18),AH3(18),AH4(18),AH5(18),AH6(18),AH7(18)         
     1 ,AH1(19),AH2(19),AH3(19),AH4(19),AH5(19),AH6(19),AH7(19)         
     1 ,AH1(20),AH2(20),AH3(20),AH4(20),AH5(20),AH6(20),AH7(20)         
     1  /2.542, 2.869, 2628.0, 1854.0,0.014720,0.005609, 6905.0,        
     1  3.792, 4.293, 2862.0, 1009.0,0.013970,0.006118, 6551.0,         
     1  4.154, 4.739, 2766.0,  164.5,0.020230,0.006628, 6309.0,         
     1  4.150, 4.700, 3329.0,  550.0,0.013210,0.007138, 6194.0,         
     1  3.232, 3.647, 3561.0, 1560.0,0.012670,0.007648, 5942.0,         
     1  3.447, 3.891, 3792.0, 1219.0,0.012110,0.008158, 5678.0,         
     1  5.047, 5.714, 4023.0,  878.6,0.011780,0.008668, 5524.0,         
     1  5.731, 6.500, 4253.0,  530.0,0.011230,0.009178, 5268.0,         
     1  5.151, 5.833, 4482.0,  545.7,0.011290,0.009687, 5295.0,         
     1  5.521, 6.252, 4710.0,  553.3,0.011120,0.010200, 5214.0/         
       DATA AH1(21),AH2(21),AH3(21),AH4(21),AH5(21),AH6(21),AH7(21)     
     1 ,AH1(22),AH2(22),AH3(22),AH4(22),AH5(22),AH6(22),AH7(22)         
     1 ,AH1(23),AH2(23),AH3(23),AH4(23),AH5(23),AH6(23),AH7(23)         
     1 ,AH1(24),AH2(24),AH3(24),AH4(24),AH5(24),AH6(24),AH7(24)         
     1 ,AH1(25),AH2(25),AH3(25),AH4(25),AH5(25),AH6(25),AH7(25)         
     1 ,AH1(26),AH2(26),AH3(26),AH4(26),AH5(26),AH6(26),AH7(26)         
     1 ,AH1(27),AH2(27),AH3(27),AH4(27),AH5(27),AH6(27),AH7(27)         
     1 ,AH1(28),AH2(28),AH3(28),AH4(28),AH5(28),AH6(28),AH7(28)         
     1 ,AH1(29),AH2(29),AH3(29),AH4(29),AH5(29),AH6(29),AH7(29)         
     1 ,AH1(30),AH2(30),AH3(30),AH4(30),AH5(30),AH6(30),AH7(30)         
     1  /5.201, 5.884, 4938.0,  560.9,0.009995,0.010710, 4688.0,        
     1  4.862, 5.496, 5165.0,  568.5,0.009474,0.011220, 4443.0,         
     1  4.480, 5.055, 5391.0,  952.3,0.009117,0.011730, 4276.0,         
     1  3.983, 4.489, 5616.0, 1336.0,0.008413,0.012240, 3946.0,         
     1  3.469, 3.907, 5725.0, 1461.0,0.008829,0.012750, 3785.0,         
     1  3.519, 3.963, 6065.0, 1243.0,0.007782,0.013260, 3650.0,         
     1  3.140, 3.535, 6288.0, 1372.0,0.007361,0.013770, 3453.0,         
     1  3.553, 4.004, 6205.0,  555.1,0.008763,0.014280, 3297.0,         
     1  3.696, 4.175, 4673.0,  387.8,0.021880,0.014790, 3174.0,         
     1  4.210, 4.750, 6953.0,  295.2,0.006809,0.015300, 3194.0/         
       DATA AH1(31),AH2(31),AH3(31),AH4(31),AH5(31),AH6(31),AH7(31)     
     1 ,AH1(32),AH2(32),AH3(32),AH4(32),AH5(32),AH6(32),AH7(32)         
     1 ,AH1(33),AH2(33),AH3(33),AH4(33),AH5(33),AH6(33),AH7(33)         
     1 ,AH1(34),AH2(34),AH3(34),AH4(34),AH5(34),AH6(34),AH7(34)         
     1 ,AH1(35),AH2(35),AH3(35),AH4(35),AH5(35),AH6(35),AH7(35)         
     1 ,AH1(36),AH2(36),AH3(36),AH4(36),AH5(36),AH6(36),AH7(36)         
     1 ,AH1(37),AH2(37),AH3(37),AH4(37),AH5(37),AH6(37),AH7(37)         
     1 ,AH1(38),AH2(38),AH3(38),AH4(38),AH5(38),AH6(38),AH7(38)         
     1 ,AH1(39),AH2(39),AH3(39),AH4(39),AH5(39),AH6(39),AH7(39)         
     1 ,AH1(40),AH2(40),AH3(40),AH4(40),AH5(40),AH6(40),AH7(40)         
     1  /5.041, 5.697, 7173.0,  202.6,0.006725,0.015810, 3154.0,        
     1  5.554, 6.300, 6496.0,  110.0,0.009689,0.016320, 3097.0,         
     1  5.323, 6.012, 7611.0,  292.5,0.006447,0.016830, 3024.0,         
     1  5.874, 6.656, 7395.0,  117.5,0.007684,0.017340, 3006.0,         
     1  5.611, 6.335, 8046.0,  365.2,0.006244,0.017850, 2928.0,         
     1  6.411, 7.250, 8262.0,  220.0,0.006087,0.018360, 2855.0,         
     1  5.694, 6.429, 8478.0,  292.9,0.006087,0.018860, 2855.0,         
     1  6.339, 7.159, 8693.0,  330.3,0.006003,0.019370, 2815.0,         
     1  6.407, 7.234, 8907.0,  367.8,0.005889,0.019880, 2762.0,         
     1  6.734, 7.603, 9120.0,  405.2,0.005765,0.020390, 2700.0/         
       DATA AH1(41),AH2(41),AH3(41),AH4(41),AH5(41),AH6(41),AH7(41)     
     1 ,AH1(42),AH2(42),AH3(42),AH4(42),AH5(42),AH6(42),AH7(42)         
     1 ,AH1(43),AH2(43),AH3(43),AH4(43),AH5(43),AH6(43),AH7(43)         
     1 ,AH1(44),AH2(44),AH3(44),AH4(44),AH5(44),AH6(44),AH7(44)         
     1 ,AH1(45),AH2(45),AH3(45),AH4(45),AH5(45),AH6(45),AH7(45)         
     1 ,AH1(46),AH2(46),AH3(46),AH4(46),AH5(46),AH6(46),AH7(46)         
     1 ,AH1(47),AH2(47),AH3(47),AH4(47),AH5(47),AH6(47),AH7(47)         
     1 ,AH1(48),AH2(48),AH3(48),AH4(48),AH5(48),AH6(48),AH7(48)         
     1 ,AH1(49),AH2(49),AH3(49),AH4(49),AH5(49),AH6(49),AH7(49)         
     1 ,AH1(50),AH2(50),AH3(50),AH4(50),AH5(50),AH6(50),AH7(50)         
     1  /6.902, 7.791, 9333.0,  442.7,0.005587,0.020900, 2621.0,        
     1  6.425, 7.248, 9545.0,  480.2,0.005367,0.021410, 2517.0,         
     1  6.799, 7.671, 9756.0,  517.6,0.005315,0.021920, 2493.0,         
     1  6.108, 6.887, 9966.0,  555.1,0.005151,0.022430, 2416.0,         
     1  5.924, 6.677,10180.0,  592.5,0.004919,0.022940, 2307.0,         
     1  5.238, 5.900,10380.0,  630.0,0.004758,0.023450, 2231.0,         
     1  5.623, 6.354, 7160.0,  337.6,0.013940,0.023960, 2193.0,         
     1  5.814, 6.554,10800.0,  355.5,0.004626,0.024470, 2170.0,         
     1  6.230, 7.024,11010.0,  370.9,0.004540,0.024980, 2129.0,         
     1  6.410, 7.227,11210.0,  386.4,0.004474,0.025490, 2099.0/         
       DATA AH1(51),AH2(51),AH3(51),AH4(51),AH5(51),AH6(51),AH7(51)     
     1 ,AH1(52),AH2(52),AH3(52),AH4(52),AH5(52),AH6(52),AH7(52)         
     1 ,AH1(53),AH2(53),AH3(53),AH4(53),AH5(53),AH6(53),AH7(53)         
     1 ,AH1(54),AH2(54),AH3(54),AH4(54),AH5(54),AH6(54),AH7(54)         
     1 ,AH1(55),AH2(55),AH3(55),AH4(55),AH5(55),AH6(55),AH7(55)         
     1 ,AH1(56),AH2(56),AH3(56),AH4(56),AH5(56),AH6(56),AH7(56)         
     1 ,AH1(57),AH2(57),AH3(57),AH4(57),AH5(57),AH6(57),AH7(57)         
     1 ,AH1(58),AH2(58),AH3(58),AH4(58),AH5(58),AH6(58),AH7(58)         
     1 ,AH1(59),AH2(59),AH3(59),AH4(59),AH5(59),AH6(59),AH7(59)         
     1 ,AH1(60),AH2(60),AH3(60),AH4(60),AH5(60),AH6(60),AH7(60)         
     1  /7.500, 8.480, 8608.0,  348.0,0.009074,0.026000, 2069.0,        
     1  6.979, 7.871,11620.0,  392.4,0.004402,0.026510, 2065.0,         
     1  7.725, 8.716,11830.0,  394.8,0.004376,0.027020, 2052.0,         
     1  8.231, 9.289,12030.0,  397.3,0.004384,0.027530, 2056.0,         
     1  7.287, 8.218,12230.0,  399.7,0.004447,0.028040, 2086.0,         
     1  7.899, 8.911,12430.0,  402.1,0.004511,0.028550, 2116.0,         
     1  8.041, 9.071,12630.0,  404.5,0.004540,0.029060, 2129.0,         
     1  7.489, 8.444,12830.0,  406.9,0.004420,0.029570, 2073.0,         
     1  7.291, 8.219,13030.0,  409.3,0.004298,0.030080, 2016.0,         
     1  7.098, 8.000,13230.0,  411.8,0.004182,0.030590, 1962.0/         
       DATA AH1(61),AH2(61),AH3(61),AH4(61),AH5(61),AH6(61),AH7(61)     
     1 ,AH1(62),AH2(62),AH3(62),AH4(62),AH5(62),AH6(62),AH7(62)         
     1 ,AH1(63),AH2(63),AH3(63),AH4(63),AH5(63),AH6(63),AH7(63)         
     1 ,AH1(64),AH2(64),AH3(64),AH4(64),AH5(64),AH6(64),AH7(64)         
     1 ,AH1(65),AH2(65),AH3(65),AH4(65),AH5(65),AH6(65),AH7(65)         
     1 ,AH1(66),AH2(66),AH3(66),AH4(66),AH5(66),AH6(66),AH7(66)         
     1 ,AH1(67),AH2(67),AH3(67),AH4(67),AH5(67),AH6(67),AH7(67)         
     1 ,AH1(68),AH2(68),AH3(68),AH4(68),AH5(68),AH6(68),AH7(68)         
     1 ,AH1(69),AH2(69),AH3(69),AH4(69),AH5(69),AH6(69),AH7(69)         
     1 ,AH1(70),AH2(70),AH3(70),AH4(70),AH5(70),AH6(70),AH7(70)         
     1  /6.910, 7.786,13430.0,  414.2,0.004058,0.031100, 1903.0,        
     1  6.728, 7.580,13620.0,  416.6,0.003976,0.031610, 1865.0,         
     1  6.551, 7.380,13820.0,  419.0,0.003877,0.032120, 1819.0,         
     1  6.739, 7.592,14020.0,  421.4,0.003863,0.032630, 1812.0,         
     1  6.212, 7.996,14210.0,  423.9,0.003725,0.033140, 1747.0,         
     1  5.517, 6.210,14440.0,  426.3,0.003632,0.033650, 1703.0,         
     1  5.219, 5.874,14600.0,  428.7,0.003498,0.034160, 1640.0,         
     1  5.071, 5.706,14790.0,  433.0,0.003405,0.034670, 1597.0,         
     1  4.926, 5.542,14980.0,  433.5,0.003342,0.035180, 1567.0,         
     1  4.787, 5.386,15170.0,  435.9,0.003292,0.035690, 1544.0/         
       DATA AH1(71),AH2(71),AH3(71),AH4(71),AH5(71),AH6(71),AH7(71)     
     1 ,AH1(72),AH2(72),AH3(72),AH4(72),AH5(72),AH6(72),AH7(72)         
     1 ,AH1(73),AH2(73),AH3(73),AH4(73),AH5(73),AH6(73),AH7(73)         
     1 ,AH1(74),AH2(74),AH3(74),AH4(74),AH5(74),AH6(74),AH7(74)         
     1 ,AH1(75),AH2(75),AH3(75),AH4(75),AH5(75),AH6(75),AH7(75)         
     1 ,AH1(76),AH2(76),AH3(76),AH4(76),AH5(76),AH6(76),AH7(76)         
     1 ,AH1(77),AH2(77),AH3(77),AH4(77),AH5(77),AH6(77),AH7(77)         
     1 ,AH1(78),AH2(78),AH3(78),AH4(78),AH5(78),AH6(78),AH7(78)         
     1 ,AH1(79),AH2(79),AH3(79),AH4(79),AH5(79),AH6(79),AH7(79)         
     1 ,AH1(80),AH2(80),AH3(80),AH4(80),AH5(80),AH6(80),AH7(80)         
     1  /4.893, 5.505,15360.0,  438.4,0.003243,0.036200, 1521.0,        
     1  5.028, 5.657,15550.0,  440.8,0.003195,0.036710, 1499.0,         
     1  4.738, 5.329,15740.0,  443.2,0.003186,0.037220, 1494.0,         
     1  4.574, 5.144,15930.0,  442.4,0.003144,0.037730, 1475.0,         
     1  5.200, 5.851,16120.0,  441.6,0.003122,0.038240, 1464.0,         
     1  5.070, 5.704,16300.0,  440.9,0.003082,0.038750, 1446.0,         
     1  4.945, 5.563,16490.0,  440.1,0.002965,0.039260, 1390.0,         
     1  4.476, 5.034,16670.0,  439.3,0.002871,0.039770, 1347.0,         
     1  4.856, 5.460,18320.0,  438.5,0.002542,0.040280, 1354.0,         
     1  4.308, 4.843,17040.0,  487.8,0.002882,0.040790, 1352.0/         
       DATA AH1(81),AH2(81),AH3(81),AH4(81),AH5(81),AH6(81),AH7(81)     
     1 ,AH1(82),AH2(82),AH3(82),AH4(82),AH5(82),AH6(82),AH7(82)         
     1 ,AH1(83),AH2(83),AH3(83),AH4(83),AH5(83),AH6(83),AH7(83)         
     1 ,AH1(84),AH2(84),AH3(84),AH4(84),AH5(84),AH6(84),AH7(84)         
     1 ,AH1(85),AH2(85),AH3(85),AH4(85),AH5(85),AH6(85),AH7(85)         
     1 ,AH1(86),AH2(86),AH3(86),AH4(86),AH5(86),AH6(86),AH7(86)         
     1 ,AH1(87),AH2(87),AH3(87),AH4(87),AH5(87),AH6(87),AH7(87)         
     1 ,AH1(88),AH2(88),AH3(88),AH4(88),AH5(88),AH6(88),AH7(88)         
     1 ,AH1(89),AH2(89),AH3(89),AH4(89),AH5(89),AH6(89),AH7(89)         
     1 ,AH1(90),AH2(90),AH3(90),AH4(90),AH5(90),AH6(90),AH7(90)         
     1  /4.723, 5.311,17220.0,  537.0,0.002913,0.041300, 1366.0,        
     1  5.319, 5.982,17400.0,  586.3,0.002871,0.041810, 1347.0,         
     1  5.956, 6.700,17800.0,  677.0,0.002660,0.042320, 1336.0,         
     1  6.158, 6.928,17770.0,  586.3,0.002813,0.042830, 1319.0,         
     1  6.204, 6.979,17950.0,  586.3,0.002776,0.043340, 1302.0,         
     1  6.181, 6.954,18120.0,  586.3,0.002748,0.043850, 1289.0,         
     1  6.949, 7.820,18300.0,  586.3,0.002737,0.044360, 1284.0,         
     1  7.506, 8.448,18480.0,  586.3,0.002727,0.044870, 1279.0,         
     1  7.649, 8.609,18660.0,  586.3,0.002697,0.045380, 1265.0,         
     1  7.710, 8.679,18830.0,  586.3,0.002641,0.045890, 1239.0/         
       DATA AH1(91),AH2(91),AH3(91),AH4(91),AH5(91),AH6(91),AH7(91)     
     1 ,AH1(92),AH2(92),AH3(92),AH4(92),AH5(92),AH6(92),AH7(92)         
     1  /7.407, 8.336,19010.0,  586.3,0.002603,0.046400, 1221.0,        
     1  7.290, 8.204,19180.0,  586.3,0.002573,0.046910, 1207.0/         
C 
C      COEFFICIENTS FOR HE IN SOLID MATTER, LOW ENERGIES      
C 
       DATA A1( 1),A2( 1),A3( 1),A4( 1),A5( 1)      
     1 ,A1( 2),A2( 2),A3( 2),A4( 2),A5( 2)
     1 ,A1( 3),A2( 3),A3( 3),A4( 3),A5( 3)
     1 ,A1( 4),A2( 4),A3( 4),A4( 4),A5( 4)
     1 ,A1( 5),A2( 5),A3( 5),A4( 5),A5( 5)
     1 ,A1( 6),A2( 6),A3( 6),A4( 6),A5( 6)
     1 ,A1( 7),A2( 7),A3( 7),A4( 7),A5( 7)
     1 ,A1( 8),A2( 8),A3( 8),A4( 8),A5( 8)
     1 ,A1( 9),A2( 9),A3( 9),A4( 9),A5( 9)
     1 ,A1(10),A2(10),A3(10),A4(10),A5(10)
     1   /0.9661,  0.4126,  6.9200,  8.8310,  2.5820,         
     1   2.0270,  0.2931, 26.3400,  6.6600,  0.3409,
     1   1.4200,  0.4900, 12.2500, 32.0000,  9.1610,
     1   2.2060,  0.5100, 15.3200,  0.2500,  8.9950,
     1   3.6910,  0.4128, 18.4800, 50.7200,  9.0000,
     1   4.2320,  0.3877, 22.9900, 35.0000,  7.9930,
     1   2.5100,  0.4752, 38.2600, 13.0200,  1.8920,
     1   1.7660,  0.5261, 37.1100, 15.2400,  2.8040,
     1   1.5330,  0.5310, 40.4400, 18.4100,  2.7180,
     1   1.1830,  0.5500, 39.8300, 17.4900,  4.0010/
       DATA A1(11),A2(11),A3(11),A4(11),A5(11)      
     1 ,A1(12),A2(12),A3(12),A4(12),A5(12)
     1 ,A1(13),A2(13),A3(13),A4(13),A5(13)
     1 ,A1(14),A2(14),A3(14),A4(14),A5(14)
     1 ,A1(15),A2(15),A3(15),A4(15),A5(15)
     1 ,A1(16),A2(16),A3(16),A4(16),A5(16)
     1 ,A1(17),A2(17),A3(17),A4(17),A5(17)
     1 ,A1(18),A2(18),A3(18),A4(18),A5(18)
     1 ,A1(19),A2(19),A3(19),A4(19),A5(19)
     1 ,A1(20),A2(20),A3(20),A4(20),A5(20)
     1   /9.8940,  0.3081, 23.6500,  0.3840, 92.9300,         
     1   4.3000,  0.4700, 34.3000,  3.3000, 12.7400,
     1   2.5000,  0.6250, 45.7000,  0.1000,  4.3590,
     1   2.1000,  0.6500, 49.3400,  1.7880,  4.1330,
     1   1.7290,  0.6562, 53.4100,  2.4050,  3.8450,
     1   1.4020,  0.6791, 58.9800,  3.5280,  3.2110,
     1   1.1170,  0.7044, 69.6900,  3.7050,  2.1560,
     1   0.9172,  0.7240, 79.4400,  3.6480,  1.6460,
     1   8.5540,  0.3817, 83.6100, 11.8400,  1.8750,
     1   6.2970,  0.4622, 65.3900, 10.1400,  5.0360/
       DATA A1(21),A2(21),A3(21),A4(21),A5(21)      
     1 ,A1(22),A2(22),A3(22),A4(22),A5(22)
     1 ,A1(23),A2(23),A3(23),A4(23),A5(23)
     1 ,A1(24),A2(24),A3(24),A4(24),A5(24)
     1 ,A1(25),A2(25),A3(25),A4(25),A5(25)
     1 ,A1(26),A2(26),A3(26),A4(26),A5(26)
     1 ,A1(27),A2(27),A3(27),A4(27),A5(27)
     1 ,A1(28),A2(28),A3(28),A4(28),A5(28)
     1 ,A1(29),A2(29),A3(29),A4(29),A5(29)
     1 ,A1(30),A2(30),A3(30),A4(30),A5(30)
     1   /5.3070,  0.4918, 61.7400, 12.4000,  6.6650,         
     1   4.7100,  0.5087, 65.2800,  8.8060,  5.9480,
     1   6.1510,  0.4524, 83.0000, 18.3100,  2.7100,
     1   6.5700,  0.4322, 84.7600, 15.5300,  2.7790,
     1   5.7380,  0.4492, 84.6100, 14.1800,  3.1010,
     1   5.0310,  0.4707, 85.5800, 16.5500,  3.2110,
     1   4.3200,  0.4947, 76.1400, 10.8500,  5.4410,
     1   4.6520,  0.4571, 80.7300, 22.0000,  4.9520,
     1   3.1140,  0.5236, 76.6700,  7.6200,  6.3850,
     1   3.1140,  0.5236, 76.6700,  7.6200,  7.5020/
       DATA A1(31),A2(31),A3(31),A4(31),A5(31)      
     1 ,A1(32),A2(32),A3(32),A4(32),A5(32)
     1 ,A1(33),A2(33),A3(33),A4(33),A5(33)
     1 ,A1(34),A2(34),A3(34),A4(34),A5(34)
     1 ,A1(35),A2(35),A3(35),A4(35),A5(35)
     1 ,A1(36),A2(36),A3(36),A4(36),A5(36)
     1 ,A1(37),A2(37),A3(37),A4(37),A5(37)
     1 ,A1(38),A2(38),A3(38),A4(38),A5(38)
     1 ,A1(39),A2(39),A3(39),A4(39),A5(39)
     1 ,A1(40),A2(40),A3(40),A4(40),A5(40)
     1   /3.1140,  0.5236, 76.6700,  7.6200,  8.5140,         
     1   5.7460,  0.4662, 79.2400,  1.1850,  7.9930,
     1   2.7920,  0.6346,106.1000,  0.2986,  2.3310,
     1   4.6670,  0.5095,124.3000,  2.1020,  1.6670,
     1   2.4400,  0.6346,105.0000,  0.8300,  2.8510,
     1   1.4910,  0.7118,120.6000,  1.1010,  1.8770,
     1  11.7200,  0.3826,102.8000,  9.2310,  4.3710,
     1   7.1260,  0.4804,119.3000,  5.7840,  2.4540,
     1  11.6100,  0.3955,146.7000,  7.0310,  1.4230,
     1  10.9900,  0.4100,163.9000,  7.1000,  1.0520/
       DATA A1(41),A2(41),A3(41),A4(41),A5(41)      
     1 ,A1(42),A2(42),A3(42),A4(42),A5(42)
     1 ,A1(43),A2(43),A3(43),A4(43),A5(43)
     1 ,A1(44),A2(44),A3(44),A4(44),A5(44)
     1 ,A1(45),A2(45),A3(45),A4(45),A5(45)
     1 ,A1(46),A2(46),A3(46),A4(46),A5(46)
     1 ,A1(47),A2(47),A3(47),A4(47),A5(47)
     1 ,A1(48),A2(48),A3(48),A4(48),A5(48)
     1 ,A1(49),A2(49),A3(49),A4(49),A5(49)
     1 ,A1(50),A2(50),A3(50),A4(50),A5(50)
     1   /9.2410,  0.4275,163.1000,  7.9540,  1.1020,         
     1   9.2760,  0.4180,157.1000,  8.0380,  1.2900,
     1   3.9990,  0.6152, 97.6000,  1.2970,  5.7920,
     1   4.3060,  0.5658, 97.9900,  5.5140,  5.7540,
     1   3.6150,  0.6197, 86.2600,  0.3330,  8.6890,
     1   5.8000,  0.4900,147.2000,  6.9030,  1.2890,
     1   5.6000,  0.4900,130.0000, 10.0000,  2.8440,
     1   3.5500,  0.6068,124.7000,  1.1120,  3.1190,
     1   3.6000,  0.6200,105.8000,  0.1692,  6.0260,
     1   5.4000,  0.5300,103.1000,  3.9310,  7.7670/
       DATA A1(51),A2(51),A3(51),A4(51),A5(51)      
     1 ,A1(52),A2(52),A3(52),A4(52),A5(52)
     1 ,A1(53),A2(53),A3(53),A4(53),A5(53)
     1 ,A1(54),A2(54),A3(54),A4(54),A5(54)
     1 ,A1(55),A2(55),A3(55),A4(55),A5(55)
     1 ,A1(56),A2(56),A3(56),A4(56),A5(56)
     1 ,A1(57),A2(57),A3(57),A4(57),A5(57)
     1 ,A1(58),A2(58),A3(58),A4(58),A5(58)
     1 ,A1(59),A2(59),A3(59),A4(59),A5(59)
     1 ,A1(60),A2(60),A3(60),A4(60),A5(60)
     1   /3.9700,  0.6459,131.8000,  0.2233,  2.7230,         
     1   3.6500,  0.6400,126.8000,  0.6834,  3.4110,
     1   3.1180,  0.6519,164.9000,  1.2080,  1.5100,
     1   2.0310,  0.7181,153.1000,  1.3620,  1.9580,
     1  14.4000,  0.3923,152.5000,  8.3540,  2.5970,
     1  10.9900,  0.4599,138.4000,  4.8110,  3.7260,
     1  16.6000,  0.3773,224.1000,  6.2800,  0.9121,
     1  10.5400,  0.4533,159.3000,  4.8320,  2.5290,
     1  10.3300,  0.4502,162.0000,  5.1320,  2.4440,
     1  10.1500,  0.4471,165.6000,  5.3780,  2.3280/
       DATA A1(61),A2(61),A3(61),A4(61),A5(61)      
     1 ,A1(62),A2(62),A3(62),A4(62),A5(62)
     1 ,A1(63),A2(63),A3(63),A4(63),A5(63)
     1 ,A1(64),A2(64),A3(64),A4(64),A5(64)
     1 ,A1(65),A2(65),A3(65),A4(65),A5(65)
     1 ,A1(66),A2(66),A3(66),A4(66),A5(66)
     1 ,A1(67),A2(67),A3(67),A4(67),A5(67)
     1 ,A1(68),A2(68),A3(68),A4(68),A5(68)
     1 ,A1(69),A2(69),A3(69),A4(69),A5(69)
     1 ,A1(70),A2(70),A3(70),A4(70),A5(70)
     1   /9.9760,  0.4439,168.0000,  5.7210,  2.2580,         
     1   9.8040,  0.4408,176.2000,  5.6750,  1.9970,
     1  14.2200,  0.3630,228.4000,  7.0240,  1.0160,
     1   9.9520,  0.4318,233.5000,  5.0650,  0.9244,
     1   9.2720,  0.4345,210.0000,  4.9110,  1.2580,
     1  10.1300,  0.4146,225.7000,  5.5250,  1.0550,
     1   8.9490,  0.4304,213.3000,  5.0710,  1.2210,
     1  11.9400,  0.3783,247.2000,  6.6550,  0.8490,
     1   8.4720,  0.4405,195.5000,  4.0510,  1.6040,
     1   8.3010,  0.4399,203.7000,  3.6670,  1.4590/
       DATA A1(71),A2(71),A3(71),A4(71),A5(71)      
     1 ,A1(72),A2(72),A3(72),A4(72),A5(72)
     1 ,A1(73),A2(73),A3(73),A4(73),A5(73)
     1 ,A1(74),A2(74),A3(74),A4(74),A5(74)
     1 ,A1(75),A2(75),A3(75),A4(75),A5(75)
     1 ,A1(76),A2(76),A3(76),A4(76),A5(76)
     1 ,A1(77),A2(77),A3(77),A4(77),A5(77)
     1 ,A1(78),A2(78),A3(78),A4(78),A5(78)
     1 ,A1(79),A2(79),A3(79),A4(79),A5(79)
     1 ,A1(80),A2(80),A3(80),A4(80),A5(80)
     1   /6.5670,  0.4858,193.0000,  2.6500,  1.6600,         
     1   5.9510,  0.5016,196.1000,  2.6620,  1.5890,
     1   7.4950,  0.4523,251.4000,  3.4330,  0.8619,
     1   6.3350,  0.4825,255.1000,  2.8340,  0.8228,
     1   4.3140,  0.5558,214.8000,  2.3540,  1.2630,
     1   4.0200,  0.5681,219.9000,  2.4020,  1.1910,
     1   3.8360,  0.5765,210.2000,  2.7420,  1.3050,
     1   4.6800,  0.5247,244.7000,  2.7490,  0.8962,
     1   3.2230,  0.5883,232.7000,  2.9540,  1.0500,
     1   2.8920,  0.6204,208.6000,  2.4150,  1.4160/
       DATA A1(81),A2(81),A3(81),A4(81),A5(81)      
     1 ,A1(82),A2(82),A3(82),A4(82),A5(82)
     1 ,A1(83),A2(83),A3(83),A4(83),A5(83)
     1 ,A1(84),A2(84),A3(84),A4(84),A5(84)
     1 ,A1(85),A2(85),A3(85),A4(85),A5(85)
     1 ,A1(86),A2(86),A3(86),A4(86),A5(86)
     1 ,A1(87),A2(87),A3(87),A4(87),A5(87)
     1 ,A1(88),A2(88),A3(88),A4(88),A5(88)
     1 ,A1(89),A2(89),A3(89),A4(89),A5(89)
     1 ,A1(90),A2(90),A3(90),A4(90),A5(90)
     1   /4.7280,  0.5522,217.0000,  3.0910,  1.3860,         
     1   6.1800,  0.5200,170.0000,  4.0000,  3.2240,
     1   9.0000,  0.4700,198.0000,  3.8000,  2.0320,
     1   2.3240,  0.6997,216.0000,  1.5990,  1.3990,
     1   1.9610,  0.7286,223.0000,  1.6210,  1.2960,
     1   1.7500,  0.7427,350.1001,  0.9789,  0.5507,
     1  10.3100,  0.4613,261.2000,  4.7380,  0.9899,
     1   7.9620,  0.5190,235.7000,  4.3470,  1.3130,
     1   6.2270,  0.5645,231.9000,  3.9610,  1.3790,
     1   5.2460,  0.5947,228.6000,  4.0270,  1.4320/
       DATA A1(91),A2(91),A3(91),A4(91),A5(91)      
     1 ,A1(92),A2(92),A3(92),A4(92),A5(92)
     1   /5.4080,  0.5811,235.7000,  3.9610,  1.3580,         
     1   5.2180,  0.5828,245.0000,  3.8380,  1.2500/
C 
C      COEFFICIENTS FOR HE IN GASEOUS MATTER, LOW ENERGIES    
C 
       DATA B1( 1),B2( 1),B3( 1),B4( 1),B5( 1)      
     1 ,B1( 2),B2( 2),B3( 2),B4( 2),B5( 2)
     1 ,B1( 3),B2( 3),B3( 3),B4( 3),B5( 3)
     1 ,B1( 4),B2( 4),B3( 4),B4( 4),B5( 4)
     1 ,B1( 5),B2( 5),B3( 5),B4( 5),B5( 5)
     1 ,B1( 6),B2( 6),B3( 6),B4( 6),B5( 6)
     1 ,B1( 7),B2( 7),B3( 7),B4( 7),B5( 7)
     1 ,B1( 8),B2( 8),B3( 8),B4( 8),B5( 8)
     1 ,B1( 9),B2( 9),B3( 9),B4( 9),B5( 9)
     1 ,B1(10),B2(10),B3(10),B4(10),B5(10)
     1   /0.39, 0.63, 4.17, 85.55, 19.55, 
     1   0.58, 0.59, 6.3, 130., 44.07,    
     1   15.23, 0.1076, 10.14, 1232., 31.24,        
     1   2.2060,  0.5100, 15.3200,  0.2500,  8.9950,
     1   3.6910,  0.4128, 18.4800, 50.7200,  9.0000,
     1   3.47,0.4485, 22.37, 36.41, 7.993,
     1   2.0, 0.548, 29.82, 18.11, 4.37,  
     1   2.717, 0.4858, 32.88, 25.88, 4.336,        
     1   2.616, 0.4708, 41.2, 28.07, 2.458,         
     1   2.303, 0.4861, 37.01, 37.96, 5.092/        
      DATA B1(11),B2(11),B3(11),B4(11),B5(11)       
     1 ,B1(12),B2(12),B3(12),B4(12),B5(12)
     1 ,B1(13),B2(13),B3(13),B4(13),B5(13)
     1 ,B1(14),B2(14),B3(14),B4(14),B5(14)
     1 ,B1(15),B2(15),B3(15),B4(15),B5(15)
     1 ,B1(16),B2(16),B3(16),B4(16),B5(16)
     1 ,B1(17),B2(17),B3(17),B4(17),B5(17)
     1 ,B1(18),B2(18),B3(18),B4(18),B5(18)
     1 ,B1(19),B2(19),B3(19),B4(19),B5(19)
     1 ,B1(20),B2(20),B3(20),B4(20),B5(20)
     1   /13.03, 0.2685, 35.65, 44.18, 9.175,       
     1   4.3000,  0.4700, 34.3000,  3.3000, 12.7400,
     1   2.5000,  0.6250, 45.7000,  0.1000,  4.3590,
     1   2.1000,  0.6500, 49.3400,  1.7880,  4.1330,
     1   1.7290,  0.6562, 53.4100,  2.4050,  4.8450,
     1   3.116, 0.5988, 53.71, 5.632, 4.536,        
     1   3.365, 0.571,  63.67, 6.182, 2.969,        
     1   2.291, 0.6284, 73.88, 4.478, 2.066,        
     1   16.6,  0.3095, 99.1,  10.98, 1.092,        
     1 6.2970,  0.4622, 65.3900, 10.1400,  5.0360/  
       DATA B1(21),B2(21),B3(21),B4(21),B5(21)      
     1 ,B1(22),B2(22),B3(22),B4(22),B5(22)
     1 ,B1(23),B2(23),B3(23),B4(23),B5(23)
     1 ,B1(24),B2(24),B3(24),B4(24),B5(24)
     1 ,B1(25),B2(25),B3(25),B4(25),B5(25)
     1 ,B1(26),B2(26),B3(26),B4(26),B5(26)
     1 ,B1(27),B2(27),B3(27),B4(27),B5(27)
     1 ,B1(28),B2(28),B3(28),B4(28),B5(28)
     1 ,B1(29),B2(29),B3(29),B4(29),B5(29)
     1 ,B1(30),B2(30),B3(30),B4(30),B5(30)
     1   /5.3070,  0.4918, 61.7400, 12.4000,  6.6650,         
     1   4.7100,  0.5087, 65.2800,  8.8060,  5.9480,
     1   6.1510,  0.4524, 83.0000, 18.3100,  2.7100,
     1   6.5700,  0.4322, 84.7600, 15.5300,  2.7790,
     1   5.7380,  0.4492, 84.6100, 14.1800,  3.1010,
     1   5.0310,  0.4707, 85.5800, 16.5500,  3.2110,
     1   4.3200,  0.4947, 76.1400, 10.8500,  5.4410,
     1   4.6520,  0.4571, 80.7300, 22.0000,  4.9520,
     1   3.1140,  0.5236, 76.6700,  7.6200,  6.3850,
     1   3.1140,  0.5236, 76.6700,  7.6200,  7.5020/
       DATA B1(31),B2(31),B3(31),B4(31),B5(31)      
     1 ,B1(32),B2(32),B3(32),B4(32),B5(32)
     1 ,B1(33),B2(33),B3(33),B4(33),B5(33)
     1 ,B1(34),B2(34),B3(34),B4(34),B5(34)
     1 ,B1(35),B2(35),B3(35),B4(35),B5(35)
     1 ,B1(36),B2(36),B3(36),B4(36),B5(36)
     1 ,B1(37),B2(37),B3(37),B4(37),B5(37)
     1 ,B1(38),B2(38),B3(38),B4(38),B5(38)
     1 ,B1(39),B2(39),B3(39),B4(39),B5(39)
     1 ,B1(40),B2(40),B3(40),B4(40),B5(40)
     1   /3.1140,  0.5236, 76.6700,  7.6200,  8.5140,         
     1   5.7460,  0.4662, 79.2400,  1.1850,  7.9930,
     1   2.7920,  0.6346,106.1000,  0.2986,  2.3310,
     1   4.6670,  0.5095,124.3000,  2.1020,  1.6670,
     1   1.65,  0.7,    148.1, 1.47,  0.9686,       
     1   1.413, 0.7377, 147.9, 1.466, 1.016,        
     1  11.7200,  0.3826,102.8000,  9.0000,  4.3710,
     1   7.1260,  0.4804,119.3000,  5.7840,  2.4540,
     1  11.6100,  0.3955,146.7000,  7.0310,  1.4230,
     1  10.9900,  0.4100,163.9000,  7.1000,  1.0520/
       DATA B1(41),B2(41),B3(41),B4(41),B5(41)      
     1 ,B1(42),B2(42),B3(42),B4(42),B5(42)
     1 ,B1(43),B2(43),B3(43),B4(43),B5(43)
     1 ,B1(44),B2(44),B3(44),B4(44),B5(44)
     1 ,B1(45),B2(45),B3(45),B4(45),B5(45)
     1 ,B1(46),B2(46),B3(46),B4(46),B5(46)
     1 ,B1(47),B2(47),B3(47),B4(47),B5(47)
     1 ,B1(48),B2(48),B3(48),B4(48),B5(48)
     1 ,B1(49),B2(49),B3(49),B4(49),B5(49)
     1 ,B1(50),B2(50),B3(50),B4(50),B5(50)
     1   /9.2410,  0.4275,163.1000,  7.9540,  1.1020,         
     1   9.2760,  0.4180,157.1000,  8.0380,  1.2900,
     1   3.9990,  0.6152, 97.6000,  1.2970,  5.7920,
     1   4.3020,  0.5658, 97.9900,  5.5140,  5.7540,
     1   3.6150,  0.6197, 86.2600,  0.3330,  8.6890,
     1   5.8000,  0.4900,147.2000,  6.9030,  1.2890,
     1   5.6000,  0.4900,130.0000, 10.0000,  2.8440,
     1   3.5500,  0.6068,124.7000,  1.1120,  3.1190,
     1   3.6000,  0.6200,105.8000,  0.1692,  6.0260,
     1   5.4000,  0.5300,103.1000,  3.9310,  7.7670/
       DATA B1(51),B2(51),B3(51),B4(51),B5(51)      
     1 ,B1(52),B2(52),B3(52),B4(52),B5(52)
     1 ,B1(53),B2(53),B3(53),B4(53),B5(53)
     1 ,B1(54),B2(54),B3(54),B4(54),B5(54)
     1 ,B1(55),B2(55),B3(55),B4(55),B5(55)
     1 ,B1(56),B2(56),B3(56),B4(56),B5(56)
     1 ,B1(57),B2(57),B3(57),B4(57),B5(57)
     1 ,B1(58),B2(58),B3(58),B4(58),B5(58)
     1 ,B1(59),B2(59),B3(59),B4(59),B5(59)
     1 ,B1(60),B2(60),B3(60),B4(60),B5(60)
     1   /3.9700,  0.6459,131.8000,  0.2233,  2.7230,         
     1   3.6500,  0.6400,126.8000,  0.6834,  3.4110,
     1   4.13,  0.6177, 152.0, 2.516, 1.938,        
     1   3.949, 0.6209, 200.5, 1.878, 0.9126,       
     1   25.94, 0.3139, 335.1, 2.946, 0.3347,       
     1  10.9900,  0.4599,138.4000,  4.8110,  3.7260,
     1  16.6000,  0.3773,224.1000,  6.2800,  0.9121,
     1  10.5400,  0.4533,159.3000,  4.8320,  2.5290,
     1  10.3300,  0.4502,162.0000,  5.1320,  2.4440,
     1  10.1500,  0.4471,165.6000,  5.3780,  2.3280/
       DATA B1(61),B2(61),B3(61),B4(61),B5(61)      
     1 ,B1(62),B2(62),B3(62),B4(62),B5(62)
     1 ,B1(63),B2(63),B3(63),B4(63),B5(63)
     1 ,B1(64),B2(64),B3(64),B4(64),B5(64)
     1 ,B1(65),B2(65),B3(65),B4(65),B5(65)
     1 ,B1(66),B2(66),B3(66),B4(66),B5(66)
     1 ,B1(67),B2(67),B3(67),B4(67),B5(67)
     1 ,B1(68),B2(68),B3(68),B4(68),B5(68)
     1 ,B1(69),B2(69),B3(69),B4(69),B5(69)
     1 ,B1(70),B2(70),B3(70),B4(70),B5(70)
     1   /9.9760,  0.4439,168.0000,  5.7210,  2.2580,         
     1   9.8040,  0.4408,176.2000,  5.6750,  1.9970,
     1  14.2200,  0.3630,228.4000,  7.0240,  1.0160,
     1   9.9520,  0.4318,233.5000,  5.0650,  0.9244,
     1   9.2720,  0.4345,210.0000,  4.9110,  1.2580,
     1  10.1300,  0.4146,225.7000,  5.5250,  1.0550,
     1   8.9490,  0.4304,213.3000,  5.0710,  1.2210,
     1  11.9400,  0.3783,247.2000,  6.6550,  0.8490,
     1   8.4720,  0.4405,195.5000,  4.0510,  1.6040,
     1   8.3010,  0.4399,203.7000,  3.6670,  1.4590/
       DATA B1(71),B2(71),B3(71),B4(71),B5(71)      
     1 ,B1(72),B2(72),B3(72),B4(72),B5(72)
     1 ,B1(73),B2(73),B3(73),B4(73),B5(73)
     1 ,B1(74),B2(74),B3(74),B4(74),B5(74)
     1 ,B1(75),B2(75),B3(75),B4(75),B5(75)
     1 ,B1(76),B2(76),B3(76),B4(76),B5(76)
     1 ,B1(77),B2(77),B3(77),B4(77),B5(77)
     1 ,B1(78),B2(78),B3(78),B4(78),B5(78)
     1 ,B1(79),B2(79),B3(79),B4(79),B5(79)
     1 ,B1(80),B2(80),B3(80),B4(80),B5(80)
     1   /6.5670,  0.4858,193.0000,  2.6500,  1.6600,         
     1   5.9510,  0.5016,196.1000,  2.6620,  1.5890,
     1   7.4950,  0.4523,251.4000,  3.4330,  0.8619,
     1   6.3350,  0.4825,255.1000,  2.8340,  0.8228,
     1   4.3140,  0.5558,214.8000,  2.3540,  1.2630,
     1   4.0200,  0.5681,219.9000,  2.4020,  1.1910,
     1   3.8360,  0.5765,210.2000,  2.7420,  1.3050,
     1   4.6800,  0.5247,244.7000,  2.7490,  0.9862,
     1   3.2230,  0.5883,232.7000,  2.9540,  1.0500,
     1   8.15, 0.4745, 269.2, 2.392, 0.7467/        
       DATA B1(81),B2(81),B3(81),B4(81),B5(81)      
     1 ,B1(82),B2(82),B3(82),B4(82),B5(82)
     1 ,B1(83),B2(83),B3(83),B4(83),B5(83)
     1 ,B1(84),B2(84),B3(84),B4(84),B5(84)
     1 ,B1(85),B2(85),B3(85),B4(85),B5(85)
     1 ,B1(86),B2(86),B3(86),B4(86),B5(86)
     1 ,B1(87),B2(87),B3(87),B4(87),B5(87)
     1 ,B1(88),B2(88),B3(88),B4(88),B5(88)
     1 ,B1(89),B2(89),B3(89),B4(89),B5(89)
     1 ,B1(90),B2(90),B3(90),B4(90),B5(90)
     1   /4.7280,  0.5522,217.0000,  3.0910,  1.3860,         
     1   6.1800,  0.5200,170.0000,  4.0000,  3.2240,
     1   9.0000,  0.4700,198.0000,  3.8000,  2.0320,
     1   2.3240,  0.6997,216.0000,  1.5990,  1.3990,
     1   1.9610,  0.7286,223.0000,  1.6210,  1.2960,
     1   4.822, 0.605, 418.3, 0.8335, 0.3865,       
     1  10.3100,  0.4613,261.2000,  4.7380,  0.9899,
     1   7.9620,  0.5190,235.7000,  4.3470,  1.3130,
     1   6.2270,  0.5645,231.9000,  3.9610,  1.3790,
     1   5.2460,  0.5947,228.6000,  4.0270,  1.4320/
       DATA B1(91),B2(91),B3(91),B4(91),B5(91)      
     1 ,B1(92),B2(92),B3(92),B4(92),B5(92)
     1   /5.4080,  0.5811,235.7000,  3.9610,  1.3580,         
     1   5.2180,  0.5828,245.0000,  3.8380,  1.2500/
C 
C      COEFFICIENTS FOR HE IN GASEOUS AND SOLID MATTER, HIGH ENERGIES   
C 
       DATA A6( 1),A7( 1),A8( 1),A9( 1)   
     1 ,A6( 2),A7( 2),A8( 2),A9( 2)       
     1 ,A6( 3),A7( 3),A8( 3),A9( 3)       
     1 ,A6( 4),A7( 4),A8( 4),A9( 4)       
     1 ,A6( 5),A7( 5),A8( 5),A9( 5)       
     1 ,A6( 6),A7( 6),A8( 6),A9( 6)       
     1 ,A6( 7),A7( 7),A8( 7),A9( 7)       
     1 ,A6( 8),A7( 8),A8( 8),A9( 8)       
     1 ,A6( 9),A7( 9),A8( 9),A9( 9)       
     1 ,A6(10),A7(10),A8(10),A9(10)       
     1  /2.371000, 0.546200,-0.079320,-0.006853,    
     1  2.809000, 0.484700,-0.087560,-0.007281,     
     1  3.095000, 0.443400,-0.092590,-0.007459,     
     1  3.280000, 0.418800,-0.095640,-0.007604,     
     1  3.426000, 0.400000,-0.097960,-0.007715,     
     1  3.588000, 0.392100,-0.099350,-0.007804,     
     1  3.759000, 0.409400,-0.096460,-0.007661,     
     1  3.782000, 0.373400,-0.101100,-0.007874,     
     1  3.816000, 0.350400,-0.104600,-0.008074,     
     1  3.863000, 0.334200,-0.107200,-0.008231/     
       DATA A6(11),A7(11),A8(11),A9(11)   
     1 ,A6(12),A7(12),A8(12),A9(12)       
     1 ,A6(13),A7(13),A8(13),A9(13)       
     1 ,A6(14),A7(14),A8(14),A9(14)       
     1 ,A6(15),A7(15),A8(15),A9(15)       
     1 ,A6(16),A7(16),A8(16),A9(16)       
     1 ,A6(17),A7(17),A8(17),A9(17)       
     1 ,A6(18),A7(18),A8(18),A9(18)       
     1 ,A6(19),A7(19),A8(19),A9(19)       
     1 ,A6(20),A7(20),A8(20),A9(20)       
     1  /3.898000, 0.319100,-0.108600,-0.008271,    
     1  3.961000, 0.314000,-0.109100,-0.008297,     
     1  4.024000, 0.311300,-0.109300,-0.008306,     
     1  4.077000, 0.307400,-0.108900,-0.008219,     
     1  4.124000, 0.302300,-0.109400,-0.008240,     
     1  4.164000, 0.296400,-0.110100,-0.008267,     
     1  4.210000, 0.293600,-0.110300,-0.008270,     
     1  4.261000, 0.299400,-0.108500,-0.008145,     
     1  4.300000, 0.290300,-0.110300,-0.008259,     
     1  4.344000, 0.289700,-0.110200,-0.008245/     
       DATA A6(21),A7(21),A8(21),A9(21)   
     1 ,A6(22),A7(22),A8(22),A9(22)       
     1 ,A6(23),A7(23),A8(23),A9(23)       
     1 ,A6(24),A7(24),A8(24),A9(24)       
     1 ,A6(25),A7(25),A8(25),A9(25)       
     1 ,A6(26),A7(26),A8(26),A9(26)       
     1 ,A6(27),A7(27),A8(27),A9(27)       
     1 ,A6(28),A7(28),A8(28),A9(28)       
     1 ,A6(29),A7(29),A8(29),A9(29)       
     1 ,A6(30),A7(30),A8(30),A9(30)       
     1  /4.327000, 0.270700,-0.112700,-0.008370,    
     1  4.340000, 0.261800,-0.113800,-0.008420,     
     1  4.361000, 0.255900,-0.114500,-0.008447,     
     1  4.349000, 0.240000,-0.116600,-0.008550,     
     1  4.362000, 0.232700,-0.117400,-0.008588,     
     1  4.375000, 0.225300,-0.118500,-0.008648,     
     1  4.362000, 0.206900,-0.121400,-0.008815,     
     1  4.346000, 0.185700,-0.124900,-0.009021,     
     1  4.355000, 0.180000,-0.125500,-0.009045,     
     1  4.389000, 0.180600,-0.125300,-0.009028/     
       DATA A6(31),A7(31),A8(31),A9(31)   
     1 ,A6(32),A7(32),A8(32),A9(32)       
     1 ,A6(33),A7(33),A8(33),A9(33)       
     1 ,A6(34),A7(34),A8(34),A9(34)       
     1 ,A6(35),A7(35),A8(35),A9(35)       
     1 ,A6(36),A7(36),A8(36),A9(36)       
     1 ,A6(37),A7(37),A8(37),A9(37)       
     1 ,A6(38),A7(38),A8(38),A9(38)       
     1 ,A6(39),A7(39),A8(39),A9(39)       
     1 ,A6(40),A7(40),A8(40),A9(40)       
     1  /4.407000, 0.175900,-0.125800,-0.009054,    
     1  4.419000, 0.169400,-0.126700,-0.009094,     
     1  4.412000, 0.154500,-0.128900,-0.009202,     
     1  4.419000, 0.144800,-0.130300,-0.009269,     
     1  4.436000, 0.144300,-0.129900,-0.009229,     
     1  4.478000, 0.160800,-0.126200,-0.008983,     
     1  4.489000, 0.151700,-0.127800,-0.009078,     
     1  4.514000, 0.155100,-0.126800,-0.009005,     
     1  4.533000, 0.156800,-0.126100,-0.008945,     
     1  4.548000, 0.157200,-0.125600,-0.008901/     
       DATA A6(41),A7(41),A8(41),A9(41)   
     1 ,A6(42),A7(42),A8(42),A9(42)       
     1 ,A6(43),A7(43),A8(43),A9(43)       
     1 ,A6(44),A7(44),A8(44),A9(44)       
     1 ,A6(45),A7(45),A8(45),A9(45)       
     1 ,A6(46),A7(46),A8(46),A9(46)       
     1 ,A6(47),A7(47),A8(47),A9(47)       
     1 ,A6(48),A7(48),A8(48),A9(48)       
     1 ,A6(49),A7(49),A8(49),A9(49)       
     1 ,A6(50),A7(50),A8(50),A9(50)       
     1  /4.553000, 0.154400,-0.125500,-0.008883,    
     1  4.548000, 0.148500,-0.125900,-0.008889,     
     1  4.489000, 0.112800,-0.130900,-0.009107,     
     1  4.402000, 0.066560,-0.137500,-0.009421,     
     1  4.292000, 0.010120,-0.145900,-0.009835,     
     1  4.187000,-0.045390,-0.154200,-0.010250,     
     1  4.577000, 0.130000,-0.128500,-0.009067,     
     1  4.583000, 0.125300,-0.129100,-0.009084,     
     1  4.580000, 0.117400,-0.130100,-0.009129,     
     1  4.581000, 0.111000,-0.130900,-0.009161/     
       DATA A6(51),A7(51),A8(51),A9(51)   
     1 ,A6(52),A7(52),A8(52),A9(52)       
     1 ,A6(53),A7(53),A8(53),A9(53)       
     1 ,A6(54),A7(54),A8(54),A9(54)       
     1 ,A6(55),A7(55),A8(55),A9(55)       
     1 ,A6(56),A7(56),A8(56),A9(56)       
     1 ,A6(57),A7(57),A8(57),A9(57)       
     1 ,A6(58),A7(58),A8(58),A9(58)       
     1 ,A6(59),A7(59),A8(59),A9(59)       
     1 ,A6(60),A7(60),A8(60),A9(60)       
     1  /4.582000, 0.104600,-0.131700,-0.009193,    
     1  4.600000, 0.105200,-0.131500,-0.009178,     
     1  4.614000, 0.104300,-0.131500,-0.009175,     
     1  4.619000, 0.097690,-0.132500,-0.009231,     
     1  4.671000, 0.113600,-0.129800,-0.009078,     
     1  4.706000, 0.120600,-0.128700,-0.009009,     
     1  4.732000, 0.124400,-0.128000,-0.008968,     
     1  4.722000, 0.115600,-0.129200,-0.009030,     
     1  4.710000, 0.106000,-0.130500,-0.009100,     
     1  4.698000, 0.096470,-0.131900,-0.009169/     
       DATA A6(61),A7(61),A8(61),A9(61)   
     1 ,A6(62),A7(62),A8(62),A9(62)       
     1 ,A6(63),A7(63),A8(63),A9(63)       
     1 ,A6(64),A7(64),A8(64),A9(64)       
     1 ,A6(65),A7(65),A8(65),A9(65)       
     1 ,A6(66),A7(66),A8(66),A9(66)       
     1 ,A6(67),A7(67),A8(67),A9(67)       
     1 ,A6(68),A7(68),A8(68),A9(68)       
     1 ,A6(69),A7(69),A8(69),A9(69)       
     1 ,A6(70),A7(70),A8(70),A9(70)       
     1  /4.681000, 0.085360,-0.133500,-0.009252,    
     1  4.676000, 0.078190,-0.134500,-0.009302,     
     1  4.663000, 0.068670,-0.135800,-0.009373,     
     1  4.676000, 0.068610,-0.135700,-0.009363,     
     1  4.649000, 0.053620,-0.137900,-0.009480,     
     1  4.634000, 0.043350,-0.139400,-0.009558,     
     1  4.603000, 0.026790,-0.141800,-0.009690,     
     1  4.584000, 0.014940,-0.143600,-0.009783,     
     1  4.576000, 0.007043,-0.144700,-0.009841,     
     1  4.571000, 0.000705,-0.145600,-0.009886/     
       DATA A6(71),A7(71),A8(71),A9(71)   
     1 ,A6(72),A7(72),A8(72),A9(72)       
     1 ,A6(73),A7(73),A8(73),A9(73)       
     1 ,A6(74),A7(74),A8(74),A9(74)       
     1 ,A6(75),A7(75),A8(75),A9(75)       
     1 ,A6(76),A7(76),A8(76),A9(76)       
     1 ,A6(77),A7(77),A8(77),A9(77)       
     1 ,A6(78),A7(78),A8(78),A9(78)       
     1 ,A6(79),A7(79),A8(79),A9(79)       
     1 ,A6(80),A7(80),A8(80),A9(80)       
     1  /4.566000,-0.005626,-0.146400,-0.009930,    
     1  4.561000,-0.011970,-0.147300,-0.009975,     
     1  4.572000,-0.012000,-0.147200,-0.009965,     
     1  4.569000,-0.017550,-0.148000,-0.010000,     
     1  4.573000,-0.019920,-0.148200,-0.010010,     
     1  4.570000,-0.025470,-0.149000,-0.010050,     
     1  4.528000,-0.046130,-0.152100,-0.010220,     
     1  4.494000,-0.063700,-0.154800,-0.010370,     
     1  4.564000,-0.027000,-0.147100,-0.009852,     
     1  4.546000,-0.049630,-0.152300,-0.010220/     
       DATA A6(81),A7(81),A8(81),A9(81)   
     1 ,A6(82),A7(82),A8(82),A9(82)       
     1 ,A6(83),A7(83),A8(83),A9(83)       
     1 ,A6(84),A7(84),A8(84),A9(84)       
     1 ,A6(85),A7(85),A8(85),A9(85)       
     1 ,A6(86),A7(86),A8(86),A9(86)       
     1 ,A6(87),A7(87),A8(87),A9(87)       
     1 ,A6(88),A7(88),A8(88),A9(88)       
     1 ,A6(89),A7(89),A8(89),A9(89)       
     1 ,A6(90),A7(90),A8(90),A9(90)       
     1  /4.594000,-0.033390,-0.149600,-0.010060,    
     1  4.608000,-0.028860,-0.148500,-0.009990,     
     1  4.624000,-0.026390,-0.148100,-0.009971,     
     1  4.636000,-0.024220,-0.147700,-0.009939,     
     1  4.648000,-0.021720,-0.147100,-0.009903,     
     1  4.662000,-0.119200,-0.175200,-0.011960,     
     1  4.690000,-0.009867,-0.144900,-0.009771,     
     1  4.715000,-0.002113,-0.143500,-0.009689,     
     1  4.729000, 0.001392,-0.142800,-0.009644,     
     1  4.729000,-0.000598,-0.143000,-0.009647/     
       DATA A6(91),A7(91),A8(91),A9(91)   
     1 ,A6(92),A7(92),A8(92),A9(92)       
     1  /4.738000, 0.001075,-0.142500,-0.009618,    
     1  4.751000, 0.004244,-0.141900,-0.009576/     
       END  
      SUBROUTINE SHROT  ( NO, NP, T, TP ,NUM )                          
C****                                                                   
C****                                                                   
C**** SUBROUTINE DOES TRANSLATIONS FIRST ALONG AXES X, Y, Z IN ORDER,   
C**** FOLLOWED BY ROTATIONS ABOUT X, Y, Z   .                           
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
c      REAL*4 DAET, TYME                                  !JDL 31-OCT-84

      include 'rtcomm0.f'
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       
C*JDL DIMENSION DATA(  75,30 ), ITITLE(30)               !JDL 17-NOV-83 
C**** DATA  C/ 3.D10/                                                   
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 1
      CALL PLT2 ( NUM, NO, NBR, TPAR )
      X0 = DATA( 1,NO )                                                 
      Y0 = DATA( 2,NO )                                                 
      Z0 = DATA( 3,NO )                                                 
      CX = DCOS( DATA(4,NO)/57.29578 )                                  
      SX = DSIN( DATA(4,NO)/57.29578 )                                  
      CY = DCOS( DATA(5,NO)/57.29578 )                                  
      SY = DSIN( DATA(5,NO)/57.29578 )                                  
      CZ = DCOS( DATA(6,NO)/57.29578 )                                  
      SZ = DSIN( DATA(6,NO)/57.29578 )                                  
  100 FORMAT( / '*** TRANSLATE-ROTATE  ****  ', A4,'  **************'// 
     1'      T CM', 18X, 'X CM', 7X, 'Y CM', 7X, 'Z CM' , '      VELZ/C'
     2   , '    THETA MR      PHI MR'  /  )                             
  101 FORMAT( '  TRANSLATE  ' )                                         
  102 FORMAT( '  ROTATE  '  )                                           
  103 FORMAT( F10.4, 11X, 3F11.3, F12.5, 2F12.3  )                      
      IF( NP  .LE. 100) PRINT 100,ITITLE(NO)                            
      VXP = 1000. *DATAN2( VXA,VZA )                                    
      VYP = 1000. *DASIN ( VYA/VEL )                                    
      VZP =  VZA  / VEL                                                 
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        
      IF( (X0 .EQ. 0.) .AND. (Y0 .EQ. 0.) .AND. (Z0 .EQ. 0.) ) GO TO 1  
      IF( NP  .LE. 100) PRINT 101                                       
      XA = XA-X0                                                        
      YA = YA-Y0                                                        
      ZA = ZA-Z0                                                        
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        
    1 IF( DATA( 4,NO ) .EQ. 0. ) GO TO 2                                
      IF( NP  .LE. 100) PRINT 102                                       
      YR =  YA*CX +  ZA*SX                                              
      ZR = -YA*SX +  ZA*CX                                              
      VYR= VYA*CX + VZA*SX                                              
      VZR=-VYA*SX + VZA*CX                                              
      YA = YR                                                           
      ZA = ZR                                                           
      VYA = VYR                                                         
      VZA = VZR                                                         
      VXP = 1000. *DATAN2( VXA,VZA )                                    
      VYP = 1000. *DASIN ( VYA/VEL )                                    
      VZP =  VZA  / VEL                                                 
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        
    2 IF( DATA( 5,NO ) .EQ. 0. ) GO TO 3                                
      IF( NP  .LE. 100) PRINT 102                                       
      XR = -ZA*SY +  XA*CY                                              
      ZR =  ZA*CY +  XA*SY                                              
      VXR=-VZA*SY + VXA*CY                                              
      VZR= VZA*CY + VXA*SY                                              
      XA = XR                                                           
      ZA = ZR                                                           
      VXA = VXR                                                         
      VZA = VZR                                                         
      VXP = 1000. *DATAN2( VXA,VZA )                                    
      VYP = 1000. *DASIN ( VYA/VEL )                                    
      VZP =  VZA  / VEL                                                 
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        
    3 IF( DATA( 6,NO ) .EQ. 0. ) GOTO 4				!bsk    
      IF( NP  .LE. 100) PRINT 102                                       
      XR =  XA*CZ +  YA*SZ                                              
      YR = -XA*SZ +  YA*CZ                                              
      VXR= VXA*CZ + VYA*SZ                                              
      VYR=-VXA*SZ + VYA*CZ                                              
      XA = XR                                                           
      YA = YR                                                           
      VXA = VXR                                                         
      VYA = VYR                                                         
      VXP = 1000. *DATAN2( VXA,VZA )                                    
      VYP = 1000. *DASIN ( VYA/VEL )                                    
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        
    4 NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT2 ( NUM, NO, NBR, TPAR )
      RETURN                                                            
      END                                                               
      subroutine sigcap       
      IMPLICIT REAL*8(A-H,O-Z)                                          
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK61/  DEDX,ALPHAK,TOLD2,DEDXQ
      COMMON  /BLCK62/  GASENE,GASVEL,TOLD,GASL
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt
c      include 'rtcomm65.f'
      COMMON  /BLCK66/  tfwopt,taufct,alffct,TBAR,THWHM,tau0
      common  /blck70/  vel0,en0,pm0
c-ddc
      integer*8 icalc

      common  /blck71/  gasopt,zgas125,zgas18,dreldee,enold,icalc
      dimension dlsc(120),dq(120)
      data aa/4.8/,beta/0.037/,bb/2.2/,gama/2.44d-5/,cc/2.6/
      data ipr/0/,ipr1/0/,ipr2/0/
      data scoef/1.1d-8/
c
c**** calculate electron capture cross sections using scaling law
c     see A.S. Schlachter et al. Phys. Rev. A 27(1983)3372
c
      epamu = 1000.*energy/pmass
      etil = epamu/zgas125
      etilb = etil/(qbar**0.7)
c
c     Check if etilb is in valid energy range >10 and <1000
      if (etilb.ge.10..and.etilb.le.1000.) go to 10
c
      if (ipr.le.0) print 1001, etilb
1001  format(//' ********************WARNING**********************'/,
     *         ' ***Etil out of range for cross section scaling***'/,
     2         '    Etil(qbar) = ',1F10.3)
      ipr = 1
      if (etilb.lt.1000.) go to 5
      print 1002
1002  format(//' Energy too high-no cross section available '/)
      stop ' Energy too high-no cross section available'     
5     if ( icalc.le.0) go to 7
      if (ipr1.le.0.) print 1003, etilb,gasene
1003  format (//' *** cross sections kept constant below Etil = ',
     2           F12.3,' energy = ',F10.3,' MeV',/)
      ipr1 = 1
      return
7     if (ipr2.le.0) print 1004, etilb
      ipr2 = 1
1004  format (//' Energy Etil = ',F12.3,
     a  ' too low - no cross section available '/
     a       ,  ' Etil = 10. kept constant'/)
      etilb = 10.
      etil = etilb * qbar**0.7
10    continue
c
      bEb = beta*etilb**bb
      gEc = gama*etilb**cc
      bterm = 0.
      ibterm = 0
      cterm = 0.
      icterm = 0
      if (bEb.gt.10.) go to 20
      ibterm = 1
      bterm = bb*bEb/(dexp(bEb)-1.)
20    continue      
      if (gEc.gt.10.) go to 30
      icterm = 1
      cterm = cc*gEc/(dexp(gEc)-1.)
30    continue     
      dreldee = -aa + bterm + cterm
c
      icalc = 1
c
c     find range of q-states : from int(qbar-4) to int(qbar+4)
c     and check for validity of range
c
      qlo = qbar-4.
      qhi = qbar+4.
      if (qlo.lt.1.) qlo = 1.
      if (qhi.gt.zion-1.) qhi = zion - 1.
      iqlo = nint(qlo)
      iqhi = nint(qhi)
      nq = iqhi - iqlo + 1
      if (nq.ge.3) go to 100
      print 1000, iqlo,iqhi,qbar
1000  format(//'   *** Q-range too small to fit cross sections :'/
     2         '       qlo,qhi= ',2(i3,','),' qbar = ',1f7.3//)
      if (nq.ge.2) go to 100
      stop 666
100   do 200 iq = iqlo,iqhi
      q = dble(iq)
      etilq = etil/(q**0.7)
      expb = 0.
      expc = 0.
      if (ibterm.eq.1)    expb = dexp(-beta*(etilq**bb))
      if (icterm.eq.1)    expc = dexp(-gama*(etilq**cc))
      sctil = scoef*(etilq**(-aa))
     2             *(1.-expb)
     3             *(1.-expc)
      scapq = sctil*dsqrt(q)/zgas18
      dlsc(iq) = dlog(scapq)
      dq(iq) = q - qbar
200   continue
c
c     Fit scap(q) to exponential function :
c     scap = gassig*exp(acapt*(q-qbar))
c     and feed gassig and acapt to main program via blck 60 and 65
c
      sumx = 0.
      sumy = 0.
      sumx2 = 0.
      sumxy = 0. 
      do 300 iq = iqlo,iqhi
      sumx = sumx + dq(iq)
      sumy = sumy + dlsc(iq)
      sumx2 = sumx2 + dq(iq)*dq(iq)
      sumxy = sumxy + dq(iq)*dlsc(iq)
300   continue
      del = nq*sumx2 - sumx*sumx
      dlsig = (sumx2*sumy - sumx*sumxy)/del
      acapt = (nq*sumxy - sumx*sumy)/del
      sig = dexp(dlsig)
      gassig = gas*sig*1.d16
      return
      end
      subroutine smangsc
      IMPLICIT REAL*8(A-H,O-Z)                                          
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt
c      include 'rtcomm65.f'
      COMMON  /BLCK66/  tfwopt,taufct,alffct,TBAR,THWHM,tau0
      data ipr/0/
c
c****
c     Calculates small-angle scattering according to
c     Meyer-Sigmund-Winterbon theory, NIM 119(1974)541.
c     Valid in range tau>0.01, tau<1000.
c     For tau<0.01, issues warning and sets thwhm = 0.
c     For tau>1000. issues fatal error and stops.
c
c
      tau = taufct/sigt
c
      if (tau.gt.0.01) go to 10
      if (ipr.gt.0) go to 5
      print 1000, tau
1000  format (//10X,'****WARNING****'/
     3          10X,'  tau(small angle) =',F12.5,/)
      ipr = 1
5     tau0 = tau0 + tau
      if (tau0.gt.0.01) go to 7
      thwhm = 0.
      return
7     tau = tau0
      tau0 = 0.
      go to 20
10    if (tau.lt.10000.) go to 20
      print 2000,tau
2000  format (//10X,'****STOP CALCULATION****'/
     2          10X,'  tau(small angle) =',F12.5,/
     3          10X,'****    TOO LARGE   ****')
      stop 777
20    continue
      fn = (dlog(1.03+tau))**(-0.115) - 0.115
      thwhm = alffct*(tau**fn)/energy
c
      return
      end
                   
      SUBROUTINE SOLND  ( NO, NP, T, TP ,NUM )                          
C****                                                                   
C****                                                                   
C**** SOLENOID      RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIAL
C**** EQUATIONS OF MOTION.                                              
C     T = TIME                                                          
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            
C**** BF (POSITIVE) : SOLENOID FIELD IN BEAM DIRECTION                  
C**** CBF - USED IN BSOL TO DISTINGUISH BETWEEN COORD. SYSTEMS          
C****                                                                   
C****                                                                   
      IMPLICIT REAL*8(A-H,O-Z)                                          
c      REAL*4 DAET, TYME                                  !JDL 31-OCT-84
      REAL*8  LF, L, BX, BY, BZ, K, TC, DTC
      include 'rtcomm0.f'
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       
      COMMON  /BLCK 7/ NCODE                                            
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          
      COMMON  /BLCK30/  BF ,      AL, RAD                               
      COMMON  /BLCK31/  S, BT                                           
      COMMON  /BLCK32/  IN                                              
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
C*JDL DIMENSION DATA(  75,30 ), ITITLE(30)               !JDL 17-NOV-83 
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             
      EXTERNAL  BSOL                                                    
C**** DATA  C/ 3.D10/                                                   
C****                                                                   
C****                                                                   
      JRAYGAS = JRAY*GAS
      LF   = DATA(  1,NO )                                              
      A    = DATA( 10,NO )                                              
      B    = DATA( 11,NO )                                              
      L    = DATA( 12,NO )                                              
      D    = DATA( 13,NO )                                              
      BF   = DATA( 14,NO )                                              
      Z11  = DATA( 15,NO )                                              
      Z22  = DATA( 16,NO )                                              
      DTF1= LF/VEL                                                      
      AL  = L/2.                                                        
      RAD = D/2.                                                        
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      S = 0.                                                            
C****                                                                   
C****                                                                   
      IF( NP  .GT. 100 ) GO TO 5                                        
  201 FORMAT(  ' SOLENOID    ****  ', A4, '  ***********************'/) 
      PRINT 201, ITITLE(NO)                                             
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HBX, 8X, 4HY CM , 7X, 2HBY,
     1   8X, 4HZ CM, 7X, 2HBZ, 8X, 6HVELZ/C , 6X, 8HTHETA MR , 5X,      
     2   6HPHI MR , 6X, 1HB             )                               
      CALL PRNT2 ( T,S,XA   ,YA   ,ZA   ,BX,BY,BZ,BT,VXA  ,VYA  ,VZA   )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      PRINT 101                                                         
      PRINT 103                                                         
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO CENTERED AXIS SYSTEM ' ) 
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.         
C****                                                                   
    5 TC(1) =  XA                                                       
      TC(2) = YA                                                        
      TC(3) = ZA-A-AL                                                   
      TC(4) =  VXA                                                      
      TC(5) = VYA                                                       
      TC(6) =  VZA                                                      
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** TRANSLATE PARTICLE TO START OF FIRST FRINGE FIELD                 
C****                                                                   
      TDT = (-TC(3) -Z11 -AL ) /DABS( TC(6) )                           
C****                                                                   
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 104                                       
  104 FORMAT( 22H0FRINGING FIELD REGION    )                            
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BSOL , 0    )            
      NSTEP = 0                                                         
    6 CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      DO 7 I = 1, NP                                                    
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BSOL , 1    )            
      NSTEP = NSTEP + 1                                                 
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      IF( (Z22+AL) .LE. TC(3) ) GO TO 8                                 
    7 CONTINUE                                                          
      GO TO 6                                                           
    8 CONTINUE                                                          
      XDTF1 =-( TC(3) -(Z22+AL)  ) / DABS( TC(6) )                      
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,BSOL ,  0    )            
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,BSOL ,  1    )            
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                
  105 FORMAT( 10H   NSTEPS=  I5 )                                       
C****                                                                   
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 
C****                                                                   
      TC(3) = TC(3) - B - AL                                            
      IF( NP  .LE. 100) PRINT 109                                       
      CALL PRNT2 ( T,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )
      IF (JRAYGAS.NE.0) CALL PRNT2A
C****                                                                   
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           
C****                                                                   
      TDT = -TC(3) /DABS( TC(6) )                                       
      TC(1) = TC(1) + TDT * TC(4)                                       
      TC(2) = TC(2) + TDT * TC(5)                                       
      TC(3) = TC(3) + TDT * TC(6)                                       
      T = T + TDT                                                       
      TP = T * VEL                                                      
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      BT = 0.                                                           
      S  = 0.                                                           
      VXF    = 1000. *DATAN2( TC(4), TC(6)  )                           
      VYF    = 1000. *DASIN ( TC(5)/ VEL    )                           
      VZF    = TC(6) / VEL                                              
      IF( NP  .LE. 100) PRINT 115,TP,TC(1),TC(2),TC(3),VZF,VXF,VYF      
  115 FORMAT( F10.4, 10X, F10.3, 11X, F10.3, 11X, F10.3, 11X,           
     1   F13.5, F13.2, F11.2                   )                        
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** CALCULATE INTERCEPTS IN SYSTEM D                                  
C****                                                                   
      Z0X = -TC(1)/ ( TC(4) / TC(6)    + 1.E-10 )                       
      Z0Y = -TC(2)/ ( TC(5) / TC(6)    + 1.E-10 )                       
      IF( NP  .LE. 100) PRINT 111, VXF, VYF, Z0X, Z0Y                   
  111 FORMAT( / ' INTERSECTIONS WITH VER. AND HOR. PLANES '          ,  
     X       /15X, 5H  XP=F10.4, 10H MR    YP= F10.4, 3H MR   /         
     1        15X, 5H Z0X=F10.2, 10H CM   Z0Y= F10.2, 3H CM   /        )
      RETURN                                                            
99      CALL PRNT4(NO, IN )                                             
        RETURN                                                          
      END                                                               
      SUBROUTINE SQDIST
c
c     charge distribution in solid target
c
c     Author: B. Schneck (ANL, 87-09-11)
c
c     Version A: Betz Formula
c     Form: H. D. Betz, Heavy Ion Charge States
c           in: Applied Atomic Collision Physics, Vol. 4 (Acad. Press, 1
c
c     Version B: Sawyer Formula (87-09-15)
c     From: R. O. Sayer, Semi-Empirical Formulas for Heavy-Ion Stripping
c           Revue de Physique Appliquee 12(1977) 1543
c
c     This routine calculates the charge state distribution of heavy ion
c     in a solid target.  The optimal solution would be to have a new 
c     type of beam-line element 'STAR' with the associated parameter inp
c     which also introduces energy straggling and scattering.
c     Also only average distributions are calculated. This is not an 
c     accurete description for thin foils.
c     But for the moment this quick solution will be enough.
c
c     (Version A)
c     This routine is quite similar to the QDIST routine for charge dist
c     in gas except for different parameter settings.
c
c     (Version B)
c     The Sayer formula is more acureat than the Bets and also introduce
c     a certain assymmetry in the charge distribution.
c
c     Global variables:
c     VEL	Velocity of ion
c     ZION	Nucleus charge of ion
c     QBAR	Average charge state
c     DELSQR	half width of charge state distribution
c
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,  !***MP 1
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG   
      COMMON  /BLCK65/ QBAR,DELSQR,ACAPT,ALOS,
     $     NSK1,NSK2,SIGC,SIGT,ATBCC,qopt
c      include 'rtcomm65.f'
c
c***** Betz *****
c      DATA V0/2.189D8/, PI/3.14159/
c      DATA C11/0.936/, C12/1.686/, A1/0.864/, DEL1/0.851/, GAM1/0.437/
c      DATA C2 /1.041/,             A2/0.851/, DEL2/0.847/, GAM2/0.432/
c
c      data da/1.41/, dk/0.11/, da/0.3/, db/0.37/
c
c      VBV0 = VEL/V0
c      if (zion.gt.15) goto 100
c      TMP = -A1*(VBV0**DEL1)/(ZION**GAM1)
c      QBAR = ZION*(1.-(c11+c12/zion)*DEXP(TMP))
c      goto 200
c  100 tmp = -a2*(vbv0**del2)/(zion**gam2)
c      qbar = zion*(1.-c2*dexp(tmp))
c  200 tmp = da * (zion**dk) * (qbar**da) * ((1- qbar/zion)**db)
c      delsqr= tmp*tmp
c
c***** Sayer *****
c
        common /sqdiscm/ sqdminq, sqdp(21)
	DIMENSION Q(21),P(21)
        data c/2.998D10/
c5	CALL INKBI(' Z(INTEGER)''   ',IZ)
c	CALL INKBI(' MASS(INTEGER)''   ',IM)
c10	CALL INKBF(' ENERGY IN MEV(FLOATING(<0 FOR NEW Z/M)''   ',E)
c	IF(E.LT.0) GO TO 5
C	CALCULATE VELOCITY
c	BETA=SQRT(2*E/(FLOAT(IM)*931.478))
c	Z=FLOAT(IZ)
C	TYPE 903,Z
C903	FORMAT(F)
C	CALCULATE MAXIMUM CHARGE STATE
        beta=vel/c
        z=zion
	Qbar=Z*(1-1.03*EXP(-47.3*(Z**(-0.38))*(BETA**0.86)))
	QOZ=Qbar/Z
	RHO=0.48*(Z**0.45)*(QOZ*(1-QOZ))**0.26
	EOR=0.0007*Z-0.7*BETA
C	ROUND Qbar
	IZM=INT(Qbar)
C	CALCULATE IZM+- 10 CHARGE UNITS
	I=IZM-10
	IF(I.LT.1.) I=1
        sqdminq=float(i)
	M=1
        sum=0
100	ARG=(I-Qbar)**2/(2*RHO*RHO*(1+EOR*(I-Qbar)))
	sqdP(M)=EXP(-ARG)
        sum=sum+sqdp(m)
	M=M+1
	I=I+1
	IF(M.GT.21) GO TO 150
c	IF(float(I).LT.z) GO TO 100   ! commented out 90-04-07
        if(float(i).le.z) go to 100   ! corrected     90-04-07 to allow 
C	NORMALIZE TO INTEGRAL OVER Q=1
150	continue
	sqdp(1)=sqdp(1)/sum
 	DO 300 K=2,21
	sqdP(K)=sqdp(k-1)+sqdP(k)/sum
c	TYPE 900,sqsminq+k-1.,sqdP(K)
300	CONTINUE
c900	FORMAT(' Q=',F8.4,'   P=',F8.4,' ')
c	TYPE 901,Qbar,RHO,EOR
c901	FORMAT(' MOST PROBABLE=',F8.3,' RHO=',F8.3,' EOR=',F8.3,' ')
c	GO TO 10
c	END
c
        delsqr=rho*rho
      RETURN
      END
      SUBROUTINE SQSAMP
c
c     sample a charge state in solid target
c
c     Author: B. Schneck (ANL, 87-09-15)
c
c     This routine is called by RTMAIN to sample an initial charge state
c     for a ray.  The parameters are set up according to the Sayer formu
c     before by a call to SQDIST.
c
c     Global variables:
c     iseed	random generator
c     sqdp(*)	probabilities for charge state
c     sqdminq	charge state corresponding to sqdp(1)
c     zion	highest returned charge state
c     q0	returned charge state
c
      IMPLICIT REAL*8(A-H,O-Z)
c
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0
      COMMON  /BLCK60/  GAS,AGAS,ZGAS,ZION,PRESS,GASSIG,QAVER,
     1                  QFWHM,RHOGAS,GASK,EMASS,GASMFP,JRAY,Q00,NPASG
      common  /BLCK63/ iseed
c
      common /sqdiscm/ sqdminq, sqdp(21)
      r=ran(0)
      q0=sqdminq
      i=1
   10 if (r.lt.sqdp(i).or.(i.ge.21).or.(q0.ge.zion)) return
      i=i+1
      if (i.gt.21) return
      q0=q0+1.
      goto 10
      END
      FUNCTION STOP(Z1,AA1,Z2,E)                                       
C 
C      CALCULATES:    
C      - NUCLEAR STOPPING POWERS
C      - ELECTRONIC STOPPING POWERS FOR HE IN       
C        ALL ELEMENTAL SOLID AND GASEOUS MATTER     
C      - EFFECTIVE CHARGES TO CONVERT HE  
C        STOPPING POWERS TO ALL OTHERS    
C        (SEE J. F. ZIEGLER)    
C 
C      INPUT:         
C      - Z1, AA1 = Z AND A OF PROJECTILE  
C      - Z2, AA2 = Z AND A OF STOPPING MATERIAL     
C      - E  = LABORATORY ENERGY OF PROJECTILE IN MEV
C      - IM = SOLID STOPPING MATTER: SET IM=1,      
C   GASEOUS STOPPING MATTER: SET IM=0     
C 
C      OUTPUT:        
C      - STOP: STOPPING POWERS IN MEV/MG/CM**2      
C      - COMMON RANGEC: VOVC = VELOCITY/C 
C        V1OVC = MAXIMUM VELOCITY FOR LINDHARDT-SCHARFF-APPROXIMATION   
C        EPS = REDUCED VELOCITY 
C        DEDXE = ELECTRONIC PART OF STOPPING POWER  
C        DEDXN = NUCLEAR PART OF STOPPING POWER     
C 
C******************************************************       
C 
      IMPLICIT REAL*8(A-H,O-Z)
 
       DIMENSION SC(92),AH1(92),AH2(92),AH3(92),AH4(92),      
     1 AH5(92),AH6(92),AH7(92)  
       DIMENSION A1(92),A2(92),A3(92),A4(92),A5(92),A6(92),A7(92),      
     1 A8(92),A9(92)  
       DIMENSION B1(92),B2(92),B3(92),B4(92),B5(92),B6(92),B7(92),      
     1 B8(92),B9(92)  
       COMMON /RANGEC/VOVC,V1OVC,EPS,DEDXE,DEDXN    
       COMMON /DATAD/ AH1,AH2,AH3,AH4,AH5,AH6,AH7,  
     1 A1,A2,A3,A4,A5,A6,A7,A8,A9,        
     1 B1,B2,B3,B4,B5,B6,B7,B8,B9         
       COMMON /SHELLC/ SC       
       DATA ZERO/1.E-30/        
C 
       IM=0.                                                            
       AA2=2.*Z2                                                        
C
       IF(Z1.LE.0..OR.AA1.LE.0..OR.Z2.LE.0..OR.AA2.LE.0..OR.E.LT.ZERO)  
     1 GOTO 400       
       IZ1=IRND(Z1)   
       K=IRND(Z2)     
       IF(K.GT.92) K=92         
       EX=2./3.       
       VSQ=E/AA1      
       VOVC=0.04634*SQRT(VSQ)   
       IF(VOVC.GT.1.) VOVC=0.999999       
       V1OVC=0.0073*Z1**EX      
       FAC=0.60225/AA2
C      FAC CONVERTS UNITS FROM EV/10**15ATOMS/CM**2 TO MEV/MG/CM**2     
C 
C      NUCLEAR STOPPING POWERS  
C 
       A=AA1+AA2      
       Z=SQRT(Z1**EX+Z2**EX)    
       EPS=E*32530.*AA2/Z/Z1/Z2/A                                       
       IF(EPS.LT.0.01) GOTO 100 
       IF(EPS.GT.10) GOTO 110   
       EU=EXP(1.)     
       DEDXN=1.7*SQRT(EPS)*DLOG(EPS+EU)/(1.+6.8*EPS+3.4*EPS**1.5)       
       GOTO 120       
 100   DEDXN=1.593*SQRT(EPS)    
       GOTO 120       
 110   DEDXN=0.5*DLOG(0.47*EPS)/EPS       
 120   DEDXN=8.462*Z1*Z2*AA1/A/Z*FAC*DEDXN
C 
C      MASTER ELECTRONIC STOPPING POWERS FOR HYDROGEN         
C 
       IF(IZ1.EQ.2) GOTO 300    
       EH=1000.*VSQ   
       IF(EH.GE.1000.) GOTO 200 
       IF(EH.LE.10.) GOTO 210   
       DEDXE=FAC/(1./AH2(K)/EH**0.45+     
     1 EH/AH3(K)/DLOG(1.+AH4(K)/EH+AH5(K)*EH))      
       GOTO 220       
 200   BSQ=VOVC*VOVC  
       DEDXE=FAC*AH6(K)/BSQ*DLOG(AH7(K)*BSQ/(1.-BSQ))+SC(K)/EH
       GOTO 220       
 210   DEDXE=FAC*AH1(K)*SQRT(EH)
 220   STOP=DEDXE+DEDXN         
       IF(IZ1.EQ.1) RETURN      
       IF(IZ1.EQ.3) GOTO 230    
C 
C      ZIEGLERS EFFECTIVE Z'S AND ELECTRONIC STOPPING         
C      POWERS FOR PROJECTILES WITH Z>3    
C 
       B=0.1772*SQRT(EH)/Z1**EX 
       G=Z1*(1.-(1.034-0.1777/EXP(0.08114*Z1))      
     1 /EXP(B+0.0378*SIN(1.5708*B)))      
       IF(G.LT.1.) G=1.         
       DEDXE=DEDXE*G*G
       STOP=DEDXE+DEDXN         
       RETURN         
C 
C      EFFECTIVE Z AND ELECTRONIC STOPPING POWER FOR LITHIUM  
C 
 230   G=3. 
       HEXP=0.7138+0.002797*EH+1.348E-6*EH*EH       
       IF(HEXP.LT.60.) G=G-3./EXP(HEXP)   
       DEDXE=G*G*DEDXE
       STOP=DEDXE+DEDXN         
       RETURN         
C 
C      MASTER ELECTRONIC STOPPING POWERS FOR HELIUM 
C 
 300   EHE=4.*VSQ     
C      SOLID AND GASEOUS MATTER, HIGH ENERGIES      
       IF(EHE.LT.10.) GOTO 310  
       E1=DLOG(1./EHE)
       DEDXE=FAC*EXP(A6(K)+(A7(K)+(A8(K)+A9(K)*E1)*E1)*E1)    
       STOP=DEDXE+DEDXN         
       RETURN         
 310   IF(IM.LT.1) GOTO 320     
C      SOLID MATTER, LOW ENERGIES         
       E1=(1000.*EHE)**A2(K)    
       DEDXE=FAC/(1./E1/A1(K)+EHE/A3(K)/DLOG(1.+A4(K)/EHE+A5(K)*EHE))   
       STOP=DEDXE+DEDXN         
       RETURN         
C      GASEOUS MATTER, LOW ENERGIES       
 320   E1=(1000.*EHE)**B2(K)    
       DEDXE=FAC/(1./E1/B1(K)+EHE/B3(K)/DLOG(1.+B4(K)/EHE+B5(K)*EHE))   
       STOP=DEDXE+DEDXN         
       RETURN         
C 
 400   DEDXE=ZERO     
       DEDXN=ZERO     
       STOP=ZERO      
       RETURN         
       END  
C 
C******************************************         
C 
       FUNCTION STOPS(Z1,AA1,Z2,AA2,E)                    !MP 29-jul-93
C 
C      CALCULATES:    
C      - NUCLEAR STOPPING POWERS
C      - ELECTRONIC STOPPING POWERS FOR HE IN       
C        ALL ELEMENTAL SOLID AND GASEOUS MATTER     
C      - EFFECTIVE CHARGES TO CONVERT HE  
C        STOPPING POWERS TO ALL OTHERS    
C        (SEE J. F. ZIEGLER)    
C 
C      INPUT:         
C      - Z1, AA1 = Z AND A OF PROJECTILE  
C      - Z2, AA2 = Z AND A OF STOPPING MATERIAL     
C      - E  = LABORATORY ENERGY OF PROJECTILE IN MEV
C      - IM = SOLID STOPPING MATTER: SET IM=1,      
C   GASEOUS STOPPING MATTER: SET IM=0     
C 
C      OUTPUT:        
C      - STOPS: STOPPING POWERS IN MEV/MG/CM**2      
C      - COMMON RANGEC: VOVC = VELOCITY/C 
C        V1OVC = MAXIMUM VELOCITY FOR LINDHARDT-SCHARFF-APPROXIMATION   
C        EPS = REDUCED VELOCITY 
C        DEDXE = ELECTRONIC PART OF STOPPING POWER  
C        DEDXN = NUCLEAR PART OF STOPPING POWER     
C 
C******************************************************       
C 
       IMPLICIT REAL*8(A-H,O-Z)
       DIMENSION SC(92),AH1(92),AH2(92),AH3(92),AH4(92),      
     1 AH5(92),AH6(92),AH7(92)  
       DIMENSION A1(92),A2(92),A3(92),A4(92),A5(92),A6(92),A7(92),      
     1 A8(92),A9(92)  
       DIMENSION B1(92),B2(92),B3(92),B4(92),B5(92),B6(92),B7(92),      
     1 B8(92),B9(92)  
       COMMON /RANGEC/VOVC,V1OVC,EPS,DEDXE,DEDXN    
       COMMON /DATAD/ AH1,AH2,AH3,AH4,AH5,AH6,AH7,  
     1 A1,A2,A3,A4,A5,A6,A7,A8,A9,        
     1 B1,B2,B3,B4,B5,B6,B7,B8,B9         
       COMMON /SHELLC/ SC       
       DATA ZERO/1.E-30/        
C 
       IM=1.                                                     !MP 27-
C      AA2=2.*Z2                                                 !MP 29-
C
       IF(Z1.LE.0..OR.AA1.LE.0..OR.Z2.LE.0..OR.AA2.LE.0..OR.E.LT.ZERO)  
     1 GOTO 400       
       IZ1=IRND(Z1)   
       K=IRND(Z2)     
       IF(K.GT.92) K=92         
       EX=2./3.       
       VSQ=E/AA1      
       VOVC=0.04634*SQRT(VSQ)   
       IF(VOVC.GT.1.) VOVC=0.999999       
       V1OVC=0.0073*Z1**EX      
       FAC=0.60225/AA2
C      FAC CONVERTS UNITS FROM EV/10**15ATOMS/CM**2 TO MEV/MG/CM**2     
C 
C      NUCLEAR STOPPING POWERS  
C 
       A=AA1+AA2      
       Z=SQRT(Z1**EX+Z2**EX)    
       EPS=E*32530.*AA2/Z/Z1/Z2/A                                       
       IF(EPS.LT.0.01) GOTO 100 
       IF(EPS.GT.10) GOTO 110   
       EU=EXP(1.)     
       DEDXN=1.7*SQRT(EPS)*DLOG(EPS+EU)/(1.+6.8*EPS+3.4*EPS**1.5)       
       GOTO 120       
 100   DEDXN=1.593*SQRT(EPS)    
       GOTO 120       
 110   DEDXN=0.5*DLOG(0.47*EPS)/EPS       
 120   DEDXN=8.462*Z1*Z2*AA1/A/Z*FAC*DEDXN
C 
C      MASTER ELECTRONIC STOPPING POWERS FOR HYDROGEN         
C 
       IF(IZ1.EQ.2) GOTO 300    
       EH=1000.*VSQ   
       IF(EH.GE.1000.) GOTO 200 
       IF(EH.LE.10.) GOTO 210   
       DEDXE=FAC/(1./AH2(K)/EH**0.45+     
     1 EH/AH3(K)/DLOG(1.+AH4(K)/EH+AH5(K)*EH))      
       GOTO 220       
 200   BSQ=VOVC*VOVC  
       DEDXE=FAC*AH6(K)/BSQ*DLOG(AH7(K)*BSQ/(1.-BSQ))+SC(K)/EH
       GOTO 220       
 210   DEDXE=FAC*AH1(K)*SQRT(EH)
 220   STOPS=DEDXE+DEDXN         
       IF(IZ1.EQ.1) RETURN      
       IF(IZ1.EQ.3) GOTO 230    
C 
C      ZIEGLERS EFFECTIVE Z'S AND ELECTRONIC STOPPING         
C      POWERS FOR PROJECTILES WITH Z>3    
C 
       B=0.1772*SQRT(EH)/Z1**EX 
       G=Z1*(1.-(1.034-0.1777/EXP(0.08114*Z1))      
     1 /EXP(B+0.0378*SIN(1.5708*B)))      
       IF(G.LT.1.) G=1.         
       DEDXE=DEDXE*G*G
       STOPS=DEDXE+DEDXN         
       RETURN         
C 
C      EFFECTIVE Z AND ELECTRONIC STOPPING POWER FOR LITHIUM  
C 
 230   G=3. 
       HEXP=0.7138+0.002797*EH+1.348E-6*EH*EH       
       IF(HEXP.LT.60.) G=G-3./EXP(HEXP)   
       DEDXE=G*G*DEDXE
       STOPS=DEDXE+DEDXN         
       RETURN         
C 
C      MASTER ELECTRONIC STOPPING POWERS FOR HELIUM 
C 
 300   EHE=4.*VSQ     
C      SOLID AND GASEOUS MATTER, HIGH ENERGIES      
       IF(EHE.LT.10.) GOTO 310  
       E1=DLOG(1./EHE)
       DEDXE=FAC*EXP(A6(K)+(A7(K)+(A8(K)+A9(K)*E1)*E1)*E1)    
       STOPS=DEDXE+DEDXN         
       RETURN         
 310   IF(IM.LT.1) GOTO 320     
C      SOLID MATTER, LOW ENERGIES         
       E1=(1000.*EHE)**A2(K)    
       DEDXE=FAC/(1./E1/A1(K)+EHE/A3(K)/DLOG(1.+A4(K)/EHE+A5(K)*EHE))   
       STOPS=DEDXE+DEDXN         
       RETURN         
C      GASEOUS MATTER, LOW ENERGIES       
 320   E1=(1000.*EHE)**B2(K)    
       DEDXE=FAC/(1./E1/B1(K)+EHE/B3(K)/DLOG(1.+B4(K)/EHE+B5(K)*EHE))   
       STOPS=DEDXE+DEDXN         
       RETURN         
C 
 400   DEDXE=ZERO     
       DEDXN=ZERO     
       STOPS=ZERO      
       RETURN         
       END  
C 
C******************************************         
C 
      SUBROUTINE VELS ( NO,NP,T,TP ,NUM )                               
C****                                                                   
C****                                                                   
C     VELOCITY SELECTOR......ADDED JAN. 1976 BY W. R. BERNECKY          
C****                                                                   
C****                                                                   
      IMPLICIT  REAL*8 (A-H,O-Z)                                        
c      REAL*4 DAET, TYME                                  !JDL 31-OCT-84
      REAL*8 LF1,LU1,LF2,L,BX, BY, BZ, K, TC, DTC
      EXTERNAL BEVC                                                     
      include 'rtcomm0.f'
      COMMON /BLCK 4/  ENERGY, VEL, PMASS, Q0                           
      COMMON /BLCK 5/  XA,YA,ZA,VXA,VYA,VZA                             
      COMMON /BLCK10/  BX, BY, BZ, K, TC, DTC                           
      COMMON /BLCK11/  EX, EY, EZ, QMC, IVEC                            
      COMMON /BLCK71/  CB0,CB1,CB2,CB3,CB4,CB5                          
      COMMON /BLCK72/  CE0,CE1,CE2,CE3,CE4,CE5                          
      COMMON /BLCK73/  IN                                               
      COMMON /BLCK74/  BF,EF,S,DG                                       
      COMMON /BLCK75/  BC2,BC4,EC2,EC4                                  
      COMMON /BLCK76/  DB,DE,WB,WE                                      
C****                                                                   
C*JDL DIMENSION  DATA(75,30) , ITITLE(30)                !JDL 17-NOV-83 
      DIMENSION  TC(6),DTC(6),DS(6),ES(6)                               
C**** DATA  C/3.D10/                                                    
C****                                                                   
      LF1=DATA( 1,NO)                                                   
      LU1=DATA( 2,NO)                                                   
      LF2=DATA( 3,NO)                                                   
      DG =DATA( 4,NO)                                                   
      A  =DATA(11,NO)                                                   
      B  =DATA(12,NO)                                                   
      L  =DATA(13,NO)                                                   
      BF =DATA(14,NO)                                                   
      EF =DATA(15,NO)                                                   
      DB =DATA(16,NO)                                                   
      DE =DATA(17,NO)                                                   
      WB =DATA(18,NO)                                                   
      WE =DATA(19,NO)                                                   
      Z11=DATA(20,NO)                                                   
      Z12=DATA(21,NO)                                                   
      Z21=DATA(22,NO)                                                   
      Z22=DATA(23,NO)                                                   
      BC2=DATA(24,NO)                                                   
      BC4=DATA(25,NO)                                                   
      EC2=DATA(26,NO)                                                   
      EC4=DATA(27,NO)                                                   
      EX = 0.                                                           
      EY = 0.                                                           
      EZ = 0.                                                           
      S  = 0.                                                           
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      IF ( NP .GT. 100 ) GO TO 5                                        
      PRINT 100, ITITLE(NO)                                             
  100 FORMAT ('0VELOCITY SELECTOR****  ',A4,'  ******************'/ )   
      PRINT 101                                                         
  101 FORMAT (8H    T CM,6X,4HX CM,5X,2HBX,8X,2HEX,8X,4HY CM,5X,2HBY,8X,
     1       2HEY,7X,4HZ CM,6X,2HBZ,8X,2HEZ,6X,8HTHETA MR,5X,6HPHI MR,  
     2   2X, 'VEL/E9'   )                                               
      TDIST = T*VEL                                                     
      CALL PRNT3( TDIST,XA,YA,ZA,BX,BY,BZ,EX,EY,EZ,VXA,VYA,VZA )        
      PRINT 103                                                         
  103 FORMAT ( '0COORDINATE TRANSFORMATION TO B AXIS SYSTEM' )          
  109 FORMAT ( '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM' )          
C****                                                                   
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES                       
C****                                                                   
    5 TC(1) = -XA                                                       
      TC(2) =  YA                                                       
      TC(3) = A-ZA                                                      
      TC(4) = -VXA                                                      
      TC(5) =  VYA                                                      
      TC(6) = -VZA                                                      
      CALL PRNT3 (TDIST,TC(1),TC(2),TC(3),BX,BY,BZ,                     
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** TRANSLATE PARTICLE TO START OF FRINGE FIELD                       
C****                                                                   
      TDT = ( TC(3)-Z11 )/DABS( TC(6) )                                 
      TC(1) = TC(1)+TDT*TC(4)                                           
      TC(2) = TC(2)+TDT*TC(5)                                           
      TC(3) = TC(3)+TDT*TC(6)                                           
      T = T+TDT                                                         
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** IN DESIGNATES MAGNET REGIONS FOR BFUN                             
C****                                                                   
      IN = 1                                                            
      CB0=DATA(28,NO)                                                   
      CB1=DATA(29,NO)                                                   
      CB2=DATA(30,NO)                                                   
      CB3=DATA(31,NO)                                                   
      CB4=DATA(32,NO)                                                   
      CB5=DATA(33,NO)                                                   
      CE0=DATA(34,NO)                                                   
      CE1=DATA(35,NO)                                                   
      CE2=DATA(36,NO)                                                   
      CE3=DATA(37,NO)                                                   
      CE4=DATA(38,NO)                                                   
      CE5=DATA(39,NO)                                                   
      DTF1 = LF1/VEL                                                    
      IF ( NP .LE. 100 ) PRINT 104                                      
  104 FORMAT ( 22H0FRINGING FIELD REGION)                               
      CALL FNMIRK (6,T,DTF1,TC,DTC,DS,ES,BEVC,0 )                       
      NSTEP = 0                                                         
      TDIST = T*VEL                                                     
    6 CONTINUE                                                          
      CALL PRNT3 (TDIST,TC(1),TC(2),TC(3),BX,BY,BZ,                     
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         
      DO 7 I=1,NP                                                       
      CALL FNMIRK (6,T,DTF1,TC,DTC,DS,ES,BEVC,1 )                       
      NSTEP = NSTEP+1                                                   
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      TDIST = TDIST + DTF1*VEL                                          
      IF ( Z12 .GE. TC(3) ) GO TO 8                                     
    7 CONTINUE                                                          
      GO TO 6                                                           
    8 CONTINUE                                                          
      XDTF1 = -( Z12-TC(3) )*DABS( TC(6) )/VEL**2                       
      CALL FNMIRK (6,T,XDTF1,TC,DTC,DS,ES,BEVC,0 )                      
      CALL FNMIRK (6,T,XDTF1,TC,DTC,DS,ES,BEVC,1 )                      
      TDIST = TDIST + XDTF1*VEL                                         
      CALL PRNT3 (TDIST,TC(1),TC(2),TC(3),BX,BY,BZ,                     
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         
      IF ( NP .LE. 100 ) PRINT 105,NSTEP                                
  105 FORMAT ( 10H   NSTEPS= I5 )                                       
C****                                                                   
C****    TRANSLATE TO 2ND VFB COORDINATE SYSTEM                         
C****                                                                   
      TC(1) = -TC(1)                                                    
      TC(3) = -(TC(3)+L)                                                
      TC(4) = -TC(4)                                                    
      TC(6) = -TC(6)                                                    
C****                                                                   
C**** UNIFORM FIELD REGION                                              
C****                                                                   
      IN = 2                                                            
      DTU = LU1/VEL                                                     
      IF ( NP .LE. 100 ) PRINT 106                                      
  106 FORMAT ( '0UNIFORM FIELD REGION IN C AXIS SYSTEM' )               
      CALL FNMIRK (6,T,DTU,TC,DTC,DS,ES,BEVC,0 )                        
      NSTEP = 0                                                         
    9 CONTINUE                                                          
      CALL PRNT3 (TDIST,TC(1),TC(2),TC(3),BX,BY,BZ,                     
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         
      DO 10 I = 1,NP                                                    
      CALL FNMIRK (6,T,DTU,TC,DTC,DS,ES,BEVC,1 )                        
      NSTEP = NSTEP+1                                                   
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      TDIST = TDIST + DTU*VEL                                           
      IF ( TC(3) .GE. Z21 ) GO TO 11                                    
   10 CONTINUE                                                          
      GO TO 9                                                           
   11 CONTINUE                                                          
      XDTU = (Z21-TC(3) )*DABS( TC(6) )/VEL**2                          
      CALL FNMIRK (6,T,XDTU,TC,DTC,DS,ES,BEVC,0)                        
      CALL FNMIRK (6,T,XDTU,TC,DTC,DS,ES,BEVC,1 )                       
      TDIST = TDIST + XDTU*VEL                                          
      CALL PRNT3 (TDIST,TC(1),TC(2),TC(3),BX,BY,BZ,                     
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         
      IF ( NP .LE. 100 ) PRINT 105, NSTEP                               
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** SET UP FOR SECOND FRINGE FIELD INTEGRATION                        
C****                                                                   
      CB0=DATA(40,NO)                                                   
      CB1=DATA(41,NO)                                                   
      CB2=DATA(42,NO)                                                   
      CB3=DATA(43,NO)                                                   
      CB4=DATA(44,NO)                                                   
      CB5=DATA(45,NO)                                                   
      CE0=DATA(46,NO)                                                   
      CE1=DATA(47,NO)                                                   
      CE2=DATA(48,NO)                                                   
      CE3=DATA(49,NO)                                                   
      CE4=DATA(50,NO)                                                   
      CE5=DATA(51,NO)                                                   
      IN = 3                                                            
      DTF2 = LF2/VEL                                                    
      IF ( NP .LE. 100 ) PRINT 104                                      
      CALL FNMIRK (6,T,DTF2,TC,DTC,DS,ES,BEVC,0 )                       
      NSTEP=0                                                           
   12 CONTINUE                                                          
      CALL PRNT3 (TDIST,TC(1),TC(2),TC(3),BX,BY,BZ,                     
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         
      DO 13  I=1,NP                                                     
      CALL FNMIRK (6,T,DTF2,TC,DTC,DS,ES,BEVC,1 )                       
      NSTEP = NSTEP+1                                                   
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
        IF (NSTEP  .GT.  100000)  GO TO 99                               
      TDIST = TDIST + DTF2*VEL                                          
      IF ( TC(3) .GE. Z22 ) GO TO 14                                    
   13 CONTINUE                                                          
      GO TO 12                                                          
   14 CONTINUE                                                          
      XDTF2 = ( Z22-TC(3) )*TC(6)/VEL**2                                
      CALL FNMIRK (6,T,XDTF2,TC,DTC,DS,ES,BEVC,0 )                      
      CALL FNMIRK (6,T,XDTF2,TC,DTC,DS,ES,BEVC,1 )                      
      TDIST = TDIST + XDTF2*VEL                                         
      CALL PRNT3 (TDIST,TC(1),TC(2),TC(3),BX,BY,BZ,                     
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         
      IF (NP .LE. 100) PRINT 105,NSTEP                                  
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TPAR )
C****                                                                   
C**** TRANSLATE TO OUTPUT COORDINATES                                   
C****                                                                   
      TC(3) = TC(3)-B                                                   
      IF ( NP .LE. 100 ) PRINT 109                                      
      CALL PRNT3 (TDIST,TC(1),TC(2),TC(3),BX,BY,BZ,                     
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         
      T = TDIST/VEL                                                     
      TDT =-TC(3)/DABS( TC(6) )                                         
      TC(1) = TC(1)+TDT*TC(4)                                           
      TC(2) = TC(2)+TDT*TC(5)                                           
      TC(3) = TC(3)+TDT*TC(6)                                           
      T = T+TDT                                                         
      BX = 0.                                                           
      BY = 0.                                                           
      BZ = 0.                                                           
      EX = 0.                                                           
      EY = 0.                                                           
      EZ = 0.                                                           
      S  = 0.                                                           
      VXF    = 1000. *DATAN2( TC(4), TC(6)  )                           
      VYF    = 1000. *DASIN ( TC(5)/ VEL    )                           
      TDIST = T*VEL                                                     
      NUM = NUM+1
      TPAR = T*VEL
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TPAR )
      IF ( NP .GT. 100 ) GO TO 15                                       
      CALL PRNT3 (TDIST,TC(1),TC(2),TC(3),BX,BY,BZ,                     
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         
   15 CONTINUE                                                          
      ZDX = -TC(1)/( TC(4)/TC(6)+1.E-10 )                               
      ZDY = -TC(2)/( TC(5)/TC(6)+1.E-10 )                               
      IF (NP .LE. 100 ) PRINT 111,VXF,VYF,ZDX,ZDY                       
  111 FORMAT (/'0INTERSECTIONS WITH VER. AND HOR. PLANES '              
     X       /15X,5H  XP=F10.4,10H MR    YP=F10.4,3H MR / ,             
     1    15X,5H Z0X=F10.2,10H CM   Z0Y=F10.2,3H CM   /   )             
      RETURN                                                            
99      CALL PRNT4(NO, IN)                                              
        RETURN                                                          
      END                                                               

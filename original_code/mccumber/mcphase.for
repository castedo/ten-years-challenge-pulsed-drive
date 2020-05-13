c $DEFINE textout       ! DEFINEd for text output
$DEFINE graphout        ! DEFINEd for graphical output
c
c $DEFINE single        ! DEFINED for single rf drive
c $DEFINE biharmonic    ! DEFINED for biharmonic drive
$DEFINE pulsed        ! DEFINED for pulsed drive
c
c ======================================================================
c       Integration of RSJ Josephson junction model
c       with McCumber dimensionless parameters
c ----------------------------------------------------------------------
c       Name ......... MCPHASE.FOR (Phase Plot of McCumber RSJ model)
c       Program ...... Integration of Josephson equation with
c                      general rf drive (single, biharmonic or pulsed)
c       Output ....... Graphical
c       Version ...... 1.0
c       Language ..... Microsoft Fortran 5.1
c       Date ......... 06.10.1994
c       Last revision  08.10.1994
c       Author ....... S. Maggi
c ----------------------------------------------------------------------
c  Computes the I-V characteristic of a dc+rf current biased Josephson
c  junction, using the RSJ junction model and McCumber parameters.
c  The rf signal can be: single, biharmonic or pulsed.
c
c  The integration of the Josephson equation is performed by a 4th-order
c  Runge-Kutta method (Numerical Recipes).
c  The integration step is constant (=taustep).
c
c  The program computes the junction time response and, if the graphical
c  output is chosen,
c  (1) either plots the normalized voltage <eta>=<V>/(R*Ic)=d(phi)/d(tau)
c      vs. the normalized time tau, or phi vs. tau.
c  Otherwise, with textual output, the computed values of
c  <eta> vs. alpha.dc are printed on the screen.
c  In both cases, the simulation parameters and the above mentioned
c  values of alpha.dc and <eta> are saved in a output file.
c  ++++ PHASE PLOT
c ----------------------------------------------------------------------
c  The main simulation parameters are:
c       alpha_dc   -> normalized dc current bias
c       alpha_rf   -> normalized rf current bias
c       betac      -> hysteresis parameter
c       tau0, tau1 -> simulation starting/ending time
c       y0(i)      -> initial conditions of i-th equation
c                     (these are initially read from the data file
c                      to start the simulation, subsequently they are
c                      set to: y0(1)=<eta>, y0(2)=0.0)
c ----------------------------------------------------------------------
c  The Josephson equation in dimensionless variables (following
c  McCumber) is
c
c           dýphi      d phi
c  betac ú ÄÄÄÄÄÄÄÄ + ÄÄÄÄÄÄÄ + sin(phi) = alpha.dc + alpha.rf ú f(omegaútau)
c           d tauý     d tau
c
c  where:
c       betac    = omega_cúRúC,
c                  where R, C are the resistance and capacitance of
c                  the junction and
c                  omega_c = 4ãúeúRúIc/h  is the junction plasma frequency;
c       tau      = dimensionless time (omega_cút);
c       phi      = phase difference across the junction;
c       alpha.dc = normalized dc junction current (I/Ic);
c       alpha.rf = normalized rf junction current (Irf/Ic);
c       omega    = normalized rf frequency (omega_rf/omega_c).
c
c ======================================================================

$if defined (graphout)
      INCLUDE     'fgraph.fi'
      INCLUDE     'fgraph.fd'

c --- graphical output variable declarations
      LOGICAL      TestVGA
      CHARACTER*50 text
      REAL         alphamin, alphamax, etamin, etamax, h
$endif

c --- general variable declarations
c   - functions
      REAL         Timer
c   - variables
      CHARACTER*50 filename
      LOGICAL      twoway
      INTEGER      l, current_no
      INTEGER      neq, maxsteps, maxpoints
      INTEGER      nharm                                   ! pulsed drive
      INTEGER      klen
      REAL*8       pi2
      REAL         alpha_dc, betac, alpha_rf, omega
      REAL         rho, theta                              ! biharmonic drive
      REAL         eta_out
      REAL         tau0, tau1, taustep, taustart, dtausave
      REAL         alphadc1, alphadc2, alphadcstep
      REAL         y1, y2
      REAL         starttime, oldtime, simtime, totaltime
c   - arrays
      REAL         tau, y, y0
      REAL         alpha, avg_eta

      PARAMETER    (neq = 2)              ! equations of differential system
      PARAMETER    (maxsteps = 10000)     ! maximum lenght of integration
                                          ! results array
      PARAMETER    (maxpoints = 10000)    ! max lenght of IV array

      DIMENSION    tau(maxsteps), y(neq, maxsteps), y0(neq)
      DIMENSION    alpha(maxpoints), avg_eta(maxpoints)

      COMMON /global/   pi2
      COMMON /junction/ alpha_dc, betac, alpha_rf, omega
      COMMON /biharmon/ rho, theta
      COMMON /pulse/ nharm

      EXTERNAL     Timer

$if defined (graphout)
      EXTERNAL     TestVGA
c --- check presence of VGA card
      IF ( .not. TestVGA() ) THEN
         WRITE (*,*) 'This program requires a VGA graphics card'
         stop
      END IF
$endif

c --- read simulation data
      filename = '.\mc-iv'
      CALL ReadData(filename, tau0, tau1, taustep, taustart,
     +              dtausave,
     +              y0, y1, y2, twoway,
     +              alphadc1, alphadc2, alphadcstep, betac,
     +              alpha_rf, omega,
     +              rho, theta, nharm)

c --- global variable(s) initialization
      pi2 = 8.D0*DATAN(1.D0)

c --- check consistency of simulation parameters
      CALL CheckParameters(tau0, tau1, taustart, dtausave, maxsteps,
     +                     alphadc1, alphadc2, alphadcstep, maxpoints)

$if defined (graphout)
c --- initialize graphics screen
      CALL GraphInit(tau0, tau1, taustart, y1, y2)
$endif


c --- do calculations
$if defined (textout)
      CALL TextHeading()
$endif
      l = 0
      starttime = Timer()
c   - increasing current
      DO alpha_dc = alphadc1, alphadc2, alphadcstep
$if defined (graphout)
         CALL TextHeading(alpha_dc)
$endif
         oldtime = Timer()
         CALL RKDriver(neq, tau0, tau1, taustep, taustart, dtausave,
     +                 klen, tau, y0, y, eta_out)
         simtime = Timer() - oldtime
         l = l+1
         alpha(l) = alpha_dc
         avg_eta(l) = eta_out
         CALL TextResults(alpha_dc, eta_out, simtime)
      END DO

c ... bookkeeping
100   current_no = l
      totaltime = timer() - starttime

c --- save results in file
      CALL SaveData(filename, tau0, tau1, taustep, taustart,
     +              y0, twoway,
     +              alphadc1, alphadc2, alphadcstep, betac,
     +              alpha_rf, omega,
     +              rho, theta, nharm,
     +              totaltime,
     +              current_no, alpha, avg_eta,
     +              klen, tau, y)

$if defined (graphout)
c --- print time and wait key press
      CALL Locate(3,1)
      WRITE (text, '(1X, A, F12.1)')
     +       'Total simulation time (s): ', totaltime
      CALL outtext(text)

c --- reset graphics screen
      READ (*,*)
      status = setvideomode( $DEFAULTMODE )
$endif

c --- end program
      END

c ======================================================================
c
c
c
c +====================================================================+
c |                                                                    |
c |                COMPUTATIONAL SUBROUTINES                           |
c |                                                                    |
c +====================================================================+

c ----------------------------------------------------------------------
      SUBROUTINE RKDriver(nvar, t1, t2, tstep, tstart, dtsave,
     +                    k, tt, y0, y, y_avg)
c ----------------------------------------------------------------------

$if defined (graphout)
      INCLUDE 'fgraph.fd'
      CHARACTER*70 text
      RECORD    /wxycoord/ wxy
$endif

      INTEGER   i, k
      INTEGER   nvar, nmax
      INTEGER   nstart, npoint
      INTEGER   nharm
      REAL      t1, t2, tstep, tstart, dtsave
      REAL      y_avg
      REAL      tt, y, y0
      REAL      t, v, dv
      REAL      xx, yy, interc, angcoeff, sigint, sigang, chi2, q
      PARAMETER (nmax=2)
      DIMENSION tt(*), y(nvar, *), y0(*)
      DIMENSION v(nmax), dv(nmax)
      DIMENSION xx(10000), yy(10000)                    !*** aggiustare!!!
      COMMON    /junction/ alpha_dc, betac, alpha_rf, omega
      COMMON    /biharmon/ rho, theta
      COMMON    /pulse/ nharm

      DO i = 1, nvar
         v(i) = y0(i)
         y(i,1) = v(i)
      END DO

      tt(1) = t1
      k = 1

$if defined (graphout)
      CALL moveto_w(0.0, 0.0, wxy)
$endif

      DO t = t1, t2, tstep
         CALL Derivs(t, v, dv)
         CALL RungeKutta4(v, dv, nvar, t, tstep, v)
c   -    store results in arrays once every 'dtausave' time units
         IF ((t - tt(k)) .GE. dtsave) THEN
              k = k + 1
              tt(k) = t
              DO i = 1, nvar
                 y(i, k) = v(i)
              END DO

$if defined (graphout)
c============ CALL PlotPoint(t, y(1, k))    ! plots phi vs. tau
              CALL PlotPoint(t, y(2, k))    ! plots eta=d(phi)/d(tau) vs. tau
$endif

         END IF
      END DO

c --- fit integration results to a straight line, neglecting
c --- the initial transient of phi(tau)
      nstart = INT(tstart / dtsave)        ! fit starting at this data point
      npoint = k - nstart                  ! number of computed points to fit
      DO i = 1, npoint
         xx(i) = tt(i+nstart)
         yy(i) = y(1, i+nstart)
      END DO
      CALL Fit(xx, yy, npoint, interc, angcoeff, sigint, sigang,
     +         chi2, q)

$if defined (graphout)
      CALL moveto_w(t1, interc+angcoeff*t1, wxy)
      CALL PlotPoint(t2, interc+angcoeff*t2)
      CALL Locate(5,1)
      WRITE (*, '(1X, A, 3E15.6)')
     +      'sigint, sigang, chi2: ', sigint, sigang, chi2
$endif

      y_avg = angcoeff

c --- new initial conditions
      y0(1) = interc
      y0(2) = angcoeff

      RETURN
      END

c ----------------------------------------------------------------------
      SUBROUTINE Derivs(x, y, dydx)
c ----------------------------------------------------------------------
      INTEGER   nmax
      INTEGER   nharm
      REAL*8    pi2
      REAL*8    dtemp
      REAL      alpha_dc, betac, alpha_rf, omega
      REAL      rho, theta
      REAL      x, y, dydx
      REAL      harm1, harm2
      REAL      arg, period, pulse, tmp1, x1
      PARAMETER (nmax = 2)
      DIMENSION y(nmax), dydx(nmax)
      COMMON /global/   pi2
      COMMON /junction/ alpha_dc, betac, alpha_rf, omega
      COMMON /biharmon/ rho, theta
      COMMON /pulse/ nharm

      dtemp = y(1) / pi2

$if defined (single)
      dydx(1) = y(2)
      dydx(2) = ( alpha_dc + alpha_rf * SIN(omega*x) - y(2)
     +            - SIN(SNGL((dtemp-DINT(dtemp)) * pi2)) ) / betac
$endif

$if defined (biharmonic)
      harm1 = omega * x
      harm2 = 2 * harm1 + theta

      dydx(1) = y(2)
      dydx(2) = ( alpha_dc + alpha_rf * (SIN(harm1) + rho * SIN(harm2))
     +            - y(2) - SIN(SNGL((dtemp-DINT(dtemp)) * pi2)) )
     +          / betac
$endif

$if defined (pulsed)
c==== arg = omega * x
c==== pulse = 0.0
c==== DO k = 1, nharm
c====    pulse= pulse + COS(k*arg)
c==== END DO

      period = pi2 / omega
      x1 = x - theta * period
      tmp1 = rho * period - MOD(x1, period)
      pulse = 0.0
      IF (tmp1 .GE. 0.0) THEN
         pulse=1.
      END IF

      dydx(1) = y(2)
      dydx(2) = ( alpha_dc + alpha_rf * pulse - y(2)
     +            - SIN(SNGL((dtemp-DINT(dtemp)) * pi2)) ) / betac

$endif

      RETURN
      END

c ----------------------------------------------------------------------
      SUBROUTINE RungeKutta4(y, dydx, nvar, x, h, yout)
c ----------------------------------------------------------------------
      INTEGER   i, nvar, nmax
      REAL      x, h
      REAL      h2, h6, xh
      REAL      y, dydx, yout
      REAL      yt, dyt, dym
      PARAMETER (nmax = 2)
      DIMENSION y(nvar), dydx(nvar), yout(nvar)
      DIMENSION yt(nmax), dyt(nmax), dym(nmax)



      h2 = h * 0.5
      h6 = h / 6.0
      xh = x + h2

c --- first step
      DO i = 1, nvar
         yt(i) = y(i) + h2 * dydx(i)
      END DO
c --- second step
      CALL Derivs(xh, yt, dyt)
      DO i = 1, nvar
         yt(i) = y(i) + h2 * dyt(i)
      END DO
c --- third step
      CALL Derivs(xh, yt, dym)
      DO i = 1, nvar
         yt(i) = y(i) + h * dym(i)
         dym(i) = dyt(i) + dym(i)
      END DO
c --- fourth step
      CALL Derivs(x + h, yt, dyt)
c --- sum increments with proper weights
      DO i = 1, nvar
         yout(i) = y(i) + h6 * (dydx(i) + dyt(i) + 2.0 * dym(i))
      END DO

      END

c ----------------------------------------------------------------------
      SUBROUTINE Fit(X, Y, NDATA, A, B, SIGA, SIGB, CHI2, Q)
c ----------------------------------------------------------------------
c   Given a set of NDATA points, X(I), Y(I), fit them to a straight line
c   y = a + bx, by minimizing chi-square. Returned are A, B and their
c   respective probable uncertainties SIGA and SIGB and the chi-square CHI2.
c   In this case, it has been thus assumed that the standard
c   deviation of the fitted data are unavailable: Q is returned as 1.0
c   and the normalization of CHI2 is to unit standard deviation on all
c   points.

      INTEGER    NDATA
      INTEGER    I
      REAL       X, Y
      REAL       A, B, SIGA, SIGB, CHI2, Q
      REAL       SX, SY, ST2, SS, SXOSS, T, SIGDAT
      DIMENSION  X(NDATA), Y(NDATA)

      SX = 0.
      SY = 0.
      ST2 = 0.
      B = 0.

      DO I=1, NDATA
         SX=SX+X(I)
         SY=SY+Y(I)
      END DO
      SS=FLOAT(NDATA)

      SXOSS=SX/SS

      DO I=1, NDATA
         T=X(I)-SXOSS
         ST2=ST2+T*T
         B=B+T*Y(I)
      END DO

      B=B/ST2
      A=(SY-SX*B)/SS
      SIGA=SQRT((1.+SX*SX/(SS*ST2))/SS)
      SIGB=SQRT(1./ST2)
      CHI2=0.

      DO I=1, NDATA
         CHI2=CHI2+(Y(I)-A-B*X(I))**2
      END DO
      Q=1.
      SIGDAT=SQRT(CHI2/(NDATA-2))
      SIGA=SIGA*SIGDAT
      SIGB=SIGB*SIGDAT

      END

c ----------------------------------------------------------------------
      SUBROUTINE CheckParameters(tau0, tau1, taustart, dtausave,
     +                           maxsteps,
     +                           alphadc1, alphadc2, alphadcstep,
     +                           maxpoints)
c ----------------------------------------------------------------------
      INTEGER   maxsteps, maxpoints
      REAL      tau0, tau1, taustart, dtausave
      REAL      alphadc1, alphadc2, alphadcstep
      REAL      temp

      temp = INT( (tau1-tau0)/dtausave + 1.0 )
      IF (temp .GT. maxsteps ) THEN
         WRITE (*, ' (1X, A, \)') 'Too many time steps!   '
         WRITE (*, '(A, I6, A, I6)')
     +         '-->  required: ', temp, ',  allocated: ', maxsteps
         STOP 'Change: tau0, tau1, dtausave or maxsteps'
      ENDIF

      IF ( (taustart .LT. tau0) .OR.
     +     (taustart .GE. tau1) ) THEN
         WRITE (*, ' (1X, A, \)') 'Wrong initial transient time!   '
         WRITE (*, '(A, F12.4, A, F12.4, A)')
     +         '-->  not in interval [', tau0, ', ', tau1, ')'
         STOP 'Change: taustart'
      ENDIF

      temp = INT( (alphadc2-alphadc1)/alphadcstep + 1.0 )
      IF (temp .GT. maxpoints) THEN
         WRITE (*, '(1X, A, \)') 'Too many simulation points!   '
         WRITE (*, '(A, I6, A, I6)')
     +         '-->  required: ', temp, ',  allocated: ', maxpoints
         STOP 'Change: alphadc1, alphadc2, alphadcstep or maxpoints'
      ENDIF

      END

c +====================================================================+
c |                                                                    |
c |                TEXT SUBROUTINES                                    |
c |                                                                    |
c +====================================================================+

$if defined (textout)

c ----------------------------------------------------------------------
      SUBROUTINE TextHeading()
c ----------------------------------------------------------------------

      WRITE (*, *) ' =============================================='
      WRITE (*, *) '      alpha         <eta>             time (s) '
      WRITE (*, *) ' ----------------------------------------------'
      WRITE (*, *)
      RETURN
      END

c ----------------------------------------------------------------------
      SUBROUTINE TextResults(alpha_dc, eta_out, simtime)
c ----------------------------------------------------------------------
      REAL  alpha_dc, eta_out, simtime

      WRITE (*, '(1X, F12.4, 4X, 1P, E12.4, 6X, 0P, F12.2)')
     +            alpha_dc, eta_out, simtime
      RETURN
      END

$endif


$if defined (graphout)

c ----------------------------------------------------------------------
      SUBROUTINE TextHeading(alpha_dc)
c----------------------------------------------------------------------

      INCLUDE      'fgraph.fd'

      CHARACTER*50 text
      REAL         alpha_dc

      CALL Locate(1,1)
      WRITE (text, '(1X, A, F12.4)')
     +      'Computing for alpha: ', alpha_dc
      CALL outtext(text)
      END

c ----------------------------------------------------------------------
      SUBROUTINE TextResults(alpha_dc, eta_out, simtime)
c----------------------------------------------------------------------
      INCLUDE 'fgraph.fd'

      CHARACTER*50 text
      REAL         alpha_dc, eta_out, simtime

      CALL Locate(2,1)
      WRITE (text, '(1X, A, F12.4)')
     +      'alpha: ', alpha_dc
      CALL outtext(text)
      CALL Locate(2,25)
      WRITE (text, '(1X, A, E12.4)')
     +      '<eta>: ', eta_out
      CALL outtext(text)
      CALL Locate(2,50)
      WRITE (text, '(1X, A, F12.1)')
     +      'simul. time: ', simtime
      CALL outtext(text)
      END

$endif

$if defined (graphout)

c +====================================================================+
c |                                                                    |
c |                GRAPHICAL SUBROUTINES                               |
c |                                                                    |
c +====================================================================+

c ----------------------------------------------------------------------
      LOGICAL FUNCTION TestVGA()
c----------------------------------------------------------------------
      INCLUDE 'fgraph.fd'

      integer*2            status
      record /videoconfig/ screen
      common               screen

      CALL getvideoconfig( screen )
      SELECT CASE (screen.adapter)
         CASE( $VGA )
            status=setvideomode( $VRES16COLOR )
         CASE default
            status = 0
      END SELECT
      CALL getvideoconfig( screen )
      TestVGA = .true.
      IF (status .eq. 0) TestVGA = .false.
      END

c ----------------------------------------------------------------------
      SUBROUTINE GraphInit(x1, x2, xs, y1, y2)
c ----------------------------------------------------------------------
      include 'fgraph.fd'

      character            xstr*5, ystr*7
      integer*2            status
      integer*2            xstart, ystart, xwidth, yheight
      integer*2            style
      real                 x1, y1, x2, y2, xs
      real                 grid
      record /videoconfig/ screen
      record /wxycoord/    wxy
      record /rccoord/     curpos
      common               screen

c --- clear screen
      CALL clearscreen( $GCLEARSCREEN )

c --- creates graphical window
      xstart=60
      ystart=50
      xwidth = screen.numxpixels
      yheight = screen.numypixels-24
      CALL setviewport(xstart, ystart, xwidth, yheight)
      status = setwindow( .true., x1, y1, x2, y2)
      status = rectangle_w( $GBORDER, x1, y1, x2, y2)

c --- write text around graph area
      CALL settextposition( 17, 3, curpos )
      CALL outtext( 'eta' )
      CALL settextposition( 30, 43, curpos )
      CALL outtext( 'tau' )

      CALL settextposition( 4, 1, curpos )
      WRITE ( ystr, '(F7.1)' ) y2
      CALL outtext ( ystr )
      CALL settextposition( 29, 1, curpos )
      WRITE ( ystr, '(F7.1)' ) y1
      CALL outtext ( ystr )

      CALL settextposition( 30, 8, curpos )
      WRITE ( xstr, '(I4)' ) int(x1)
      CALL outtext( xstr )
      CALL settextposition( 30, 75, curpos )
      WRITE ( xstr, '(I5)' ) int(x2)
      CALL outtext( xstr )
c --- set line style to dotted
      style = #AAAA                    ! Hex: AAAA (dotted)
      CALL setlinestyle( style )

c --- horizontal grid
      DO grid=y1, y2, (y2-y1)/10
         CALL moveto_w(x1, grid, wxy)
         status = lineto_w(x2, grid)
      END DO

c --- vertical grid
      do grid=x1, x2, (x2-x1)/10
         CALL moveto_w(grid, y1, wxy)
         status = lineto_w(grid, y2)
      END DO
c --- vertical line at x=xs, with line style set to dashed
c --- and appropriate write mode
      status = setwritemode( $GXOR )   ! sets current logical write mode
      style = #5A5A                    ! sets current line style
      CALL setlinestyle( style )
      CALL moveto_w(xs, y1, wxy)
      status = lineto_w(xs, y2)

c --- move current graphic position to (0.0, 0.0)
      CALL moveto_w(0., 0.0, wxy)
c --- reset write mode and line style to default
      status = setwritemode( $GPSET )  ! sets logical write mode
      style = #FFFF                    ! Hex: FFFF (default)
      CALL setlinestyle( style )

      END

c ----------------------------------------------------------------------
      SUBROUTINE PlotPoint(x, y)
c ----------------------------------------------------------------------
      include 'fgraph.fd'

      INTEGER*2 status
      REAL      x, y

      status = lineto_w(x, y)

      END

c ----------------------------------------------------------------------
      SUBROUTINE Locate(row, col)
c ----------------------------------------------------------------------
      INCLUDE 'fgraph.fd'

      INTEGER          row, col
      RECORD /rccoord/ curpos

      CALL settextposition(row, col, curpos)
      END

$endif

c +====================================================================+
c |                                                                    |
c |                DATA I/O SUBROUTINES                                |
c |                                                                    |
c +====================================================================+

c ----------------------------------------------------------------------
      SUBROUTINE ReadData(filename, tau0, tau1, taustep, taustart,
     +                    dtausave,
     +                    y0, y1, y2, twoway,
     +                    alphadc1, alphadc2, alphadcstep, betac,
     +                    alpha_rf, omega,
     +                    rho, theta, nharm)
c ----------------------------------------------------------------------
      CHARACTER filename*50
      CHARACTER skip*80
      LOGICAL   twoway
      INTEGER   nharm
      REAL      tau0, tau1, taustep, taustart, dtausave
      REAL      alphadc1, alphadc2, alphadcstep, betac
      REAL      alpha_rf, omega
      REAL      y0, y1, y2
      REAL      rho, theta
      DIMENSION y0(*)

c***      WRITE (*, '(1X, A, \)')
c***     +      'Simulation data filename (no extension): '
c***      READ (*, '(BN, A)') filename
      OPEN (UNIT = 10, FILE =filename//'.dat', STATUS = 'OLD')

c Title heading
      READ (10, '(A80)') skip            ! skip one text line in data file
c Integration settings heading
      DO i=1, 7
        READ (10, '(A80)') skip
      END DO
c - read: tau0, tau1, taustep, taustart, dtausave
      READ (10, '(5F12.4)') tau0, tau1, taustep, taustart, dtausave
c - read: initial conditions at start of simulation
      READ (10, '(2F12.4)') y0(1), y0(2)
c - read: graph vertical axis limits
      READ (10, '(2F12.4)') y1, y2
c - read: simulate in one/both directions
      READ (10, '(L12   )') twoway

c Junction parameters heading
      DO i=1, 4
         READ (10, '(A80)') skip
      END DO
c - read: alphadc1, alphadc2, alphadcstep, betac
      READ (10, '(4F12.4)') alphadc1, alphadc2, alphadcstep, betac

c Single rf drive parameters heading
      DO i=1, 4
         READ (10, '(A80)') skip
      END DO
c - read: alpha_rf, omega
      READ (10, '(2F12.4)') alpha_rf, omega

c Biharmonic drive heading
      DO i=1, 4
         READ (10, '(A80)') skip
      END DO
c - read: rho, theta
      READ (10, '(2F12.4)') rho, theta

c Pulsed drive heading
      DO i=1, 4
         READ (10, '(A80)') skip
      END DO
c - read: nharm
      READ (10, '(I12)') nharm

      CLOSE (UNIT = 10)
      RETURN
      END

c ----------------------------------------------------------------------
      SUBROUTINE SaveData(filename, tau0, tau1, taustep, taustart,
     +                    y0, twoway,
     +                    alphadc1, alphadc2, alphadcstep, betac,
     +                    alpha_rf, omega,
     +                    rho, theta, nharm,
     +                    totaltime,
     +                    current_no, alpha, avg_eta,
     +                    klen, tau, y)
c ----------------------------------------------------------------------
      CHARACTER*50   filename
      CHARACTER*60   line1, line2
      LOGICAL        twoway
      INTEGER*2      iyr, imon, iday
      INTEGER*2      ihr, imin, isec, dummy
      INTEGER        i, current_no
      INTEGER        nharm
      INTEGER        klen
      REAL           tau0, tau1, taustep, taustart
      REAL           totaltime
      REAL           alphadc1, alphadc2, alphadcstep
      REAL           betac, alpha_rf, omega
      REAL           rho, theta
      REAL           y0, alpha, avg_eta
      REAL           tau, y
      DIMENSION      y0(*), alpha(*), avg_eta(*)
      DIMENSION      tau(*), y(2, *)
      line1 =
     + '============================================================'
      line2 =
     + '------------------------------------------------------------'

      OPEN (UNIT = 11, ACCESS='APPEND', FILE = filename//'.out',
     +      STATUS = 'UNKNOWN')

      WRITE (11, '(A)') line1
      WRITE (11, '(A, A)') 'Filename: ', filename
      CALL GETDAT(iyr, imon, iday)
      WRITE (11, '(A, I2.2, A, I2.2, A, I4)')
     +      'Date:     ', iday, '.', imon, '.', iyr
      CALL GETTIM(ihr, imin, isec, dummy)
      WRITE (11, '(3(A, I2.2))')
     +      'Time:     ', ihr, ':', imin, ':', isec

      WRITE (11, '(A)') line2
      WRITE (11, '(A)') 'Integration settings:'
      WRITE (11, '(4X, 2(A, F12.4) )')
     +      'time range:                  ', tau0, ' ', tau1
      WRITE (11, '(4X, A, F12.4)')
     +      'time step:                   ', taustep
      WRITE (11, '(4X, A, F12.4)')
     +      'initial transient time:      ', taustart
      WRITE (11, '(4X, 2(A, F12.4) )')
     +      'init. condit. (last cycle):  ', y0(1), ' ', y0(2)
      WRITE (11, '(4X, A, L12 )')
     +      'simulate in both directions: ', twoway

      WRITE (11, '(A)') line2
      WRITE (11, '(A)') 'Junction characteristics:'
      WRITE (11, '(4X, 2(A, F12.4) )')
     +      'normalized dc current range: ', alphadc1, ' ', alphadc2
      WRITE (11, '(4X, A, F12.4)')
     +      'dc current step:             ', alphadcstep
      WRITE (11, '(4X, A, F12.4)')
     +      'betac:                       ', betac

      WRITE (11, '(A)') line1
      WRITE (11, '(A)') 'Rf drive:'
      WRITE (11, '(4X, A, F12.4)')
     +      'normalized rf current:       ', alpha_rf
      WRITE (11, '(4X, A, F12.4)')
     +      'normalized rf frequency:     ', omega

      WRITE (11, '(A)') line2
      WRITE (11, '(A)') 'Biharmonic drive, 2nd harmonic:'
      WRITE (11, '(4X, A, F12.4)')
     +      'relative amplitude:          ', rho
      WRITE (11, '(4X, A, F12.4)')
     +      'phase difference:            ', theta

      WRITE (11, '(A)') line2
      WRITE (11, '(A)') 'Pulsed drive:'
      WRITE (11, '(4X, A, I12)')
     +      'number of harmonics          ', nharm
      WRITE (11, '(A)') line1

      WRITE (11, '(A)') 'Results:'
      WRITE (11, '(A)') ' '
      WRITE (11, '(A,  F12.4)')
     +      'Total integration time (s):  ', totaltime
      WRITE (11, '(A)') ' '
      WRITE (11, '(3A)') 'eta',            ', ', 'alpha'
      WRITE (11, '(3A)') '--------------', ', ', '--------------'
      DO i = 1, current_no
         WRITE (11, '(E12.6, A, E12.6)')
     +                avg_eta(i), ', ', alpha(i)
      END DO

      WRITE (11, '(A)') ' '
      WRITE (11, '(A)') line2
      WRITE (11, '(A)') '    tau     ,     phi     ,    phidot'
      WRITE (11, '(A)') '============, ============, ============'
      DO i = 1, klen
         WRITE (11, '(E12.6, A, E12.6, A, E12.6)')
     +                tau(i), ', ', y(1,i), ', ', y(2,i)
      END DO

      CLOSE (UNIT = 11)
      RETURN
      END

c ----------------------------------------------------------------------
      REAL FUNCTION Timer()
c ----------------------------------------------------------------------
      INTEGER*2    hr, min, sec, dummy

      CALL gettim(hr, min, sec, dummy)
      Timer = (hr * 60. + min) * 60. + sec
      END

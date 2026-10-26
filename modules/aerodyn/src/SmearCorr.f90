module SmearCorr

   use, intrinsic :: iso_c_binding, only: c_double
   use NWTC_Library
   use SmearCorr_Types

   implicit none
   private

   public :: SC_Init
   public :: SC_FLLC_Update
   public :: SC_VBSC_Update

   real(R8Ki), parameter :: SC_TwoPi        = 6.283185307179586_R8Ki
   real(R8Ki), parameter :: SC_EpsVmag      = 1.0e-12_R8Ki
   real(R8Ki), parameter :: SC_EpsOptGuard  = 1.0e-6_R8Ki
   real(R8Ki), parameter :: SC_SharpTipFrac = 0.005_R8Ki

   real(R8Ki), parameter :: VB_Pi             = 3.14159265358979323846_R8Ki
   real(R8Ki), parameter :: VB_HalfPi         = 0.5_R8Ki * VB_Pi
   real(R8Ki), parameter :: VB_CoreCutoff     = 1.82_R8Ki
   real(R8Ki), parameter :: VB_MinRootRadius  = 0.1_R8Ki
   real(R8Ki), parameter :: VB_SeriesLimit    = 4.0e-3_R8Ki
   real(R8Ki), parameter :: VB_SlowAmplitude  = 1.359_R8Ki
   real(R8Ki), parameter :: VB_FastAmplitude  = -0.359_R8Ki / 4.0_R8Ki
   real(R8Ki), parameter :: VB_StraightSlope  = 0.788_R8Ki
   real(R8Ki), parameter :: VB_RelaxMax       = 0.99_R8Ki
   integer(IntKi), parameter :: VB_RelaxRefresh = 100
   integer(IntKi), parameter :: VB_MaxIterations = 500
   real(R8Ki), parameter :: VB_RelaxSafetyStep  = 0.1_R8Ki
   real(R8Ki), parameter :: VB_RelaxSafetyLimit = 0.8_R8Ki
   real(R8Ki), parameter :: VB_FilterRate(3)   = (/ 0.3064_R8Ki, 0.0439_R8Ki, 3.227_R8Ki /)
   real(R8Ki), parameter :: VB_FilterWeight(3) = (/ 0.27735_R8Ki, 0.0914_R8Ki, 0.13125_R8Ki /)
   real(R8Ki), parameter :: VB_LagWeight(3)    = (/ 0.5547_R8Ki, 0.1828_R8Ki, 0.2625_R8Ki /)
   integer(IntKi), parameter :: VB_ShedCount = 29
   real(R8Ki), parameter :: VB_ShedLogRatioStep = 0.1732867951399863_R8Ki
   real(R8Ki), parameter :: VB_ShedWeightA(VB_ShedCount) = (/ -0.159155_R8Ki, -0.159155_R8Ki, -0.159155_R8Ki, -0.159155_R8Ki, -0.159155_R8Ki, &
      -0.158419_R8Ki, -0.152962_R8Ki, -0.140736_R8Ki, -0.123935_R8Ki, -0.106820_R8Ki, -0.091982_R8Ki, -0.080012_R8Ki, -0.070594_R8Ki, &
      -0.063216_R8Ki, -0.057421_R8Ki, -0.052859_R8Ki, -0.049271_R8Ki, -0.046471_R8Ki, -0.044317_R8Ki, -0.042701_R8Ki, -0.041531_R8Ki, &
      -0.040722_R8Ki, -0.040196_R8Ki, -0.039877_R8Ki, -0.039703_R8Ki, -0.039622_R8Ki, -0.039598_R8Ki, -0.039604_R8Ki, -0.039625_R8Ki /)
   real(R8Ki), parameter :: VB_ShedWeightB(VB_ShedCount) = (/ 0.000000_R8Ki, 0.000000_R8Ki, 0.000000_R8Ki, 0.000000_R8Ki, 0.000000_R8Ki, &
      -0.000736_R8Ki, -0.006193_R8Ki, -0.018419_R8Ki, -0.035220_R8Ki, -0.052335_R8Ki, -0.067173_R8Ki, -0.079143_R8Ki, -0.088561_R8Ki, &
      -0.095939_R8Ki, -0.101734_R8Ki, -0.106296_R8Ki, -0.109884_R8Ki, -0.112684_R8Ki, -0.114838_R8Ki, -0.116454_R8Ki, -0.117624_R8Ki, &
      -0.118432_R8Ki, -0.118959_R8Ki, -0.119278_R8Ki, -0.119452_R8Ki, -0.119533_R8Ki, -0.119557_R8Ki, -0.119551_R8Ki, -0.119530_R8Ki /)
   real(R8Ki), parameter :: VB_ShedRateA(VB_ShedCount) = (/ 0.956396_R8Ki, 0.806644_R8Ki, 0.696495_R8Ki, 0.613905_R8Ki, 0.551461_R8Ki, &
      0.501848_R8Ki, 0.449931_R8Ki, 0.393028_R8Ki, 0.337601_R8Ki, 0.288024_R8Ki, 0.246001_R8Ki, 0.211400_R8Ki, 0.183312_R8Ki, &
      0.160677_R8Ki, 0.142523_R8Ki, 0.128043_R8Ki, 0.116588_R8Ki, 0.107642_R8Ki, 0.100794_R8Ki, 0.095700_R8Ki, 0.092060_R8Ki, &
      0.089591_R8Ki, 0.088026_R8Ki, 0.087120_R8Ki, 0.086665_R8Ki, 0.086493_R8Ki, 0.086486_R8Ki, 0.086562_R8Ki, 0.086677_R8Ki /)
   real(R8Ki), parameter :: VB_ShedRateB(VB_ShedCount) = (/ 8.000000_R8Ki, 8.000000_R8Ki, 8.000000_R8Ki, 8.000000_R8Ki, 8.000000_R8Ki, &
      8.000000_R8Ki, 6.643761_R8Ki, 1.738705_R8Ki, 1.180631_R8Ki, 0.962908_R8Ki, 0.846544_R8Ki, 0.773653_R8Ki, 0.723400_R8Ki, &
      0.686589_R8Ki, 0.658588_R8Ki, 0.636829_R8Ki, 0.619780_R8Ki, 0.606462_R8Ki, 0.596209_R8Ki, 0.588518_R8Ki, 0.582970_R8Ki, &
      0.579174_R8Ki, 0.576748_R8Ki, 0.575333_R8Ki, 0.574616_R8Ki, 0.574342_R8Ki, 0.574328_R8Ki, 0.574444_R8Ki, 0.574622_R8Ki /)
   real(R8Ki), parameter :: VB_OutboardFit(5,4) = reshape( (/ &
       1.01933206359188_R8Ki,  -0.135668151345576_R8Ki,  0.395524705910666_R8Ki,   0.0801822997046969_R8Ki, 44.8347503471217_R8Ki, &
      12.9874512870379_R8Ki,   49.9999999999916_R8Ki,    0.00235345218088083_R8Ki, 11.3116074964299_R8Ki,   3935.34323353354_R8Ki, &
      -0.690159136848837_R8Ki, 101.238775997879_R8Ki,   -0.00154467589177718_R8Ki,  3.99520376143011_R8Ki,   0.394541495293350_R8Ki, &
      -0.269253409815015_R8Ki,  49.9999999999843_R8Ki,   -0.00247975579295973_R8Ki,  0.403642518874381_R8Ki,  1.16610462658971_R8Ki /), (/ 5, 4 /) )
   real(R8Ki), parameter :: VB_InboardFit(4,4) = reshape( (/ &
      -1.64636754184988_R8Ki,   8.14821474772595_R8Ki, -12.1784861715161_R8Ki,   5.02652654794305_R8Ki, &
      -0.499006672527007_R8Ki,  6.08465268675599_R8Ki, -15.1712005446736_R8Ki,  14.8254135311781_R8Ki, &
       3.90835606394205_R8Ki, -18.7662330478906_R8Ki,   39.1243282362892_R8Ki, -29.4870086106427_R8Ki, &
      -1.60622843516794_R8Ki,   7.42952711674470_R8Ki, -15.8594777074799_R8Ki,  11.6870163960092_R8Ki /), (/ 4, 4 /) )

   interface
      function c_expm1(x) bind(C, name='expm1')
         import :: c_double
         real(c_double), value :: x
         real(c_double)        :: c_expm1
      end function c_expm1
   end interface

contains

subroutine SC_Init(InitInp, p, xd, m, ErrStat, ErrMsg)
   type(SC_InitInputType),     intent(in   ) :: InitInp
   type(SC_ParameterType),     intent(  out) :: p
   type(SC_DiscreteStateType), intent(  out) :: xd
   type(SC_MiscVarType),       intent(  out) :: m
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   integer(IntKi)          :: ErrStat2
   character(ErrMsgLen)    :: ErrMsg2
   character(*), parameter :: RoutineName = 'SC_Init'
   integer(IntKi)          :: n, nb

   ErrStat = ErrID_None
   ErrMsg  = ""

   p%SmearMod      = InitInp%SmearMod
   p%Eps           = InitInp%Eps
   p%Start         = InitInp%Start
   p%EpsChord      = InitInp%EpsChord
   p%Tau           = InitInp%Tau
   p%MaxResid      = InitInp%MaxResid
   p%CircFilter    = InitInp%CircFilter
   p%RelaxSafety   = InitInp%RelaxSafety
   p%Shed          = InitInp%Shed
   p%ShedPitchRate = InitInp%ShedPitchRate
   p%NumBlades     = InitInp%NumBlades
   p%NumBlNds      = InitInp%NumBlNds
   p%DT            = InitInp%DT

   n  = p%NumBlNds
   nb = p%NumBlades
   if (n < 2) then
      call SetErrStat(ErrID_Fatal, 'The smearing correction needs at least 2 nodes per blade.', ErrStat, ErrMsg, RoutineName)
      return
   end if

   call AllocAry(xd%du,    3, n, nb, 'xd%du',    ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%duNew,  3, n, nb, 'm%duNew',  ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%W,      3, n, nb, 'm%W',      ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%F,      3, n, nb, 'm%F',      ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%G,      3, n,     'm%G',      ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%Umag,   n,        'm%Umag',   ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%Smooth, 3, n,     'm%Smooth', ErrStat2, ErrMsg2); if (Failed()) return
   xd%du    = 0.0_R8Ki
   m%duNew  = 0.0_R8Ki
   m%W      = 0.0_R8Ki
   m%F      = 0.0_R8Ki
   m%G      = 0.0_R8Ki
   m%Umag   = 0.0_R8Ki
   m%Smooth = 0.0_R8Ki
   m%nUpdates = 0

   if (p%SmearMod == SC_FLLC) then
      call FLLC_Init(InitInp, p, ErrStat2, ErrMsg2); if (Failed()) return
   else if (p%SmearMod == SC_VBSC) then
      call VBSC_Init(InitInp, p, xd, m, ErrStat2, ErrMsg2); if (Failed()) return
   end if

contains
   logical function Failed()
      call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
   end function Failed
end subroutine SC_Init

subroutine FLLC_Init(InitInp, p, ErrStat, ErrMsg)
   type(SC_InitInputType), intent(in   ) :: InitInp
   type(SC_ParameterType), intent(inout) :: p
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   integer(IntKi)          :: ErrStat2
   character(ErrMsgLen)    :: ErrMsg2
   character(*), parameter :: RoutineName = 'FLLC_Init'
   integer(IntKi)          :: n, nb, j, k, q
   real(R8Ki), allocatable :: z(:), cRaw(:), epsOpt(:)
   real(R8Ki)              :: maxC, slope, ext, dzMean, epsMin, epsFloor, epsRaw
   real(R8Ki)              :: epsLes2, epsOpt2, r, r2, aL, aO, inv2r2, worst

   ErrStat = ErrID_None
   ErrMsg  = ""

   if (.not. (p%Eps > 0.0_ReKi)) then
      call SetErrStat(ErrID_Fatal, 'SmearEps must be greater than 0.', ErrStat, ErrMsg, RoutineName); return
   end if
   if (.not. (p%Tau > 0.0_ReKi)) then
      call SetErrStat(ErrID_Fatal, 'FLLCTau must be greater than 0.', ErrStat, ErrMsg, RoutineName); return
   end if
   if (.not. (p%EpsChord > 0.0_ReKi)) then
      call SetErrStat(ErrID_Fatal, 'FLLCEpsChord must be greater than 0.', ErrStat, ErrMsg, RoutineName); return
   end if

   n  = p%NumBlNds
   nb = p%NumBlades
   p%fRelax = 1.0_R8Ki - exp(-real(p%DT, R8Ki) / real(p%Tau, R8Ki))

   call AllocAry(p%chord,    n, nb,    'p%chord',    ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%dz,       n, nb,    'p%dz',       ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%kLes,     n, n, nb, 'p%kLes',     ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%kOpt,     n, n, nb, 'p%kOpt',     ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%Resolved, n, nb,    'p%Resolved', ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(z,          n,        'z',          ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(cRaw,       n,        'cRaw',       ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(epsOpt,     n,        'epsOpt',     ErrStat2, ErrMsg2); if (Failed()) return

   epsLes2 = real(p%Eps, R8Ki)**2
   p%SharpTip = .false.

   do k = 1, nb
      do j = 1, n
         z(j) = TwoNorm(InitInp%NodePos(:,j,k) - InitInp%NodePos(:,1,k))
      end do
      do j = 2, n - 1
         p%dz(j,k) = 0.5_R8Ki * TwoNorm(InitInp%NodePos(:,j+1,k) - InitInp%NodePos(:,j-1,k))
      end do
      p%dz(1,k) = 0.5_R8Ki * TwoNorm(InitInp%NodePos(:,2,k) - InitInp%NodePos(:,1,k))
      p%dz(n,k) = 0.5_R8Ki * TwoNorm(InitInp%NodePos(:,n,k) - InitInp%NodePos(:,n-1,k))

      cRaw = real(InitInp%chord(:,k), R8Ki)
      maxC = maxval(cRaw)
      if (k == 1) p%SharpTip = (maxC > 0.0_R8Ki) .and. (cRaw(n) < SC_SharpTipFrac * maxC)

      p%chord(:,k) = cRaw
      do j = 2, n
         if (p%chord(j,k) >= 0.1_R8Ki * p%chord(j-1,k)) cycle
         ext = p%chord(j-1,k)
         if (j >= 3) then
            slope = (p%chord(j-1,k) - p%chord(j-2,k)) / (z(j-1) - z(j-2))
            if (p%chord(j-1,k) + slope * (z(j) - z(j-1)) > 0.0_R8Ki) ext = p%chord(j-1,k) + slope * (z(j) - z(j-1))
         end if
         p%chord(j,k) = ext
      end do

      dzMean   = (z(n) - z(1)) / real(max(1, n - 1), R8Ki)
      epsMin   = max(3.0_R8Ki * dzMean, SC_EpsOptGuard)
      if (p%SharpTip) then
         epsFloor = epsMin
      else
         epsFloor = SC_EpsOptGuard
      end if
      do j = 1, n
         epsRaw = real(p%EpsChord, R8Ki) * p%chord(j,k)
         epsOpt(j) = max(epsRaw, epsFloor)
         p%Resolved(j,k) = .not. (p%SharpTip .and. epsRaw < epsMin)
      end do

      do j = 1, n
         do q = 1, n
            r = z(q) - z(j)
            epsOpt2 = epsOpt(q)**2
            if (r == 0.0_R8Ki) then
               p%kLes(j,q,k) = 0.5_R8Ki / epsLes2
               p%kOpt(j,q,k) = 0.5_R8Ki / epsOpt2
            else
               r2 = r * r
               aL = -r2 / epsLes2
               aO = -r2 / epsOpt2
               inv2r2 = 1.0_R8Ki / (2.0_R8Ki * r2)
               p%kLes(j,q,k) = exp(aL) / epsLes2 + inv2r2 * c_expm1(aL)
               p%kOpt(j,q,k) = exp(aO) / epsOpt2 + inv2r2 * c_expm1(aO)
            end if
         end do
      end do
   end do

   worst = huge(1.0_R8Ki)
   do k = 1, nb
      do j = 2, n
         r = TwoNorm(InitInp%NodePos(:,j,k) - InitInp%NodePos(:,1,k)) - TwoNorm(InitInp%NodePos(:,j-1,k) - InitInp%NodePos(:,1,k))
         if (abs(r) > 0.0_R8Ki) worst = min(worst, real(p%EpsChord, R8Ki) * p%chord(j,k) / abs(r))
      end do
   end do
   call WrScr('   FLLC: smallest target width over node spacing '//trim(num2lstr(worst))//', sharp tip '//trim(merge('yes', 'no ', p%SharpTip)))
   if (worst < 1.0_R8Ki) call WrScr('   FLLC WARNING: the target width is below the node spacing, so the nodes do not resolve the correction.')

   call Cleanup()

contains
   logical function Failed()
      call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
      if (Failed) call Cleanup()
   end function Failed
   subroutine Cleanup()
      if (allocated(z))      deallocate(z)
      if (allocated(cRaw))   deallocate(cRaw)
      if (allocated(epsOpt)) deallocate(epsOpt)
   end subroutine Cleanup
end subroutine FLLC_Init

subroutine SC_FLLC_Update(tNew, p, duOld, rho, m)
   real(DbKi),             intent(in   ) :: tNew
   type(SC_ParameterType), intent(in   ) :: p
   real(R8Ki),             intent(in   ) :: duOld(:,:,:)
   real(ReKi),             intent(in   ) :: rho
   type(SC_MiscVarType),   intent(inout) :: m

   integer(IntKi) :: n, k, j, q, pass, j0, j1, i, lastRes, firstRes
   real(R8Ki)     :: W(3), F(3), W2, fw, invW2, invRho, coef, uLes(3), uOpt(3), w8, s(3), oneMinusF
   real(R8Ki), parameter :: bw(5) = (/ 1.0_R8Ki, 4.0_R8Ki, 6.0_R8Ki, 4.0_R8Ki, 1.0_R8Ki /)

   if (tNew < p%Start) then
      m%duNew = 0.0_R8Ki
      return
   end if

   n = p%NumBlNds
   invRho = 1.0_R8Ki / real(rho, R8Ki)
   oneMinusF = 1.0_R8Ki - p%fRelax

   do k = 1, p%NumBlades
      do j = 1, n
         W  = m%W(:,j,k)
         F  = m%F(:,j,k)
         W2 = dot_product(W, W)
         m%Umag(j) = max(sqrt(W2), SC_EpsVmag)
         fw = dot_product(F, W)
         invW2 = 1.0_R8Ki / max(W2, SC_EpsVmag * SC_EpsVmag)
         m%G(:,j) = -(F - W * fw * invW2) * invRho
      end do

      do j = 1, n
         uLes = 0.0_R8Ki
         uOpt = 0.0_R8Ki
         do q = 1, n
            coef = p%dz(q,k) / (SC_TwoPi * m%Umag(q))
            uLes = uLes + m%G(:,q) * p%kLes(j,q,k) * coef
            uOpt = uOpt + m%G(:,q) * p%kOpt(j,q,k) * coef
         end do
         m%duNew(:,j,k) = oneMinusF * duOld(:,j,k) + p%fRelax * (uOpt - uLes)
      end do

      if (p%SharpTip) then
         lastRes = 0
         do j = 1, n
            if (p%Resolved(j,k)) then
               lastRes = j
            else if (lastRes > 0) then
               m%duNew(:,j,k) = m%duNew(:,lastRes,k)
            end if
         end do
         firstRes = 0
         do j = 1, n
            if (p%Resolved(j,k)) then
               firstRes = j
               exit
            end if
         end do
         if (firstRes > 1) then
            do j = 1, firstRes - 1
               m%duNew(:,j,k) = m%duNew(:,firstRes,k)
            end do
         end if
         if (n >= 5) then
            do pass = 1, 4
               m%Smooth = m%duNew(:,:,k)
               do j = 1, n
                  j0 = max(1, j - 2)
                  j1 = min(n, j + 2)
                  w8 = 0.0_R8Ki
                  s  = 0.0_R8Ki
                  do i = j0, j1
                     w8 = w8 + bw(i - j + 3)
                     s  = s  + bw(i - j + 3) * m%Smooth(:,i)
                  end do
                  m%duNew(:,j,k) = s / w8
               end do
            end do
         end if
      end if
   end do

   m%nUpdates = m%nUpdates + 1

end subroutine SC_FLLC_Update

subroutine VBSC_Init(InitInp, p, xd, m, ErrStat, ErrMsg)
   type(SC_InitInputType),     intent(in   ) :: InitInp
   type(SC_ParameterType),     intent(inout) :: p
   type(SC_DiscreteStateType), intent(inout) :: xd
   type(SC_MiscVarType),       intent(inout) :: m
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg

   integer(IntKi)          :: ErrStat2
   character(ErrMsgLen)    :: ErrMsg2
   character(*), parameter :: RoutineName = 'VBSC_Init'
   integer(IntKi)          :: n, nb, ns, ne, np, k, j, s, idLo, idHi, nk, nTot, i0, i1
   real(R8Ki)              :: x, tt
   real(R8Ki), allocatable :: z(:), zs(:), knots(:), tabA(:), tabC(:)
   integer(IntKi)          :: iw

   ErrStat = ErrID_None
   ErrMsg  = ""

   n  = p%NumBlNds
   nb = p%NumBlades
   ns = n - 1
   ne = n
   np = ne * ns
   p%ns = ns
   p%ne = ne
   if (n < 3) then
      call SetErrStat(ErrID_Fatal, 'The VBSC needs at least 3 nodes per blade.', ErrStat, ErrMsg, RoutineName); return
   end if
   if (.not. (p%Eps > 0.0_ReKi)) then
      call SetErrStat(ErrID_Fatal, 'SmearEps must be greater than 0.', ErrStat, ErrMsg, RoutineName); return
   end if
   if (p%Shed) then
      call SetErrStat(ErrID_Fatal, 'VBSCShed is not implemented yet.', ErrStat, ErrMsg, RoutineName); return
   end if

   call AllocAry(p%secT,       ns, nb,    'p%secT',       ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%velLo,      n,  nb,    'p%velLo',      ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%velT,       n,  nb,    'p%velT',       ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%secChord,   ns, nb,    'p%secChord',   ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%secTwist,   ns, nb,    'p%secTwist',   ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%shedWeight, 2, ns, nb, 'p%shedWeight', ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%shedRate,   2, ns, nb, 'p%shedRate',   ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%liftOffset, ns*nb+1,   'p%liftOffset', ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(z,  n,  'z',  ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(zs, ns, 'zs', ErrStat2, ErrMsg2); if (Failed()) return

   nTot = 0
   do k = 1, nb
      do s = 1, ns
         idLo = InitInp%AFindx(s,k)
         idHi = InitInp%AFindx(s+1,k)
         nTot = nTot + (InitInp%AFOffset(idLo+1) - InitInp%AFOffset(idLo)) + (InitInp%AFOffset(idHi+1) - InitInp%AFOffset(idHi))
      end do
   end do
   call AllocAry(tabA, nTot, 'tabA', ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(tabC, nTot, 'tabC', ErrStat2, ErrMsg2); if (Failed()) return

   iw = 0
   do k = 1, nb
      z(1) = 0.0_R8Ki
      do j = 2, n
         z(j) = TwoNorm(InitInp%NodePos(:,j,k) - InitInp%NodePos(:,1,k))
         if (.not. (z(j) > z(j-1))) then
            call SetErrStat(ErrID_Fatal, 'The VBSC needs node distances from the root that increase along the blade.', ErrStat, ErrMsg, RoutineName)
            call Cleanup(); return
         end if
      end do
      do s = 1, ns
         zs(s) = 0.5_R8Ki * (z(s) + z(s+1))
         p%secT(s,k) = (zs(s) - z(s)) / (z(s+1) - z(s))
      end do
      do j = 1, n
         if (z(j) <= zs(1)) then
            p%velLo(j,k) = 1
            p%velT(j,k)  = 0.0_R8Ki
         else if (z(j) >= zs(ns)) then
            p%velLo(j,k) = ns - 1
            p%velT(j,k)  = 1.0_R8Ki
         else
            p%velLo(j,k) = j - 1
            p%velT(j,k)  = (z(j) - zs(j-1)) / (zs(j) - zs(j-1))
         end if
      end do
      do s = 1, ns
         tt = p%secT(s,k)
         p%secChord(s,k) = real(InitInp%chord(s,k), R8Ki) * (1.0_R8Ki - tt) + real(InitInp%chord(s+1,k), R8Ki) * tt
         p%secTwist(s,k) = real(InitInp%twist(s,k), R8Ki) * (1.0_R8Ki - tt) + real(InitInp%twist(s+1,k), R8Ki) * tt
         if (.not. (p%secChord(s,k) > 0.0_R8Ki)) then
            call SetErrStat(ErrID_Fatal, 'The VBSC needs a chord greater than 0 at every section.', ErrStat, ErrMsg, RoutineName)
            call Cleanup(); return
         end if

         x = log(real(p%Eps, R8Ki) / p%secChord(s,k)) / VB_ShedLogRatioStep
         x = min(max(x, 0.0_R8Ki), real(VB_ShedCount - 1, R8Ki))
         i0 = min(int(x), VB_ShedCount - 2)
         x = x - real(i0, R8Ki)
         p%shedWeight(1,s,k) = (1.0_R8Ki - x) * VB_ShedWeightA(i0+1) + x * VB_ShedWeightA(i0+2)
         p%shedWeight(2,s,k) = (1.0_R8Ki - x) * VB_ShedWeightB(i0+1) + x * VB_ShedWeightB(i0+2)
         p%shedRate(1,s,k)   = (1.0_R8Ki - x) * VB_ShedRateA(i0+1)   + x * VB_ShedRateA(i0+2)
         p%shedRate(2,s,k)   = (1.0_R8Ki - x) * VB_ShedRateB(i0+1)   + x * VB_ShedRateB(i0+2)

         idLo = InitInp%AFindx(s,k)
         idHi = InitInp%AFindx(s+1,k)
         i0 = InitInp%AFOffset(idLo)
         i1 = InitInp%AFOffset(idLo+1) - 1
         p%liftOffset((k-1)*ns+s) = iw + 1
         if (idLo /= idHi .and. tt > 0.0_R8Ki) then
            nk = (i1 - i0 + 1) + (InitInp%AFOffset(idHi+1) - InitInp%AFOffset(idHi))
            call AllocAry(knots, nk, 'knots', ErrStat2, ErrMsg2); if (Failed()) return
            knots(1:i1-i0+1) = InitInp%AFAlpha(i0:i1)
            knots(i1-i0+2:nk) = InitInp%AFAlpha(InitInp%AFOffset(idHi):InitInp%AFOffset(idHi+1)-1)
            call SortUnique(knots, nk)
            do j = 1, nk
               iw = iw + 1
               tabA(iw) = knots(j)
               tabC(iw) = (1.0_R8Ki - tt) * InterpLinear(InitInp%AFAlpha(i0:i1), InitInp%AFCl(i0:i1), knots(j)) + &
                          tt * InterpLinear(InitInp%AFAlpha(InitInp%AFOffset(idHi):InitInp%AFOffset(idHi+1)-1), &
                                            InitInp%AFCl(InitInp%AFOffset(idHi):InitInp%AFOffset(idHi+1)-1), knots(j))
            end do
            deallocate(knots)
         else
            do j = i0, i1
               iw = iw + 1
               tabA(iw) = InitInp%AFAlpha(j)
               if (idLo == idHi) then
                  tabC(iw) = (1.0_R8Ki - tt) * InitInp%AFCl(j) + tt * InitInp%AFCl(j)
               else
                  tabC(iw) = (1.0_R8Ki - tt) * InitInp%AFCl(j) + tt * InterpLinear(InitInp%AFAlpha(InitInp%AFOffset(idHi):InitInp%AFOffset(idHi+1)-1), &
                                                                   InitInp%AFCl(InitInp%AFOffset(idHi):InitInp%AFOffset(idHi+1)-1), InitInp%AFAlpha(j))
               end if
            end do
         end if
      end do
   end do
   p%liftOffset(ns*nb+1) = iw + 1
   call AllocAry(p%liftAlpha, iw, 'p%liftAlpha', ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(p%liftCl,    iw, 'p%liftCl',    ErrStat2, ErrMsg2); if (Failed()) return
   p%liftAlpha = tabA(1:iw)
   p%liftCl    = tabC(1:iw)

   call AllocAry(xd%vn,           ns, nb,    'xd%vn',           ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%vt,           ns, nb,    'xd%vt',           ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%wake,         ns, nb,    'xd%wake',         ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%steady,       ns, nb,    'xd%steady',       ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%filter,    3, ns, nb,    'xd%filter',       ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%attack,       ns, nb,    'xd%attack',       ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%shedFilt,  2, ns, nb,    'xd%shedFilt',     ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%shedLift,     ns, nb,    'xd%shedLift',     ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%shedAngle,    ns, nb,    'xd%shedAngle',    ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%shedReady,        nb,    'xd%shedReady',    ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%rEdge,        ne, nb,    'xd%rEdge',        ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%axialEdge,    ne, nb,    'xd%axialEdge',    ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%rSection,     ns, nb,    'xd%rSection',     ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%axialSection, ns, nb,    'xd%axialSection', ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%slowWake,     np, nb,    'xd%slowWake',     ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(xd%fastWake,     np, nb,    'xd%fastWake',     ErrStat2, ErrMsg2); if (Failed()) return
   xd%vn = 0.0_R8Ki; xd%vt = 0.0_R8Ki; xd%wake = 0.0_R8Ki; xd%steady = 0.0_R8Ki; xd%filter = 0.0_R8Ki; xd%attack = 0.0_R8Ki
   xd%shedFilt = 0.0_R8Ki; xd%shedLift = 0.0_R8Ki; xd%shedAngle = 0.0_R8Ki; xd%shedReady = 0.0_R8Ki
   xd%rEdge = 0.0_R8Ki; xd%axialEdge = 0.0_R8Ki; xd%rSection = 0.0_R8Ki; xd%axialSection = 0.0_R8Ki
   xd%slowWake = 0.0_R8Ki; xd%fastWake = 0.0_R8Ki
   xd%relaxSafety = real(p%RelaxSafety, R8Ki)

   call AllocAry(m%dValid,            nb, 'm%dValid',        ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%dREdge,        ne, nb, 'm%dREdge',        ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%dAxialEdge,    ne, nb, 'm%dAxialEdge',    ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%dRSection,     ns, nb, 'm%dRSection',     ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%dAxialSection, ns, nb, 'm%dAxialSection', ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%edgeSecW,      ne, nb, 'm%edgeSecW',      ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%secFirst,      ne, nb, 'm%secFirst',      ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%secCount,      ne, nb, 'm%secCount',      ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%pairBegin,   ne+1, nb, 'm%pairBegin',     ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%circPhi,       np, nb, 'm%circPhi',       ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%straightPhi,   np, nb, 'm%straightPhi',   ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%slowAmp,       np, nb, 'm%slowAmp',       ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%fastAmp,       np, nb, 'm%fastAmp',       ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%radiusRatio,   np, nb, 'm%radiusRatio',   ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%helixFit,   4, np, nb, 'm%helixFit',      ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%planarSmear,   np, nb, 'm%planarSmear',   ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%axialOff2,     np, nb, 'm%axialOff2',     ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%ownPhi,     2, ns, nb, 'm%ownPhi',        ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%ownSlow,    2, ns, nb, 'm%ownSlow',       ErrStat2, ErrMsg2); if (Failed()) return
   call AllocAry(m%ownFast,    2, ns, nb, 'm%ownFast',       ErrStat2, ErrMsg2); if (Failed()) return
   m%dValid = .false.

   call Cleanup()

contains
   logical function Failed()
      call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
      Failed = ErrStat >= AbortErrLev
      if (Failed) call Cleanup()
   end function Failed
   subroutine Cleanup()
      if (allocated(z))     deallocate(z)
      if (allocated(zs))    deallocate(zs)
      if (allocated(knots)) deallocate(knots)
      if (allocated(tabA))  deallocate(tabA)
      if (allocated(tabC))  deallocate(tabC)
   end subroutine Cleanup
end subroutine VBSC_Init

subroutine SortUnique(a, n)
   real(R8Ki),     intent(inout) :: a(:)
   integer(IntKi), intent(inout) :: n
   integer(IntKi) :: i, j, kept
   real(R8Ki)     :: v
   do i = 2, n
      v = a(i)
      j = i - 1
      do while (j >= 1)
         if (a(j) <= v) exit
         a(j+1) = a(j)
         j = j - 1
      end do
      a(j+1) = v
   end do
   kept = 1
   do i = 2, n
      if (abs(a(kept) - a(i)) < 1.0e-12_R8Ki) cycle
      kept = kept + 1
      a(kept) = a(i)
   end do
   n = kept
end subroutine SortUnique

function InterpLinear(xs, ys, x) result(y)
   real(R8Ki), intent(in) :: xs(:), ys(:), x
   real(R8Ki)             :: y, t
   integer(IntKi)         :: lo, hi, mid
   if (x <= xs(1)) then
      y = ys(1); return
   end if
   if (x >= xs(size(xs))) then
      y = ys(size(xs)); return
   end if
   lo = 1
   hi = size(xs)
   do while (hi - lo > 1)
      mid = (lo + hi) / 2
      if (xs(mid) <= x) then
         lo = mid
      else
         hi = mid
      end if
   end do
   t = (x - xs(lo)) / (xs(hi) - xs(lo))
   y = ys(lo) * (1.0_R8Ki - t) + ys(hi) * t
end function InterpLinear

subroutine NearWake(h, hr, phi, slow, fast)
   real(R8Ki), intent(in   ) :: h, hr
   real(R8Ki), intent(  out) :: phi, slow, fast
   real(R8Ki) :: unit, quarterTurn
   if (hr > 0.0_R8Ki) then
      phi = 0.25_R8Ki * VB_Pi * abs((1.0_R8Ki + 0.5_R8Ki * hr) * log(1.0_R8Ki - hr))
   else
      phi = log(1.0_R8Ki - hr) / (1.5_R8Ki + log(1.0_R8Ki - 0.5_R8Ki * hr))
   end if
   unit = 1.0_R8Ki / (4.0_R8Ki * VB_Pi * abs(h) * hr)
   slow = VB_SlowAmplitude * unit
   fast = VB_FastAmplitude * unit
   quarterTurn = (slow * (1.0_R8Ki - exp(-VB_HalfPi / phi)) + fast * (1.0_R8Ki - exp(-2.0_R8Ki * VB_Pi / phi))) / (slow + fast)
   phi = phi * quarterTurn
end subroutine NearWake

subroutine VBSC_BuildDerived(p, k, rEdge, axialEdge, rSection, axialSection, m)
   type(SC_ParameterType), intent(in   ) :: p
   integer(IntKi),         intent(in   ) :: k
   real(R8Ki),             intent(in   ) :: rEdge(:), axialEdge(:), rSection(:), axialSection(:)
   type(SC_MiscVarType),   intent(inout) :: m
   integer(IntKi) :: ns, ne, e, s, first, last, q, i, c
   real(R8Ki)     :: cutoff, eps2, h, hr, delta
   real(R8Ki)     :: fo(5), fi(4)

   ns = p%ns
   ne = p%ne
   cutoff = VB_CoreCutoff * real(p%Eps, R8Ki)
   eps2   = real(p%Eps, R8Ki)**2

   m%edgeSecW(:,k) = 0.0_R8Ki
   do e = 2, ne - 1
      m%edgeSecW(e,k) = (rEdge(e) - rSection(e-1)) / (rSection(e) - rSection(e-1))
   end do

   m%pairBegin(1,k) = 1
   do e = 1, ne
      first = 1
      last  = ns
      do s = 1, ns
         h = rEdge(e) - rSection(s)
         if (h > 0.0_R8Ki) then
            if (abs(h) > cutoff) first = s + 1
         else if (abs(h) < cutoff) then
            last = s
         end if
      end do
      if (last < first) then
         m%secFirst(e,k) = 1
         m%secCount(e,k) = 0
      else
         m%secFirst(e,k) = first
         m%secCount(e,k) = last - first + 1
      end if
      m%pairBegin(e+1,k) = m%pairBegin(e,k) + m%secCount(e,k)
   end do

   do e = 1, ne
      do c = 0, m%secCount(e,k) - 1
         s = m%secFirst(e,k) + c
         q = m%pairBegin(e,k) + c
         h  = rEdge(e) - rSection(s)
         hr = h / rEdge(e)
         call NearWake(h, hr, m%circPhi(q,k), m%slowAmp(q,k), m%fastAmp(q,k))
         m%straightPhi(q,k) = VB_StraightSlope * abs(hr)
         m%radiusRatio(q,k) = hr
         do i = 1, 4
            if (hr < 0.0_R8Ki) then
               fo = VB_OutboardFit(:,i)
               m%helixFit(i,q,k) = fo(1) + fo(2) * exp(fo(3) * hr) + fo(4) * exp(fo(5) * hr) - fo(4) - fo(2)
            else
               fi = VB_InboardFit(:,i)
               m%helixFit(i,q,k) = fi(1) + fi(2) * hr + fi(3) * hr * hr + fi(4) * hr * hr * hr
            end if
         end do
         delta = -(axialEdge(e) - axialSection(s))
         m%planarSmear(q,k) = exp(-h * h / eps2)
         m%axialOff2(q,k)   = delta * delta / eps2
      end do
   end do

   do s = 1, ns
      do i = 1, 2
         h = rEdge(s + i - 1) - rSection(s)
         call NearWake(h, h / rEdge(s + i - 1), m%ownPhi(i,s,k), m%ownSlow(i,s,k), m%ownFast(i,s,k))
      end do
   end do

   m%dREdge(:,k)        = rEdge
   m%dAxialEdge(:,k)    = axialEdge
   m%dRSection(:,k)     = rSection
   m%dAxialSection(:,k) = axialSection
   m%dValid(k) = .true.
end subroutine VBSC_BuildDerived

subroutine VBSC_EnsureDerived(p, k, m)
   type(SC_ParameterType),     intent(in   ) :: p
   integer(IntKi),             intent(in   ) :: k
   type(SC_MiscVarType),       intent(inout) :: m
   real(R8Ki) :: rE(p%ne), aE(p%ne), rS(p%ns), aS(p%ns)
   if (m%dValid(k)) then
      if (all(m%dREdge(:,k) == m%xdNew%rEdge(:,k)) .and. all(m%dAxialEdge(:,k) == m%xdNew%axialEdge(:,k)) .and. &
          all(m%dRSection(:,k) == m%xdNew%rSection(:,k)) .and. all(m%dAxialSection(:,k) == m%xdNew%axialSection(:,k))) return
   end if
   rE = m%xdNew%rEdge(:,k)
   aE = m%xdNew%axialEdge(:,k)
   rS = m%xdNew%rSection(:,k)
   aS = m%xdNew%axialSection(:,k)
   call VBSC_BuildDerived(p, k, rE, aE, rS, aS, m)
end subroutine VBSC_EnsureDerived

subroutine VBSC_Refresh(p, k, rEdgeNow, axialEdgeNow, rSecNow, axialSecNow, m, ErrStat, ErrMsg)
   type(SC_ParameterType),     intent(in   ) :: p
   integer(IntKi),             intent(in   ) :: k
   real(R8Ki),                 intent(in   ) :: rEdgeNow(:), axialEdgeNow(:), rSecNow(:), axialSecNow(:)
   type(SC_MiscVarType),       intent(inout) :: m
   integer(IntKi),             intent(  out) :: ErrStat
   character(*),               intent(  out) :: ErrMsg
   integer(IntKi) :: ne, ns, e, s, c, qNew, qOld
   real(R8Ki)     :: moved
   logical        :: wasBuilt
   integer(IntKi) :: oldFirst(p%ne), oldCount(p%ne), oldBegin(p%ne+1)
   real(R8Ki)     :: oldSlow(size(m%xdNew%slowWake,1)), oldFast(size(m%xdNew%fastWake,1))

   ErrStat = ErrID_None
   ErrMsg  = ""
   ne = p%ne
   ns = p%ns
   wasBuilt = m%xdNew%rEdge(ne,k) > 0.0_R8Ki

   if (wasBuilt) then
      call VBSC_EnsureDerived(p, k, m)
      moved = 0.0_R8Ki
      do e = 1, ne
         moved = max(moved, abs(rEdgeNow(e) - m%xdNew%rEdge(e,k)))
         moved = max(moved, abs(axialEdgeNow(e) - m%xdNew%axialEdge(e,k)))
      end do
      do s = 1, ns
         moved = max(moved, abs(rSecNow(s) - m%xdNew%rSection(s,k)))
         moved = max(moved, abs(axialSecNow(s) - m%xdNew%axialSection(s,k)))
      end do
      if (.not. (moved > 0.0_R8Ki)) return
      m%rebuilds = m%rebuilds + 1
      oldFirst = m%secFirst(:,k)
      oldCount = m%secCount(:,k)
      oldBegin = m%pairBegin(:,k)
      oldSlow  = m%xdNew%slowWake(:,k)
      oldFast  = m%xdNew%fastWake(:,k)
   end if

   if (rEdgeNow(1) < VB_MinRootRadius) then
      ErrStat = ErrID_Fatal
      ErrMsg  = 'VBSC_Refresh: the innermost trailing point is on the rotor axis, the near-wake model is singular there.'
      return
   end if
   do e = 2, ne
      if (.not. (rEdgeNow(e) > rEdgeNow(e-1))) then
         ErrStat = ErrID_Fatal
         ErrMsg  = 'VBSC_Refresh: the trailing-point radius must increase from root to tip.'
         return
      end if
   end do

   m%xdNew%rEdge(:,k)        = rEdgeNow
   m%xdNew%axialEdge(:,k)    = axialEdgeNow
   m%xdNew%rSection(:,k)     = rSecNow
   m%xdNew%axialSection(:,k) = axialSecNow
   call VBSC_BuildDerived(p, k, rEdgeNow, axialEdgeNow, rSecNow, axialSecNow, m)

   m%xdNew%slowWake(:,k) = 0.0_R8Ki
   m%xdNew%fastWake(:,k) = 0.0_R8Ki
   if (wasBuilt) then
      do e = 1, ne
         do c = 0, m%secCount(e,k) - 1
            s = m%secFirst(e,k) + c
            if (s < oldFirst(e) .or. s > oldFirst(e) + oldCount(e) - 1) cycle
            qNew = m%pairBegin(e,k) + c
            qOld = oldBegin(e) + (s - oldFirst(e))
            m%xdNew%slowWake(qNew,k) = oldSlow(qOld)
            m%xdNew%fastWake(qNew,k) = oldFast(qOld)
         end do
      end do
   end if
end subroutine VBSC_Refresh

function VBSC_Lift(p, k, s, alpha) result(cl)
   type(SC_ParameterType), intent(in) :: p
   integer(IntKi),         intent(in) :: k, s
   real(R8Ki),             intent(in) :: alpha
   real(R8Ki)                         :: cl
   integer(IntKi) :: b, e, lo, hi, mid
   b = p%liftOffset((k-1)*p%ns + s)
   e = p%liftOffset((k-1)*p%ns + s + 1) - 1
   if (alpha <= p%liftAlpha(b)) then
      cl = p%liftCl(b); return
   end if
   if (alpha >= p%liftAlpha(e)) then
      cl = p%liftCl(e); return
   end if
   lo = b
   hi = e
   do while (hi - lo > 1)
      mid = (lo + hi) / 2
      if (p%liftAlpha(mid) <= alpha) then
         lo = mid
      else
         hi = mid
      end if
   end do
   cl = p%liftCl(lo) + (alpha - p%liftAlpha(lo)) / (p%liftAlpha(lo+1) - p%liftAlpha(lo)) * (p%liftCl(lo+1) - p%liftCl(lo))
end function VBSC_Lift

subroutine SC_VBSC_Update(tNew, dt, p, hub, axis, pos, solid, wind, nrm, chd, m, ErrStat, ErrMsg)
   real(DbKi),             intent(in   ) :: tNew
   real(DbKi),             intent(in   ) :: dt
   type(SC_ParameterType), intent(in   ) :: p
   real(R8Ki),             intent(in   ) :: hub(3), axis(3)
   real(R8Ki),             intent(in   ) :: pos(:,:,:), solid(:,:,:), wind(:,:,:), nrm(:,:,:), chd(:,:,:)
   type(SC_MiscVarType),   intent(inout) :: m
   integer(IntKi),         intent(  out) :: ErrStat
   character(*),           intent(  out) :: ErrMsg

   integer(IntKi) :: n, nb, ns, ne, k, j, s, e, lo
   real(R8Ki)     :: sense, rotSign, tt, helix, dtl
   real(R8Ki)     :: ps(3), sv(3), wv(3), rel(3), na(3), ca(3), arm(3), radial(3), tg(3), d(3)
   real(R8Ki)     :: axisX(p%ns), axisY(p%ns), tanX(p%ns), tanY(p%ns), windX(p%ns), windY(p%ns)
   real(R8Ki)     :: windAx(p%ns), windTan(p%ns), helixSec(p%ns), rSecNow(p%ns), axialSecNow(p%ns)
   real(R8Ki)     :: rEdgeNow(p%ne), axialEdgeNow(p%ne)
   real(R8Ki)     :: tangent(3,p%ns), corr(3,p%ns)
   logical        :: anyUnconv

   ErrStat = ErrID_None
   ErrMsg  = ""

   if (tNew < p%Start) then
      m%duNew = 0.0_R8Ki
      return
   end if

   n  = p%NumBlNds
   nb = p%NumBlades
   ns = p%ns
   ne = p%ne
   dtl = real(dt, R8Ki)

   sense = 0.0_R8Ki
   do k = 1, nb
      do j = 1, n
         sense = sense + dot_product(solid(:,j,k), Cross_Product(axis, pos(:,j,k) - hub))
      end do
   end do
   rotSign = 1.0_R8Ki
   if (sense < 0.0_R8Ki) rotSign = -1.0_R8Ki

   anyUnconv = .false.
   do k = 1, nb
      do e = 1, ne
         d = pos(:,e,k) - hub
         axialEdgeNow(e) = dot_product(d, axis)
         rEdgeNow(e)     = TwoNorm(d - axialEdgeNow(e) * axis)
      end do
      do s = 1, ns
         tt = p%secT(s,k)
         d = pos(:,s,k) + tt * (pos(:,s+1,k) - pos(:,s,k)) - hub
         axialSecNow(s) = dot_product(d, axis)
         rSecNow(s)     = TwoNorm(d - axialSecNow(s) * axis)
      end do
      call VBSC_Refresh(p, k, rEdgeNow, axialEdgeNow, rSecNow, axialSecNow, m, ErrStat, ErrMsg)
      if (ErrStat >= AbortErrLev) return
      call VBSC_EnsureDerived(p, k, m)

      do s = 1, ns
         tt = p%secT(s,k)
         ps = pos(:,s,k)   + tt * (pos(:,s+1,k)   - pos(:,s,k))
         sv = solid(:,s,k) + tt * (solid(:,s+1,k) - solid(:,s,k))
         wv = wind(:,s,k)  + tt * (wind(:,s+1,k)  - wind(:,s,k))
         rel = wv - sv
         na = Normalised((1.0_R8Ki - tt) * nrm(:,s,k) + tt * nrm(:,s+1,k))
         ca = Normalised((1.0_R8Ki - tt) * chd(:,s,k) + tt * chd(:,s+1,k))
         arm = ps - hub
         radial = Normalised(arm - dot_product(arm, axis) * axis)
         tg = rotSign * Cross_Product(axis, radial)
         tangent(:,s) = tg
         axisX(s)   = dot_product(axis, na)
         axisY(s)   = dot_product(axis, ca)
         tanX(s)    = dot_product(tg, na)
         tanY(s)    = dot_product(tg, ca)
         windX(s)   = dot_product(rel, na)
         windY(s)   = dot_product(rel, ca)
         windAx(s)  = dot_product(rel, axis)
         windTan(s) = dot_product(rel, tg)
         helix = atan2(windAx(s), -windTan(s))
         if (helix > VB_HalfPi) helix = VB_Pi - helix
         helixSec(s) = helix
      end do

      call SolveBlade(k)

      do s = 1, ns
         corr(:,s) = m%xdNew%vn(s,k) * axis + m%xdNew%vt(s,k) * tangent(:,s)
      end do
      do j = 1, n
         lo = p%velLo(j,k)
         m%duNew(:,j,k) = corr(:,lo) + p%velT(j,k) * (corr(:,lo+1) - corr(:,lo))
      end do
   end do
   m%xdNew%du = m%duNew
   if (anyUnconv) m%unconvSteps = m%unconvSteps + 1
   m%nUpdates = m%nUpdates + 1

contains

   function EffectivePhi(q, hx, complement, complementDecay) result(phi)
      integer(IntKi), intent(in) :: q
      real(R8Ki),     intent(in) :: hx, complement, complementDecay
      real(R8Ki)                 :: phi, mix
      if (m%radiusRatio(q,k) < 0.0_R8Ki) then
         mix = m%helixFit(1,q,k) + m%helixFit(2,q,k) * exp(m%helixFit(3,q,k) * complement) + m%helixFit(4,q,k) * complementDecay &
               - m%helixFit(2,q,k) - m%helixFit(4,q,k)
      else
         mix = m%helixFit(1,q,k) + hx * (m%helixFit(2,q,k) + hx * (m%helixFit(3,q,k) + hx * m%helixFit(4,q,k)))
      end if
      mix = min(1.0_R8Ki, max(0.0_R8Ki, mix))
      phi = mix * m%straightPhi(q,k) + (1.0_R8Ki - mix) * m%circPhi(q,k)
   end function EffectivePhi

   subroutine SolveBlade(k)
      integer(IntKi), intent(in) :: k
      real(R8Ki) :: helixEdge(p%ne), cosHE(p%ne), sinHE(p%ne), advance(p%ne), trailed(p%ne), turnBase(p%ne), steepest(p%ne)
      real(R8Ki) :: speed(p%ns), steadyNow(p%ns), circ(p%ns), iter(p%ns), iterN(p%ns), iterT(p%ns), cosH(p%ns), sinH(p%ns)
      real(R8Ki) :: startN(p%ns), startT(p%ns), startW(p%ns), stepScale(p%ns), stepsBase(p%ns)
      real(R8Ki) :: filterNow(3,p%ns), filterDecayBase(3,p%ns)
      real(R8Ki) :: turnRate(size(m%circPhi,1)), slowGain(size(m%circPhi,1)), fastGain(size(m%circPhi,1))
      real(R8Ki) :: smearing(size(m%circPhi,1)), slowNow(size(m%circPhi,1)), fastNow(size(m%circPhi,1)), decayBase(size(m%circPhi,1))
      real(R8Ki) :: hx, cos2, complement, complementDecay, phi, steep, wx, wy, sp, alpha, steps, stepShift, x, decay, dyn
      real(R8Ki) :: normalSpeed, tangentialSpeed, total, w, strength, turn, shift, slow, slow2, fast, slowTerm, fastTerm, part
      real(R8Ki) :: relax, residual, projection
      integer(IntKi) :: e, s, q, q0, q1, i, iteration, nPairs, c
      logical :: tipInPlane, refresh, converged

      nPairs = m%pairBegin(p%ne+1,k) - 1

      helixEdge(1)    = helixSec(1)
      helixEdge(p%ne) = helixSec(p%ns)
      do e = 2, p%ne - 1
         helixEdge(e) = helixSec(e-1) + m%edgeSecW(e,k) * (helixSec(e) - helixSec(e-1))
      end do

      do e = 1, p%ne
         hx = helixEdge(e)
         cosHE(e) = cos(hx)
         cos2 = cosHE(e) * cosHE(e)
         sinHE(e) = sin(hx)
         complement = VB_HalfPi - hx
         complementDecay = exp(-8.0_R8Ki * complement)
         steep = 0.0_R8Ki
         do q = m%pairBegin(e,k), m%pairBegin(e+1,k) - 1
            phi = EffectivePhi(q, hx, complement, complementDecay)
            turnRate(q) = -1.0_R8Ki / phi
            steep = max(steep, 1.0_R8Ki / phi)
            slowGain(q) = m%slowAmp(q,k) * phi
            fastGain(q) = m%fastAmp(q,k) * phi
            if (m%axialOff2(q,k) > 0.0_R8Ki) then
               smearing(q) = m%planarSmear(q,k) * exp(-m%axialOff2(q,k) * cos2)
            else
               smearing(q) = m%planarSmear(q,k)
            end if
         end do
         steepest(e) = steep
      end do

      startN = m%xdNew%vn(:,k)
      startT = m%xdNew%vt(:,k)
      startW = m%xdNew%wake(:,k)
      do s = 1, p%ns
         stepScale(s) = dtl * 2.0_R8Ki / p%secChord(s,k)
      end do
      stepsBase = 0.0_R8Ki
      filterDecayBase = 0.0_R8Ki
      turnBase = 0.0_R8Ki

      tipInPlane = helixEdge(p%ne) < 0.25_R8Ki * VB_Pi
      relax = 0.0_R8Ki
      refresh = .true.
      converged = .false.
      iteration = 0

      do
         do s = 1, p%ns
            wx = windX(s) + m%xdNew%vn(s,k) * axisX(s) + m%xdNew%vt(s,k) * tanX(s)
            wy = windY(s) + m%xdNew%vn(s,k) * axisY(s) + m%xdNew%vt(s,k) * tanY(s)
            sp = sqrt(wx * wx + wy * wy)
            alpha = atan2(wx, wy)
            speed(s) = sp
            m%xdNew%attack(s,k) = alpha
            steadyNow(s) = 0.5_R8Ki * sp * p%secChord(s,k) * VBSC_Lift(p, k, s, alpha)

            if (p%CircFilter) then
               steps = stepScale(s) * sp
               stepShift = steps - stepsBase(s)
               if (iteration == 0 .or. .not. (abs(stepShift) * VB_FilterRate(3) < VB_SeriesLimit)) then
                  stepsBase(s) = steps
                  stepShift = 0.0_R8Ki
                  do i = 1, 3
                     filterDecayBase(i,s) = exp(-VB_FilterRate(i) * steps)
                  end do
               end if
               dyn = 0.0_R8Ki
               do i = 1, 3
                  x = -VB_FilterRate(i) * stepShift
                  decay = filterDecayBase(i,s) * (1.0_R8Ki + x * (1.0_R8Ki + x * (0.5_R8Ki + x * (1.0_R8Ki / 6.0_R8Ki + &
                          x * (1.0_R8Ki / 24.0_R8Ki + x * (1.0_R8Ki / 120.0_R8Ki))))))
                  filterNow(i,s) = m%xdNew%filter(i,s,k) * decay + VB_FilterWeight(i) * (steadyNow(s) + m%xdNew%steady(s,k)) * (1.0_R8Ki - decay)
                  dyn = dyn + filterNow(i,s)
               end do
               circ(s) = dyn
            else
               filterNow(:,s) = 0.0_R8Ki
               circ(s) = steadyNow(s)
            end if
            iter(s)  = 0.0_R8Ki
            iterN(s) = 0.0_R8Ki
            iterT(s) = 0.0_R8Ki

            normalSpeed     = windAx(s) + m%xdNew%vn(s,k)
            tangentialSpeed = -(windTan(s) + m%xdNew%vt(s,k))
            total = sqrt(normalSpeed * normalSpeed + tangentialSpeed * tangentialSpeed)
            if (total > 0.0_R8Ki) then
               cosH(s) = tangentialSpeed / total
               sinH(s) = normalSpeed / total
            else
               cosH(s) = 1.0_R8Ki
               sinH(s) = 0.0_R8Ki
            end if
         end do

         advance(1)    = speed(1) * dtl / m%xdNew%rEdge(1,k)
         advance(p%ne) = speed(p%ns) * dtl / m%xdNew%rEdge(p%ne,k)
         trailed(1)    = circ(1)
         trailed(p%ne) = -circ(p%ns)
         do e = 2, p%ne - 1
            w = m%edgeSecW(e,k)
            advance(e) = (speed(e-1) + w * (speed(e) - speed(e-1))) * dtl / m%xdNew%rEdge(e,k)
            trailed(e) = circ(e) - circ(e-1)
         end do

         do e = 1, p%ne
            strength = trailed(e)
            turn = advance(e)
            q0 = m%pairBegin(e,k)
            q1 = m%pairBegin(e+1,k) - 1
            shift = turn - turnBase(e)
            if (iteration == 0 .or. .not. (abs(shift) * steepest(e) < VB_SeriesLimit)) then
               turnBase(e) = turn
               shift = 0.0_R8Ki
               do q = q0, q1
                  decayBase(q) = exp(turn * turnRate(q))
               end do
            end if
            do c = 0, q1 - q0
               q = q0 + c
               s = m%secFirst(e,k) + c
               x = shift * turnRate(q)
               slow = decayBase(q) * (1.0_R8Ki + x * (1.0_R8Ki + x * (0.5_R8Ki + x * (1.0_R8Ki / 6.0_R8Ki + &
                      x * (1.0_R8Ki / 24.0_R8Ki + x * (1.0_R8Ki / 120.0_R8Ki))))))
               slow2 = slow * slow
               fast = slow2 * slow2
               slowTerm = strength * slowGain(q) * (1.0_R8Ki - slow) + m%xdNew%slowWake(q,k) * slow
               fastTerm = strength * fastGain(q) * (1.0_R8Ki - fast) + m%xdNew%fastWake(q,k) * fast
               slowNow(q) = slowTerm
               fastNow(q) = fastTerm
               part = smearing(q) * (slowTerm + fastTerm)
               iter(s)  = iter(s)  + part
               iterN(s) = iterN(s) + part * cosHE(e)
               iterT(s) = iterT(s) + part * sinHE(e)
            end do
         end do

         if (refresh) then
            relax = RelaxationFactor(k, advance)
            refresh = .false.
         end if

         residual = 0.0_R8Ki
         do s = 1, p%ns
            iter(s) = relax * m%xdNew%wake(s,k) + (1.0_R8Ki - relax) * iter(s)
            if (tipInPlane) then
               projection = cosH(s)
            else
               projection = sinH(s)
            end if
            residual = max(residual, abs(iter(s) - m%xdNew%wake(s,k)) * projection)
         end do

         iteration = iteration + 1
         if (iteration == VB_RelaxRefresh) then
            refresh = .true.
            if (.not. (residual < real(p%MaxResid, R8Ki))) call RaiseSafety()
         end if
         if (iteration == VB_MaxIterations) then
            call RaiseSafety()
            exit
         end if

         do s = 1, p%ns
            m%xdNew%wake(s,k) = iter(s)
            m%xdNew%vn(s,k) = relax * m%xdNew%vn(s,k) + (1.0_R8Ki - relax) * iterN(s)
            m%xdNew%vt(s,k) = relax * m%xdNew%vt(s,k) + (1.0_R8Ki - relax) * iterT(s)
         end do
         if (residual < real(p%MaxResid, R8Ki)) then
            converged = .true.
            exit
         end if
      end do

      m%maxIter = max(m%maxIter, iteration)
      m%sumIter = m%sumIter + iteration
      m%nSolves = m%nSolves + 1
      if (.not. converged) then
         m%unconvSolves = m%unconvSolves + 1
         anyUnconv = .true.
         m%xdNew%vn(:,k)   = startN
         m%xdNew%vt(:,k)   = startT
         m%xdNew%wake(:,k) = startW
         return
      end if

      m%xdNew%filter(:,:,k) = filterNow
      m%xdNew%steady(:,k)   = steadyNow
      m%xdNew%slowWake(1:nPairs,k) = slowNow(1:nPairs)
      m%xdNew%fastWake(1:nPairs,k) = fastNow(1:nPairs)
   end subroutine SolveBlade

   function RelaxationFactor(k, advance) result(relax)
      integer(IntKi), intent(in) :: k
      real(R8Ki),     intent(in) :: advance(:)
      real(R8Ki) :: relax, wx, wy, sp, alpha, inflow, normalSpeed, tangentialSpeed, tau, lag, ratio, gradient, own(2), phi, turn
      integer(IntKi) :: s, i
      relax = 0.0_R8Ki
      do s = 1, p%ns
         wx = windX(s) + m%xdNew%vn(s,k) * axisX(s) + m%xdNew%vt(s,k) * tanX(s)
         wy = windY(s) + m%xdNew%vn(s,k) * axisY(s) + m%xdNew%vt(s,k) * tanY(s)
         sp = hypot(wx, wy)
         if (.not. (sp > 0.0_R8Ki)) cycle
         alpha = atan2(wx, wy)
         inflow = alpha + p%secTwist(s,k)
         normalSpeed     = sp * sin(inflow)
         tangentialSpeed = sp * cos(inflow)
         tau = 2.0_R8Ki * sp / p%secChord(s,k) * dtl
         lag = 1.0_R8Ki
         if (p%CircFilter) then
            do i = 1, 3
               lag = lag - VB_LagWeight(i) / (tau * VB_FilterRate(i)) * (1.0_R8Ki - exp(-tau * VB_FilterRate(i)))
            end do
         end if
         do i = 1, 2
            phi  = m%ownPhi(i,s,k)
            turn = advance(s + i - 1)
            own(i) = m%ownSlow(i,s,k) * phi * (1.0_R8Ki - exp(-turn / phi)) + m%ownFast(i,s,k) * phi * (1.0_R8Ki - exp(-4.0_R8Ki * turn / phi))
         end do
         ratio = normalSpeed / abs(tangentialSpeed)
         gradient = lag * VB_Pi * p%secChord(s,k) * (own(1) - own(2)) * &
                    (alpha * normalSpeed / sp + sp / (abs(tangentialSpeed) * (ratio * ratio + 1.0_R8Ki)))
         relax = max(relax, -(1.0_R8Ki + gradient) / (1.0_R8Ki - gradient))
      end do
      relax = relax + m%xdNew%relaxSafety * (1.0_R8Ki - relax)
      relax = min(relax, VB_RelaxMax)
   end function RelaxationFactor

   subroutine RaiseSafety()
      if (m%xdNew%relaxSafety < VB_RelaxSafetyLimit) m%xdNew%relaxSafety = m%xdNew%relaxSafety + VB_RelaxSafetyStep
   end subroutine RaiseSafety

end subroutine SC_VBSC_Update

function Normalised(a) result(b)
   real(R8Ki), intent(in) :: a(3)
   real(R8Ki)             :: b(3), nn
   nn = sqrt(dot_product(a, a))
   if (nn > 0.0_R8Ki) then
      b = a / nn
   else
      b = a
   end if
end function Normalised

end module SmearCorr

module global
  use declrs



  character(125) :: str, str1

  character(MAX_FILE_LEN) :: datapath

  type(surfaces), allocatable, target :: tsurf(:), surf(:),trsrf(:)
  type(surfaces),             pointer :: sp => null()
  type(cells),   allocatable, target :: tcell(:), cell(:)
  type(cells),               pointer :: cp => null()
  type(universes), allocatable, target :: univ(:)
  type(universes), pointer :: up => null()
  type(lattices), allocatable, target :: latt(:)
  type(lattices), pointer :: lp => null()
!  type(XsListing), allocatable, target :: xslists(:)
  type(Material), allocatable, target :: mats(:)
  type(Nuclide), allocatable, target :: nucs(:)
  type(SAlphaBeta), allocatable, target :: sabs(:)

  type istore
    integer :: t
    integer,allocatable :: i(:)
  end type

  type cstore
    integer :: t
    character(15),allocatable :: c(:)
  end type

  ! ============================================================================
  ! EIGENVALUE SIMULATION VARIABLES

  integer(8) :: n_particles = 0   ! # of particles per generation
  integer    :: n_batcheses         ! # of batches
  integer    :: n_inactive        ! # of inactive batches
  integer    :: n_active          ! # of active batches

  type(istore) :: cel_store
  type(istore) :: uni_store
  type(istore) :: lat_store

  type(cstore) :: nuc_store
  type(cstore) :: sab_store

  integer :: tmp_surf, t_cell, nsrf, ncel, nuni, nlat, nmat, nksrc, nxsl, nnuc, &
             n_skip, n_batches, nsab

  integer :: run_mode = NONE

  integer(8) :: isource, aa, ll

  real(8), allocatable :: ksrc(:,:)
  integer, allocatable :: dimp(:)

  contains

  function sabstore_ix(name) result(ix)
    character(*) :: name
    integer :: j, ix
    ix = -huge(0)
    do j = 1, sab_store%t
      if(sab_store%c(j) == sset(name))then
        ix = j ; exit
      end if
    end do
  end function

  function nucstore_ix(name) result(ix)
    character(*) :: name
    integer :: j, ix
    ix = -huge(0)
    do j = 1, nuc_store%t
      if(nuc_store%c(j) == sset(name))then
        ix = j ; exit
      end if
    end do
  end function

!  function xsl_ix(name) result(ix)
!    character(*) :: name
!    integer :: j, ix
!    ix = -huge(0)
!    do j = 1, nxsl
!      if(xslists(j)%name ==sset(name))then
!        ix = j ; exit
!      end if
!    end do
!
!    if(ix == -huge(0))call fatal(name//' does not exists')
!
!  end function

  function mat_ix(i) result(ix)
    integer :: i,j, ix
    ix = -huge(0)
    do j = 1, nmat
      if(mats(j)%i == i)then
        ix = j ; exit
      end if
    end do
  end function

  function lat_ix(i) result(ix)
    integer :: i,j, ix
    ix = -huge(0)
    do j=1,nlat
      if(latt(j)%i==i)then
        ix = j ; exit
      end if
    end do
    if(ix == -huge(0)) call fatal('lattice id "'//sset(to_str(i))//'" ix error')
  end function

  function uni_ix(i) result(ix)
    integer :: i,j, ix
    ix = -huge(0)
    do j=1,nuni
      if(univ(j)%i==i)then
        ix = j ; exit
      end if
    end do
    if(ix == -huge(0)) call fatal('Universe id "'//sset(to_str(i))//'"ix error')
  end function

  function sur_ix(i) result(ix)
    integer ::  i, ix, j
    ix = 0
    do j = 1, tmp_surf
      if(tsurf(j)%i==i)then
        ix = j ; exit
      end if
    end do
    if(ix == 0)call fatal('Surface "'//sset(to_str(i))//'"  not found -DEV1')
  end function

  function sur_ixf(i) result(ix)
    integer ::  i, ix, j
    ix = 0
    do j = 1, nsrf
      if(surf(j)%i==i)then
        ix = j ; exit
      end if
    end do
    if(ix == 0)call fatal('Surface "'//sset(to_str(i))//'"  not found - DEV')
  end function

  function cel_ix(i) result(ix)
    integer ::  i, ix, j
    ix = 0
    do j = 1, t_cell
      if(tcell(j)%i==i)then
        ix = j ; exit
      end if
    end do
    if(ix == 0)call warn('Cel_ix not found --DEV-- '//sset(to_str(i)))
  end function

end module

module constants
  implicit none

  integer, parameter ::  inp = 18 , ou =6

  integer, parameter :: xsunit = 18
  real(8), parameter :: ZERO = 0.0_8
  ! ACE table types
  integer, parameter :: &
       ACE_NEUTRON   = 1, & ! continuous-energy neutron
       ACE_THERMAL   = 2    ! thermal S(a,b) scattering data

  integer, parameter :: NONE = 0

  integer, parameter ::    &
    MAX_WORDS       = 1000, &
    MAX_LINE_LEN    = 250, &
    MAX_WORD_LEN    = 150, &
    MAX_FILE_LEN    = 255

  logical :: dev
  logical :: fatalstop

  integer, parameter ::    &
      P_P   = 1, &
      P_PX  = 2, &
      P_PY  = 3, &
      P_PZ  = 4, &
      S_O   = 5, &
      S_S   = 6, &
      S_SX  = 7, &
      S_SY  = 8, &
      S_SZ  = 9, &
      C_CXX = 10, &
      C_CYY = 11, &
      C_CZZ = 12, &
      C_CX  = 13, &
      C_CY  = 14, &
      C_CZ  = 15
!
  integer, parameter ::  &
    BC_TRANSMIT = 0,  & ! Transmission boundary condition (default)
    BC_VACUUM   = 1,  & ! Vacuum boundary condition
    BC_REFLECT  = 2,  & ! Reflecting boundary condition
    BC_PERIODIC = 3,  & ! Periodic boundary condition
    BC_WHITE    = 4     ! Reflect particle with random direction

  integer, parameter :: SRF = 1
  integer, parameter :: CEL = 2

  integer, parameter :: MAT_VOID = 0


  integer, parameter ::   &
       OP_LEFT_PAREN   = huge(0),     & ! Left parentheses          7
       OP_RIGHT_PAREN  = huge(0) - 1, & ! Right parentheses         6
       OP_COMPLEMENT   = huge(0) - 2, & ! Complement operator (#)   5
       OP_INTERSECTION = huge(0) - 3, & ! Intersection operator     4
       OP_UNION        = huge(0) - 4    ! Union operator (:)        3

  ! Cell types
  integer, parameter ::  &
       CELL_NORMAL  = 1, & ! Cell with a specified material
       CELL_FILL    = 2, & ! Cell filled by a separate universe
       CELL_LATTICE = 3    ! Cell filled with a lattice

  ! Particle type
  integer, parameter :: &
       NEUTRON  = 1, &
       PHOTON   = 2, &
       ELECTRON = 3

end module

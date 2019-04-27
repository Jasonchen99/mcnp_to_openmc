module declrs
  use constants
  use general

!===============================================================================
! MATERIAL describes a material by its constituent nuclides
!===============================================================================

  type Material
    integer              :: i               ! unique identifier
    character(len=15)    :: name = ""       ! User-defined name
    integer              :: n_nuclides      ! number of nuclides
    integer, allocatable :: nuclide(:)      ! index in nuclides array
    real(8)              :: density         ! total atom density in atom/b-cm
    real(8), allocatable :: atom_density(:) ! nuclide atom density in atom/b-cm

    ! S(a,b) data references
    integer              :: n_sab = 0         ! number of S(a,b) tables
    integer, allocatable :: i_sab_nuclides(:) ! index of corresponding nuclide
    integer, allocatable :: i_sab_tables(:)   ! index in sab_tables

    ! Temporary names read during initialization
    character(15), allocatable :: names(:)     ! isotope names
    character(15), allocatable :: sab_names(:) ! name of S(a,b) table

  end type Material


 type :: Nuclide
    ! Nuclide meta-data
    character(15) :: name    ! name of nuclide, e.g. 92235.03c
    integer       :: zaid    ! Z and A identifier, e.g. 92235
    real(8)       :: awr     ! Atomic Weight Ratio
    integer       :: listing ! index in xs_listings
    real(8)       :: kT      ! temperature in MeV (k*T)

    ! Fission information
    logical :: fissionable   ! nuclide is fissionable?

    ! Energy grid information
    integer :: n_grid                     ! # of nuclide grid points
    integer, allocatable :: grid_index(:) ! log grid mapping indices
    real(8), allocatable :: energy(:)     ! energy values corresponding to xs

    ! Microscopic cross sections
    real(8), allocatable :: total(:)      ! total cross section
    real(8), allocatable :: elastic(:)    ! elastic scattering
    real(8), allocatable :: fission(:)    ! fission
    real(8), allocatable :: nu_fission(:) ! neutron production
    real(8), allocatable :: absorption(:) ! absorption (MT > 100)
    real(8), allocatable :: heating(:)    ! heating

    ! Resonance scattering info
    logical              :: resonant = .false. ! resonant scatterer?
    character(10)        :: name_0K = '' ! name of 0K nuclide, e.g. 92235.00c
    character(16)        :: scheme ! target velocity sampling scheme
    integer              :: n_grid_0K ! number of 0K energy grid points
    real(8), allocatable :: energy_0K(:)  ! energy grid for 0K xs
    real(8), allocatable :: elastic_0K(:) ! Microscopic elastic cross section
    real(8), allocatable :: xs_cdf(:) ! CDF of v_rel times cross section
    real(8)              :: E_min ! lower cutoff energy for res scattering
    real(8)              :: E_max ! upper cutoff energy for res scattering

    ! Fission information
    logical :: has_partial_fission = .false. ! nuclide has partial fission reactions?
    integer :: n_fission                     ! # of fission reactions
    integer :: n_precursor = 0               ! # of delayed neutron precursors
    integer, allocatable :: index_fission(:) ! indices in reactions

    ! Unresolved resonance data
    logical                :: urr_present
    integer                :: urr_inelastic

    ! Reactions
    integer :: n_reaction ! # of reactions

  end type Nuclide
!===============================================================================
! SALPHABETA contains S(a,b) data for thermal neutron scattering, typically off
! of light isotopes such as water, graphite, Be, etc
!===============================================================================

  type SAlphaBeta
    character(10) :: name     ! name of table, e.g. lwtr.10t
    real(8)       :: awr      ! weight of nucleus in neutron masses
    real(8)       :: kT       ! temperature in MeV (k*T)
    integer       :: n_zaid   ! Number of valid zaids
    integer, allocatable :: zaid(:) ! List of valid Z and A identifiers, e.g. 6012
  end type SAlphaBeta

  type surfaces
    integer :: i, ityp, ncf, peri, ix, bc, nposi, nnega
    real(8), allocatable :: coef(:)
    integer, allocatable :: npos(:),nneg(:)
    character(3) :: ctyp
  end type

  type cells
    integer ::  i, typ, fil, uni, lat, nrgn, nrpn, imp, mat, like, ncrgn, tr
    integer, allocatable :: rgn(:), rpn(:)
    real(8) :: den, trcl(3)
    real(8), allocatable :: atom_density(:)
    logical :: ltr, prcd, simple
    character(:), allocatable :: crgn, latfill, for_imp_zero_with_comp(:)
  end type

  type universes
    integer :: i, ncel
    integer, allocatable :: cell(:)
  end type

  type lattices
    integer :: i  ! lat=1 or 2   u=i number here
    real(8) :: width(3) ! x2-x1 y2-y1 z2-z1
    logical :: pattern
    integer :: fil     ! fill number  fill= U or LAT
    integer :: ftyp    ! fill number is (UNIV ftyp = 1 or LATT ftyp = 2)
    integer, allocatable :: fils(:,:,:)  ! fill=0:0 0:0 0:0 1 2 3 4
    integer :: ixl, ixu, iyl, iyu, izl, izu ! 1:2 3:4 5:6
    integer :: typ ! rect 1 hex 2
    real(8) :: xpls, xmin, ypls, ymin, zpls, zmin
  end type

  integer :: BASE_UNIV_INDEX

  contains

  subroutine surfid(chr,typ,ncfs)
    character(3), intent(inout) :: chr
    integer :: typ, ncfs
    selectcase(chr)
      case('p')   ; typ = P_P   ; ncfs = 4
      case('px')  ; typ = P_PX  ; ncfs = 1
      case('py')  ; typ = P_PY  ; ncfs = 1
      case('pz')  ; typ = P_PZ  ; ncfs = 1
      case('so')  ; typ = S_O   ; ncfs = 1
      case('s')   ; typ = S_S   ; ncfs = 4
      case('sx')  ; typ = S_SX  ; ncfs = 2
      case('sy')  ; typ = S_SY  ; ncfs = 2
      case('sz')  ; typ = S_SZ  ; ncfs = 2
      case('c/x') ; typ = C_CXX ; ncfs = 3
      case('c/y') ; typ = C_CYY ; ncfs = 3
      case('c/z') ; typ = C_CZZ ; ncfs = 3
      case('cx')  ; typ = C_CX  ; ncfs = 1
      case('cy')  ; typ = C_CY  ; ncfs = 1
      case('cz')  ; typ = C_CZ  ; ncfs = 1
      case default
        call fatal(' Undefined surface type "'//chr//'"')
        typ = 0 ; ncfs = 0
    end select
  end subroutine
!===============================================================================
! TOKENIZE takes a string that includes logical expressions for a list of
! bounding surfaces in a cell and splits it into separate tokens. The characters
! (, ), |, and ~ count as separate tokens since they represent operators.
!===============================================================================
!
  subroutine tokenize(string, tokens, ntk)
    character(*), intent(in) :: string
    integer,   intent(inout) :: tokens(:),ntk

    integer :: i       ! current index
    integer :: j       ! size of tokens
    integer :: i_start ! starting index of word
    integer :: token
    character(len=len_trim(string)) :: string_

    ! Remove leading blanks
    string_ = adjustl(string)

    i_start = 0
    i = 1
    j = 0
    do while (i <= len_trim(string_))
      ! Check for special characters
      if (index('():#~| ', string_(i:i)) > 0) then
        ! If the special character appears immediately after a non-operator,
        ! create a token with the surface half-space
        if (i_start > 0) then
          j = j + 1
          tokens(j) = int(to_int(string_(i_start:i - 1)))
        end if

        select case (string_(i:i))
        case ('(')
          j = j + 1
          tokens(j) = OP_LEFT_PAREN
        case (')')
          if (j > 0) then
            token = tokens(j)
            if (token >= OP_UNION .and. token < OP_RIGHT_PAREN) then
              call fatal("Right parentheses cannot follow an operator in &
                   &region specification: " // trim(string))
            end if
          end if
          j = j + 1
          tokens(j) = OP_RIGHT_PAREN
        case (':','|')
          if (j > 0) then
            token = tokens(j)
            if (.not. (token < OP_UNION .or. token == OP_RIGHT_PAREN)) then
              call fatal("Union cannot follow an operator in region &
                   &specification: " // trim(string))
            end if
          end if
          j = j + 1
          tokens(j) = OP_UNION
        case ('#','~')
          j = j + 1
          tokens(j) = OP_COMPLEMENT
        case (' ')
          ! Find next non-space character
          do while (string_(i+1:i+1) == ' ')
            i = i + 1
          end do

          ! If previous token is a halfspace or right parenthesis and next token
          ! is not a left parenthese or union operator, that implies that the
          ! whitespace is to be interpreted as an intersection operator
          if (i_start > 0 .or. tokens(j) == OP_RIGHT_PAREN) then
            if (index('):)|', string_(i+1:i+1)) == 0) then
              j = j + 1
              tokens(j) = OP_INTERSECTION
            end if
          end if
        end select

        i_start = 0
      else
        ! Check for invalid characters
        if (index('-+0123456789', string_(i:i)) == 0) then
          call fatal("Invalid character '" // string_(i:i) // "' in &
               &region specification."//string_)
          print*,'--',trim(string_),'--'
          stop 127
        end if

        ! If we haven't yet reached the start of a word, start a new word
        if (i_start == 0) i_start = i
      end if

      i = i + 1
    end do

    ! If we've reached the end and we're still in a word, create a token from it
    ! and add it to the list
    if (i_start > 0) then
      j = j + 1
      tokens(j) = int(to_int(string_(i_start:len_trim(string_))))
    end if
    ntk = j
  end subroutine tokenize


!===============================================================================
! GENERATE_RPN implements the shunting-yard algorithm to generate a Reverse
! Polish notation (RPN) expression for the region specification of a cell given
! the infix notation.
!===============================================================================

  subroutine generate_rpn(celid,tokens,ntkn,output,nout)
    integer, intent(inout) :: ntkn, nout, celid
    integer, intent(inout) :: tokens(:)    ! infix notation
    integer, intent(inout) :: output(:)    ! RPN notation

    integer :: i
    integer :: token
    integer :: op, nstk
    integer :: stack(ntkn)
    stack(1:ntkn) = 0
    nstk = 0

    do i = 1, ntkn
      token = tokens(i)

      if (token < OP_UNION) then
        ! If token is not an operator, add it to output
        nout = nout + 1
        output(nout) = token

      elseif (token < OP_RIGHT_PAREN) then
        ! Regular operators union, intersection, complement
        do while (nstk > 0)
          op = stack(nstk)

          if (op < OP_RIGHT_PAREN .and. &
               ((token == OP_COMPLEMENT .and. token < op) .or. &
               (token /= OP_COMPLEMENT .and. token <= op))) then
            ! While there is an operator, op, on top of the stack, if the token
            ! is left-associative and its precedence is less than or equal to
            ! that of op or if the token is right-associative and its precedence
            ! is less than that of op, move op to the output queue and push the
            ! token on to the stack. Note that only complement is
            ! right-associative.
            nout = nout + 1
            output(nout) = op
            stack(nstk) = 0
            nstk = nstk - 1
          else
            exit
          end if
        end do
        nstk = nstk + 1
        stack(nstk) = token

      elseif (token == OP_LEFT_PAREN) then
        ! If the token is a left parenthesis, push it onto the stack
        nstk = nstk + 1
        stack(nstk) = token

      else
        ! If the token is a right parenthesis, move operators from the stack to
        ! the output queue until reaching the left parenthesis.
        do
          ! If we run out of operators without finding a left parenthesis, it
          ! means there are mismatched parentheses.
          if (nstk == 0) then
            call fatal('Mimatched parentheses in region specification &
                 & for cell "'// sset(to_str(celid)) // '"')
          end if

          op = stack(nstk)
          if (op == OP_LEFT_PAREN) exit
          nout = nout + 1
          output(nout) = op
          stack(nstk) = 0
          nstk = nstk - 1
        end do

        ! Pop the left parenthesis.
        stack(nstk) = 0
        nstk = nstk - 1
      end if
    end do

    ! While there are operators on the stack, move them to the output queue
    do while (nstk > 0)
      op = stack(nstk)

      ! If the operator is a parenthesis, it is mismatched
      if (op >= OP_RIGHT_PAREN) then
        call fatal('Mimatched parentheses in region specification &
             &for cell "'// sset(to_str(celid)) // '"')
      end if

      nout = nout + 1
      output(nout) = op
      stack(nstk) = 0
      nstk = nstk - 1

    end do

  end subroutine generate_rpn

end module

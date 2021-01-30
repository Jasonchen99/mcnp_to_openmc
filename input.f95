module input
  use constants
  use general
  use global
  ! use text linux foul

  ! p, px, py, pz are allowed for * + -

  ! input file should begin from column 1-5

  ! like cell for cells only with u fill needs check for # cell

  ! u -ve number not allowed *fill tr call rotation cones not allowed.


  implicit none

  integer :: t_lines, long_line, cell_start, cell_end, surf_start, surf_end, &
    data_start, data_end, iproc, kinp, s_trcl_count
  integer, allocatable    :: itoken(:)

  character(:),    allocatable :: iusr(:), iprc(:), iinp(:)
  character(2100), allocatable :: tinp(:)
  character(MAX_FILE_LEN) :: inpfile
  character(2100)         :: ctmp, ctmpx


contains

  subroutine i_routines

    ! i_process >> reads input file, concatenate multiple lines and store in
    ! iinp array. Decide length of different blocks and error check
    call i_process

    ! Reads surface line, check for input error.
    call i_surf_templines

    ! Reads cell lines , store various parameters as material, density, fill,
    ! u, lat, imp:n and process region specification create rpn and error check.
    call i_cell_lines


    call i_data_lines ! TODO: need to finalize


    !call i_mats_process ! not required for this github project

    ! count number of surfaces to be created (trcl), create final surfaces in
    ! array surf and build neighbour list.
    call i_surf_lines

    ! construct universes and lattices and other params
    call i_cell_process

    ! checks whether the universes, cells, surfaces and lattices are consistant
    ! wrt each other according to the dependency and create cell(:) array

    call i_cell_final! all fatal warnings comes here -- need to do


    !call i_cell_temp_to_index     ! finalize later just for plotter<<<<<<<<<<<<


    if(dev)print*,'input read over'

  end subroutine

  subroutine i_data_lines

    integer :: i, j, k, l,  nw
    character(MAX_WORD_LEN) :: wrd(MAX_WORDS)
    !character(MAX_LINE_LEN) ::  env_datapath

    ! check windows or linux or mac

    ! print*,' DATA LINES',ncel

    ! XSDIR

    ! get environment variables
!    ldir = .false.
!
!    call get_environment_variable('DATAPATH',env_datapath)
!
!    ! xsdir in current directory is used,
!    ! if no xsdir in current directory then look for datapath
!
!    ! Check in current execution directory
!
!    inquire(file='xsdir',exist=filexsdir) ! Capital or Small ??
!
!    if(filexsdir)then
!
!      if(dev)call msg('Using xsdir from current directory')
!      open(unit=xsunit, file='xsdir', action='read',iostat=io)
!      if(io /= 0) call fatal('xsdir file opening error - Check file')
!
!    else
!
!      if(len_trim(env_datapath) == 0)call fatal('No xsdir file in current worki&
!      &ng directory and no path in DATAPATH environment variable')
!
!      inquire(file=trim(adjustl(env_datapath))//'xsdir',exist=filexsdir)
!
!      if(filexsdir)then
!        call msg('xsdir file found in the path specified at DATAPATH environmen&
!        &t variable')
!        open(unit=xsunit, file=trim(adjustl(env_datapath))//'/xsdir', &
!        action='read',iostat=io)
!        if(io /= 0) call fatal('xsdir file opening error')
!      else
!        call fatal('xsdir file does not exists in current working directory &
!        and in DATAPATH environment variable path:'//new_line('') &
!        //trim(adjustl(env_datapath))//'/xsdir')
!      end if
!    end if
!
!    read(xsunit,'(A)')datapath
!
!!    datapath = lower(datapath)
!
!    if(index(datapath,'datapath') == 0)then ! Caps or lower ???
!      call warn('Datapath not found in 1st line of xsdir file.'//new_line('')//&
!      'Looking for cross section files in current directory')
!      datapath = ''
!    else
!      wrd(1:MAX_WORDS) = ''
!      call split(datapath,wrd,i)
!      if(i > 1)then
!        datapath=datapath(index(datapath,wrd(2)):len_trim(datapath))
!        !print*,'Datapath = ',datapath
!      else
!        datapath = ''
!      end if
!    end if
!
!    ! TODO: handle atomic weight ratio
!
!    i = 0
!    do
!      read(xsunit,*,iostat=io)dctmp
!      if(io /= 0)exit
!      if(index(dctmp,'directory') /= 0) i = 1
!      if(i /= 0)i = i + 1
!    end do
!
!    nxsl = i - 2
!
!    allocate(xslists(nxsl))
!
!    rewind(xsunit)
!    !if(index(datapath,'datapath') /= 0)read(xsunit,*)dctmp
!
!    i = 0
!
!    do
!      read(xsunit,'(a255)')dctmp
!
!      if(index(dctmp,'directory') /= 0)then
!        ldir = .true.
!        read(xsunit,'(a255)')dctmp
!        i = i + 1
!      end if
!
!      if(.not.ldir)cycle
!      wrd(1:MAX_WORDS) = ''
!    !  dctmp = lower(dctmp)
!      call split(dctmp,wrd,k)
!
!      ! Add fatal for all these errors
!      xslists(i)%name = sset(wrd(1))
!      xslists(i)%awr  = to_real(wrd(2))
!      xslists(i)%path = sset(wrd(3))
!      xslists(i)%location = to_pint(wrd(6))
!      xslists(i)%kT = to_real(wrd(10))
!      if(index(xslists(i)%name,'c')/=0)xslists(i)%type = ACE_NEUTRON
!      if(index(xslists(i)%name,'t')/=0)xslists(i)%type = ACE_THERMAL   ! names with c and t gives issue
!      if(xslists(i)%type == ACE_NEUTRON) xslists(i)%zaid = to_pint(wrd(1)(1:index(wrd(1),'.')-1))
!      i = i + 1
!      !print*,'i = ',i
!
!      if(i == nxsl)exit
!      !print*,trim(xslists(j)%name),xslists(j)%awr,trim(xslists(j)%path),xslists(j)%location,xslists(j)%kT,xslists(j)%type
!    end do
!
!! only one sab allowed --- limit
!
!
!    ! count mats

    j = 0
    do i = data_start, data_end
      if(iinp(i)(1:1) == 'm' .and. iinp(i)(1:2) /= 'mt' .and. &
         iinp(i)(1:4) /= 'mode') j = j + 1
    end do

    !  print*,'nmat = ',j

    nmat = j

    ! allocate mats

    ! mt card should appear only after corresponding material m card

    allocate(mats(nmat))

    mats(:)%n_sab = 0
    mats(:)%n_nuclides = 0
    mats(:)%density = 0.0_8
!    mats(:)%fissionable = .false.
    mats(:)%i = 0
    mats(:)%name = ''
!    mats(:)%n_grid = 0
    mats(:)%n_nuclides = 0
    mats(:)%n_sab = 0

    do i = 1, nmat
!      mats(i)%atom_density(:) = 0.0_8
!      mats(i)%e_grid(:) = 0
!      mats(i)%i_sab_nuclides(:) = 0
!      mats(i)%i_sab_tables(:) = 0
!      mats(i)%names(:) = ''
!      mats(i)%nuclide(:) = 0
!      mats(i)%nuclide_grid_index(:,:) = 0
!      mats(i)%p0(:) = .false.
    end do


    ! initialize=-----------------------------


    nmat = 0

    do i = data_start, data_end

      ctmp = sset(lower(iinp(i)))

      wrd(1:MAX_WORDS) = '' ; nw = 0

      call split(ctmp,wrd,nw)

      if(wrd(1)(1:1) == 'm' .and. wrd(1)(1:2) /= 'mt' .and. &
      wrd(1)(1:4) /= 'mode')then

        if(nw == 1)call fatal('No nuclides specified for material "'// &
        sset(wrd(1))//'"')

        nmat = nmat + 1
        if(mod(nw,2) /= 1) call fatal('Nuclide name and fractions are not in pa&
        &irs for material "'//sset(wrd(1))//'"')

        if(.not.is_pint(wrd(1)(2:len_trim(wrd(1)))))call fatal('Can not conver&
        &t "'//wrd(1)(2:len_trim(wrd(1)))//'" to positive integer specified for&
        & material id "'//sset(wrd(1))//'"')

        mats(nmat)%i = int(to_pint(wrd(1)(2:len_trim(wrd(1)))))

        mats(nmat)%name = sset(wrd(1))
        mats(nmat)%n_nuclides = (nw - 1)/2

        allocate(mats(nmat)%atom_density(mats(nmat)%n_nuclides))
        allocate(mats(nmat)%names(mats(nmat)%n_nuclides))
        allocate(mats(nmat)%nuclide(mats(nmat)%n_nuclides))

        ! initialize

        mats(nmat)%atom_density = 0.0_8
        mats(nmat)%names = ''
        mats(nmat)%nuclide = 0


        do j = 1, mats(nmat)%n_nuclides
          mats(nmat)%names(j) = sset(wrd(2*j)) ! add fatals for names ====

          if(.not.is_real(sset(wrd((2*j)+1))))call fatal('Can not convert nucli&
          &de fraction "'//sset(wrd((2*j)+1))//'" to real specified in material&
          & "'//sset(wrd(1))//'"')
          mats(nmat)%atom_density(j) = to_real(sset(wrd((2*j)+1)))
        end do

      elseif(wrd(1)(1:2) == 'mt')then ! add fatal if mt123 m123 doesnt match

        allocate(mats(nmat)%sab_names(1))
        allocate(mats(nmat)%i_sab_tables(1))
        allocate(mats(nmat)%i_sab_nuclides(1))
!
        mats(nmat)%n_sab = 1
!
        mats(nmat)%sab_names(1) = ''
!
        mats(nmat)%sab_names(1) = sset(wrd(2))

      elseif(wrd(1) == 'ksrc')then

        if(nw == 1)call fatal('Source points are not specified at "ksrc" card')

        ! auto generate source points

        if(mod(nw-1,3) /= 0) call fatal('Source points specified on ksrc card a&
        &re not in triplet(xyz) form')

        nksrc = (nw-1)/3

        allocate(ksrc(nksrc,3))

        ! initialize

        do j = 1, nksrc
          if(.not.is_real(sset(wrd(j*3-1))))call fatal('Can not convert source &
          &point "'//sset(wrd(j*3-1))//'" to real number on ksrc card')
          if(.not.is_real(sset(wrd(j*3))))call fatal('Can not convert source &
          &point "'//sset(wrd(j*3))//'" to real number on ksrc card')
          if(.not.is_real(sset(wrd(j*3+1))))call fatal('Can not convert source &
          &point "'//sset(wrd(j*3+1))//'" to real number on ksrc card')
          ksrc(j,:) = [to_real(sset(wrd(j*3-1))),to_real(sset(wrd(j*3))), &
          to_real(sset(wrd(j*3+1)))]
        end do

      !  print'(3(2x,f8.5))',ksrc

      elseif(wrd(1) == 'kcode')then

        if(nw == 1)call fatal('No parameters on "kcode" card') ! take default

        if(.not.is_pint(wrd(2)))call fatal('Can not convert "'//sset(wrd(2))//'&
        &" to positive integer specified on "kcode" card')
        if(.not.is_pint(wrd(4)))call fatal('Can not convert "'//sset(wrd(4))//'&
        &" to positive integer specified on "kcode" card')
        if(.not.is_pint(wrd(5)))call fatal('Can not convert "'//sset(wrd(5))//'&
        &" to positive integer specified on "kcode" card')

        n_particles = to_pint(wrd(2))
        n_inactive  = to_pint(wrd(4))
        n_batches   = to_pint(wrd(5))

      elseif(wrd(1) == 'imp:n')then

        if(nw ==1)call fatal('Cell importances are not specified on imp:n card')

        k = 0

        do j = 1, nw - 1
          if(index(wrd(j+1),'r') /= 0)then

            if(.not.is_pint(wrd(j+1)(1:len_trim(wrd(j+1))-1)))call fatal('Can n&
            &ot convert "'//wrd(j+1)(1:len_trim(wrd(j+1))-1)//'" to positive in&
            &teger specified at imp:n card')

            do l = 1, to_pint(wrd(j+1)(1:len_trim(wrd(j+1))-1))
              k = k + 1
       !       dimp(k) = to_int(wrd(j))
            end do
          else
            k = k + 1
       !       dimp(k) = to_int(wrd(j+1))
          end if
        end do

        if(k /= ncel)call fatal('Cell importances on imp:n card doesn"t match t&
        &otal number of cells specified')
        allocate(dimp(ncel))
        k = 0
        do j = 1, nw - 1
          if(index(wrd(j+1),'r') /= 0)then
            do l = 1, to_pint(wrd(j+1)(1:len_trim(wrd(j+1))-1))
              k = k + 1
              if(.not.is_int(wrd(j)))call fatal('Can not convert "'// &
              sset(wrd(j))//'" to integer specified on imp:n card')
              dimp(k) = to_int(wrd(j))
            end do
          else
            k = k + 1
            if(.not.is_int(wrd(j+1)))call fatal('Can not convert "'// &
            sset(wrd(j+1))//'" to integer specified on imp:n card')
            dimp(k) = to_int(wrd(j+1))
          end if
        end do
      else
        call warn('Unrecognized data card "'//sset(ctmp)//'" - Ignored.')
      end if

    end do


    if(dev)print*,'data line process over'
  end subroutine


  subroutine i_cell_final
   ! stop 63
  end subroutine

  subroutine i_cell_temp_to_index
    integer :: i,j,k,l,m,n,o

    ! Change surface id to index in cells (Only changed in RPNs)

    BASE_UNIV_INDEX = -huge(0)

    if(any(tcell(:)%lat /= 0))then
      call warn('  lat is not ready   ')
      return
    end if

    do i = 1, ncel
      do j = 1, tcell(i)%nrpn
        k = tcell(i)%rpn(j)
        if(k >= OP_UNION)cycle
        k = sur_ixf(iabs(k))
        tcell(i)%rpn(j) = sign(k,tcell(i)%rpn(j))
      end do

      if(tcell(i)%lat /= 0)then
        tcell(i)%lat =lat_ix(tcell(i)%uni)  ! tcell%lat has been reused to store latt index in cell
      end if

      tcell(i)%uni = uni_ix(tcell(i)%uni) ! change with uni ix  --- %%%%%%

      j = tcell(i)%fil

      if(any(univ(:)%i == j) .and. j /= 0)then
        tcell(i)%fil = uni_ix(j)
      elseif(j == 0 .and. tcell(i)%lat /= 0)then ! pattern
        k = tcell(i)%lat              ! uni is index by this time
        do l = latt(k)%izl,latt(k)%izu
          do m = latt(k)%iyl,latt(k)%iyu
            do n = latt(k)%ixl,latt(k)%ixu
              o = latt(k)%fils(n,m,l)
              latt(k)%fils(n,m,l) = uni_ix(o) ! change with uni ix
            end do
          end do
        end do
      elseif(j == 0)then
        cycle
      else
        call fatal(' sub i_cell_temp_to_index :: latt --- DEV')
      end if
    end do

    do i = 1, nuni
      if(univ(i)%i == 0)BASE_UNIV_INDEX = i
      do j = 1, univ(i)%ncel
        univ(i)%cell(j) = cel_ix(univ(i)%cell(j))
      end do
!      print*,i,'->',univ(i)%i ! 1 0 3 for 5.5
!      print*,BASE_UNIV_INDEX   ! 2 for 5.5
    end do

    do i = 1, ncel
      if(tcell(i)%mat /= MAT_VOID)tcell(i)%mat = mat_ix(tcell(i)%mat)
    end do

    do i = 1, nmat
      do j = 1, mats(i)%n_nuclides
        mats(i)%nuclide(j) = nucstore_ix(mats(i)%names(j))
      end do
      if(mats(i)%n_sab /= 0)then
        mats(i)%i_sab_tables(1) = sabstore_ix(mats(i)%sab_names(1))
      end if
    end do

    do i = 1, nsrf
      do j = 1, surf(i)%nposi
        surf(i)%npos(j) = cel_ix(surf(i)%npos(j))
      end do
      do j = 1, surf(i)%nnega
        surf(i)%nneg(j) = cel_ix(surf(i)%nneg(j))
      end do
    end do


if(dev)print*,'i_cell_temp_to_index over'
  end subroutine

  subroutine i_cell_process

    integer :: i, j, k, l, m, nwrd, ix, iy, iz, i1, i2, i3, j1, j2,  rnum,tmp
    character(MAX_WORD_LEN) :: wrd(MAX_WORDS)
    logical :: flip

    if(fatalcount /= 0)call fatal('fatal errors in input &
    &i_cell_process "'//sset(to_str(fatalcount)),.true.)

    ! copy importance from data

    if(all(tcell(:)%imp == 0).and.(.not.allocated(dimp)))call fatal('Cell importance &
    &data is not present in cell cards and in imp:n card at data blocks')


    if(allocated(dimp))then
      do i = 1, ncel
        tcell(cel_ix(cel_store%i(i)))%imp = dimp(i)
      end do
    end if

    ! create universes

    nuni = 0 ; uni_store%t = 0 ; allocate(uni_store%i(ncel)) ! temp
    uni_store%i = -huge(0) ! hex univ -1 clash

    do i = 1, ncel

      if(any(tcell(i)%uni == uni_store%i))then
        cycle
      else
        !if(tcell(i)%lat /= 0)cycle     ! skip lat for uni ---%%% changed
        uni_store%t = uni_store%t + 1
        uni_store%i(uni_store%t) = tcell(i)%uni
      end if

    end do

    if(.not.any(uni_store%i == 0))call fatal('No cells in base universe -> One &
    &cell without "u" should be present <-')

    allocate(univ(uni_store%t)) ; univ(:)%i = -huge(0) ;  univ(:)%ncel = 0

    nuni = uni_store%t
    univ(1:nuni)%i = uni_store%i(1:nuni) ! copy IDs

    ! add cells -- skipping lattice cell

    do i = 1, ncel       ! Count
      !if(tcell(i)%lat /= 0)cycle  ! skip lat for uni ---%%% changed
      j = uni_ix(tcell(i)%uni)
      univ(j)%ncel = univ(j)%ncel + 1
    end do

    do i = 1, nuni       ! Allocate
      allocate(univ(i)%cell(univ(i)%ncel))
      univ(i)%cell(:) = 0
    end do

    univ(:)%ncel = 0
    do i = 1, ncel       ! Add
      !if(tcell(i)%lat /= 0)cycle ! skip lat for uni ---%%% changed
      j = uni_ix(tcell(i)%uni)
      univ(j)%ncel = univ(j)%ncel + 1
      univ(j)%cell(univ(j)%ncel) = tcell(i)%i
    end do

!    simple cell or complex

    do i = 1, ncel
      if(any(tcell(i)%rpn == OP_UNION) .or. any(tcell(i)%rpn == OP_COMPLEMENT)) &
      tcell(i)%simple = .false.
      if(tcell(i)%imp == 0)then
        k = 18
        if(.not.any(tcell(i)%rgn == OP_COMPLEMENT))then
          do j = 1, tcell(i)%nrpn
            if(tcell(i)%rpn(j) >= OP_UNION)cycle
            surf(sur_ixf(iabs(tcell(i)%rpn(j))))%bc = BC_VACUUM
          end do
        else
          flip = .false.
          do j = 1, tcell(i)%nrgn

            ! something wrong may be
            if(tcell(i)%rgn(j) == OP_COMPLEMENT)flip = .true.
            if(tcell(i)%rgn(j) >= OP_UNION)cycle
            if(flip)cycle ! order needs to change
            if(flip .and. tcell(i)%rgn(j) == OP_RIGHT_PAREN)flip = .false.
            if(.not.flip)surf(sur_ixf(iabs(tcell(i)%rgn(j))))%bc = BC_VACUUM
          end do
        end if
      end if
    end do



    if(k /= 18)call fatal('One cell with importance zero must be present in base universe')
    k = 0

    ! create lattices    ! fill string handling

    nlat = 0

    do i = 1, ncel       ! count
      if(tcell(i)%lat /= 0)nlat = nlat + 1
    end do

    if(nlat /= 0)call warn('For now this project does not process lattice cells...')

    if(nlat /= 0)then

      return !--------- no lattice

      allocate(lat_store%i(nlat)) ; lat_store%i(:) = -huge(0) ; lat_store%t = 0

      do i = 1, ncel
        if(tcell(i)%lat == 0)cycle

        ! CHECK: lattice id u=* shouldn't present in other cells u=*


        ! TODO: enable this in different way

!        if(any(uni_store%i == tcell(i)%uni)) call fatal('Universe "'//sset&
!        (to_str(tcell(i)%uni))//'" is already used, specified for cell "'//&
!        sset(to_str(tcell(i)%i))//'" -> lattice cell must belongs to different &
!        universe <-')

        lat_store%t = lat_store%t + 1
        if(any(lat_store%i == tcell(i)%uni))call fatal('Two lattice cells can &
        &not belongs to the same universe "'//sset(to_str(tcell(i)%uni))//'"')
        lat_store%i(lat_store%t) = tcell(i)%uni

      end do

    end if

    if(nlat /= 0)then

      allocate(latt(nlat)) ! allocate

      ! Todo: lat mat  and density addition <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      latt(:)%i = 0          ! init
      latt(:)%ixl = -huge(0) ; latt(:)%ixu = -huge(0)
      latt(:)%iyl = -huge(0) ; latt(:)%iyu = -huge(0)
      latt(:)%izl = -huge(0) ; latt(:)%izu = -huge(0)
      latt(:)%pattern = .false.
      latt(:)%fil = 0
      latt(:)%typ = 0

      latt(:)%xpls = 0.0_8 ; latt(:)%xmin = 0.0_8
      latt(:)%ypls = 0.0_8 ; latt(:)%ymin = 0.0_8
      latt(:)%zpls = 0.0_8 ; latt(:)%zmin = 0.0_8
      latt(:)%width(1) = 0.0_8;latt(:)%width(2) = 0.0_8;latt(:)%width(3) = 0.0_8

      nlat = 0

      ALL_CELL: do i = 1, ncel

        if(tcell(i)%lat == 0)cycle
        nlat = nlat + 1
        latt(nlat)%i = tcell(i)%uni ! take lattice u=* as lattice id

        if(tcell(i)%fil == 0)latt(nlat)%pattern = .true. ! pattern
        latt(nlat)%typ = tcell(i)%lat ! rect or hex

        LAT_TYP: if(latt(nlat)%typ == 1)then   !square

!call color1 ('sqr lat -- '//trim( iinp(1)),5,7)!---------------------------------

          LAT_PATRN: if(.not.latt(nlat)%pattern)then

            latt(nlat)%fil = tcell(i)%fil ! single number

          else LAT_PATRN

            ! Pattern

            wrd(1:MAX_WORDS) = ''
            nwrd = 0
            call split(tcell(i)%latfill,wrd,nwrd)
            do j = 1, 3
              if(index(sset(wrd(j)),':')==0) call fatal('Error in fill pattern &
              &specified for cell "'//sset(to_str(tcell(i)%i))//'"')
            end do

            do j = 1, 3

              if(.not.is_int(wrd(j)(1:index(sset(wrd(j)),':')-1)).or. .not. &
              is_int(wrd(j)(index(sset(wrd(j)),':')+1:len_trim(wrd(j)))))then

                if(.not.is_int(wrd(j)(1:index(sset(wrd(j)),':')-1)))then
                  call fatal('Can not convert fill index "'// sset(wrd(j) &
                  (1:index(sset(wrd(j)),':')-1))//'" to integer specif&
                  &ied for cell "'//sset(to_str(tcell(i)%i))//'"')
                end if

                if(.not.is_int(wrd(j) &
                (index(sset(wrd(j)),':')+1:len_trim(wrd(j)))))then
                  call fatal('Can not convert fill index "'// sset(wrd(j) &
                  (index(sset(wrd(j)),':')+1:len_trim(wrd(j))))//'" to integer&
                  & specified for cell "'//sset(to_str(tcell(i)%i))//'"')
                end if

              else

                if(j==1)then
                  latt(nlat)%ixl = to_int(wrd(j)(1:index(sset(wrd(j)),':')-1))
                  latt(nlat)%ixu = to_int &
                    (wrd(j)(index(sset(wrd(j)),':')+1:len_trim(wrd(j))))
                elseif(j==2)then
                  latt(nlat)%iyl = to_int(wrd(j)(1:index(sset(wrd(j)),':')-1))
                  latt(nlat)%iyu = to_int &
                    (wrd(j)(index(sset(wrd(j)),':')+1:len_trim(wrd(j))))
                elseif(j==3)then
                  latt(nlat)%izl = to_int(wrd(j)(1:index(sset(wrd(j)),':')-1))
                  latt(nlat)%izu = to_int &
                    (wrd(j)(index(sset(wrd(j)),':')+1:len_trim(wrd(j))))
                end if

              end if

            end do

            ! universe count and store

            ix = latt(nlat)%ixu - latt(nlat)%ixl + 1
            iy = latt(nlat)%iyu - latt(nlat)%iyl + 1
            iz = latt(nlat)%izu - latt(nlat)%izl + 1

            allocate(latt(nlat)%fils(latt(nlat)%ixl:latt(nlat)%ixu,&
             latt(nlat)%iyl:latt(nlat)%iyu,latt(nlat)%izl:latt(nlat)%izu))

            m = 4  ; i1 = 0 ; i2 = 0

            do j = latt(nlat)%izl, latt(nlat)%izu
              do k = latt(nlat)%iyl, latt(nlat)%iyu
                do l = latt(nlat)%ixl, latt(nlat)%ixu
                  i2 = i2 + 1

                  if(index(sset(wrd(m)),'r') /= 0)then
                    if(.not.is_pint(sset(wrd(m)(1:index(sset(wrd(m)),'r')-1))))&
                    call fatal('Can not convert "'//sset(wrd(m) &
                    (1:index(sset(wrd(m)),'r')-1))//'" ('//sset(wrd(m) &
                    (1:index(sset(wrd(m)),'r')-1))//'r) to positive integer &
                    &specified for cell "'//sset(to_str(tcell(i)%i))//'"')

                    rnum = to_pint(sset(wrd(m)(1:index(sset(wrd(m)),'r')-1)))

                    i1 = i1 + 1

                    latt(nlat)%fils(l,k,j) = to_pint(sset(wrd(m-1)))
                    if(i1 == rnum)then
                      m = m + 1 ; i1 = 0
                    end if
                  elseif(is_pint(sset(wrd(m))))then
                    latt(nlat)%fils(l,k,j) = to_pint(sset(wrd(m)))
                     m = m + 1
                  else
                    if(len_trim(wrd(m))==0)then
                      call fatal('Found less number of universe than specified &
                      &for cell "'//sset(to_str(tcell(i)%i))//'"')
                    else
                      call fatal('Can not convert "'//sset(wrd(m))//'" to posit&
                      &ive integer specified for cell "'// &
                      sset(to_str(tcell(i)%i))//'"')
                    end if
                  end if
                end do
              end do
            end do

            if(i1 /= 0) call fatal('Found more number of universe than specifi &
            &ed for cell "'//sset(to_str(tcell(i)%i))//'"')
            if(len_trim(wrd(m+1))/=0)call fatal('Found less number of universe &
            &than specified for cell "'//sset(to_str(tcell(i)%i))//'"')

          end if LAT_PATRN

          ! width

          if(any(tcell(i)%rgn(:) == OP_UNION))&
            call fatal('Union operator not allowed for lattice cell "'// &
            sset(to_str(tcell(i)%i))//'"  -> use only intersections <-')
          if(any(tcell(i)%rgn(:) == OP_COMPLEMENT))&
            call fatal('Complement operator not allowed for lattice cell "'// &
            sset(to_str(tcell(i)%i))//'" -> use only intersections <-')
          if(any(tcell(i)%rgn(:) == OP_LEFT_PAREN))&
            call fatal('Parentheses not allowed for lattice cell "'// &
            sset(to_str(tcell(i)%i))//'" -> use only intersections <-')
          if(any(tcell(i)%rgn(:) == OP_RIGHT_PAREN))&
            call fatal('Parentheses not allowed for lattice cell "'// &
            sset(to_str(tcell(i)%i))//'" -> use only intersections <-')

          ix = 0 ; iy = 0 ; iz = 0
          k = 0

          !todo: check number of surfaces 4 or 6  -- can be done with other para

          do j = 1, tcell(i)%nrgn
            if(tcell(i)%rgn(j)>=OP_UNION)cycle
            k = k + 1
            if(k==1)i1 =iabs(tcell(i)%rgn(j))
            if(k==2)then
              k=0
              i2 =iabs(tcell(i)%rgn(j))
              i1 = sur_ixf(i1) ; i2 = sur_ixf(i2)

              if(.not.any(surf(i1)%ityp == [P_PX,P_PY,P_PZ]))call fatal('Surfac&
              &e "'//sset(to_str(surf(i1)%i))//'" specified for lattice cell "'&
              //sset(to_str(tcell(i)%i))//'" must be of the type px, py or pz')

              if(.not.any(surf(i2)%ityp == [P_PX,P_PY,P_PZ]))call fatal('Surfac&
              &e "'//sset(to_str(surf(i1)%i))//'" specified for lattice cell "'&
              //sset(to_str(tcell(i)%i))//'" must be of the type px, py or pz')

              if(surf(i1)%ityp /= surf(i2)%ityp) call fatal('Surfaces "'//     &
              sset(to_str(surf(i1)%i))//'" and "'//sset(to_str(surf(i2)%i))//'"&
              &specified for lattice cell "'//sset(to_str(tcell(i)%i))//'" must &
              &be of same type' )

              if(surf(i1)%ityp == P_PX)then
                if(surf(i1)%coef(1) == surf(i2)%coef(1))call fatal('Surfaces "'&
                //sset(to_str(surf(i1)%i))//'" and "'//sset(to_str(surf(i2)%i))&
                //'" specified for lattice cell "'//sset(to_str(tcell(i)%i))// &
                '" must not have same co-efficient' )
                latt(nlat)%xpls = surf(i1)%coef(1)
                latt(nlat)%xmin = surf(i2)%coef(1)
                if(surf(i1)%coef(1) > surf(i2)%coef(1))then
                  latt(nlat)%width(1) = surf(i1)%coef(1) - surf(i2)%coef(1)
                else
                  latt(nlat)%width(1) = surf(i2)%coef(1) - surf(i1)%coef(1)
                end if
              end if

              if(surf(i1)%ityp == P_PY)then
                if(surf(i1)%coef(1) == surf(i2)%coef(1))call fatal('Surfaces "'&
                //sset(to_str(surf(i1)%i))//'" and "'//sset(to_str(surf(i2)%i))&
                //'" specified for lattice cell "'//sset(to_str(tcell(i)%i))// &
                '" must not have same co-efficient')
                latt(nlat)%ypls = surf(i1)%coef(1)
                latt(nlat)%ymin = surf(i2)%coef(1)
                if(surf(i1)%coef(1) > surf(i2)%coef(1))then
                  latt(nlat)%width(2) = surf(i1)%coef(1) - surf(i2)%coef(1)
                else
                  latt(nlat)%width(2) = surf(i2)%coef(1) - surf(i1)%coef(1)
                end if
              end if

              if(surf(i1)%ityp == P_PZ)then
                if(surf(i1)%coef(1) == surf(i2)%coef(1))call fatal('Surfaces "'&
                //sset(to_str(surf(i1)%i))//'" and "'//sset(to_str(surf(i2)%i))&
                //'" specified for lattice cell "'//sset(to_str(tcell(i)%i))// &
                '" must not have same co-efficient')
                latt(nlat)%zpls = surf(i1)%coef(1)
                latt(nlat)%zmin = surf(i2)%coef(1)
                if(surf(i1)%coef(1) > surf(i2)%coef(1))then
                  latt(nlat)%width(3) = surf(i1)%coef(1) - surf(i2)%coef(1)
                else
                  latt(nlat)%width(3) = surf(i2)%coef(1) - surf(i1)%coef(1)
                end if

              end if

            end if

          end do

        else LAT_TYP

          ! Hex

!          call color1 ('HEX lat -- '//trim( iinp(1)),5,8)

          return  ! TODO: hex lat not done

        end if LAT_TYP

      end do ALL_CELL

    end if

    ! cell fill type decide

    ! CHECK : all filling elements in fill pattern of lattice cell must be
    !         either universe or lattice  ( mix not allowed )
    !         add fatal for this ^-^

    do i = 1, ncel

      tmp = tcell(i)%fil

      if(tmp == 0 .and. tcell(i)%lat == 0)then
        tcell(i)%typ = CELL_NORMAL ; cycle
      end if

      if(tcell(i)%fil /= 0)then
        do j = 1, ncel
          if(tcell(j)%uni == tmp .and. tcell(j)%lat == 0)then
            tcell(i)%typ = CELL_FILL ; exit
          elseif(tcell(j)%uni == tmp .and. tcell(j)%lat /= 0)then
            tcell(i)%typ = CELL_LATTICE ; exit
          end if
        end do
        if(tcell(i)%typ == 0) call fatal(' error at cell fill decide --DEV '//sset(to_str(tcell(i)%i)))
!         if(any(univ(:)%i==tmp))tcell(i)%typ = FILL_UNIVERSE !-----%%%%%
!         if(any(latt(:)%i==tmp))tcell(i)%typ = FILL_LATTICE  !-----%%%%%
      else
        tmp = lat_ix(tcell(i)%uni) ; j2 = 0
        do i1 = latt(tmp)%izl,latt(tmp)%izu
          do i2 = latt(tmp)%iyl,latt(tmp)%iyu
            do i3 = latt(tmp)%ixl,latt(tmp)%ixu
              j1 = latt(tmp)%fils(latt(tmp)%ixl,latt(tmp)%iyl,latt(tmp)%izl)
              if(j1 /= tcell(i)%fil)then
                j2 = j1
              end if
            end do
          end do
        end do
        do j = 1, ncel
          if(tcell(j)%uni == j2 .and. tcell(j)%lat == 0)then
            tcell(i)%typ = CELL_FILL ; exit
          elseif(tcell(j)%uni == j2 .and. tcell(j)%lat /= 0)then
            tcell(i)%typ = CELL_LATTICE ; exit
          end if

        end do
        if(tcell(i)%typ == 0) call fatal(' error at cell fill decide --DEV- 1')

!        if(any(univ(:)%i==j2))tcell(i)%typ = FILL_UNIVERSE  !-----%%%%%
!        if(any(latt(:)%i==j2))tcell(i)%typ = FILL_LATTICE  !-----%%%%%

      end if

!      if(any(univ(:)%i==tmp))tcell(i)%typ = FILL_UNIVERSE  !-----%%%%%
!      if(any(latt(:)%i==tmp))tcell(i)%typ = FILL_LATTICE lat has been checked

    end do
if(dev)print*,'cell_process over'
  end subroutine

  subroutine i_surf_lines
    integer :: i, j, k, l, m

    l = 0
    do i = 1, t_cell
      if(tcell(i)%ltr)then
        do j = 1, tcell(i)%nrgn
          if(iabs(tcell(i)%rgn(j))<OP_UNION)s_trcl_count = s_trcl_count + 1
        end do
      end if
    end do

    ! TODO: remove duplicate surfaces and replace index in removed surfaces

    allocate(trsrf(s_trcl_count))

    trsrf(:)%bc = 0
    trsrf(:)%ctyp = ''
    trsrf(:)%i = 0
    trsrf(:)%ityp = 0
    trsrf(:)%ncf = 0
    trsrf(:)%nnega = 0
    trsrf(:)%nposi = 0
    trsrf(:)%peri = 0


    FOR_TRCL_CELL: do i = 1, t_cell
      IF_TRCL: if(tcell(i)%ltr)then
        do j = 1, tcell(i)%nrgn
          if(iabs(tcell(i)%rgn(j))>=OP_UNION)cycle
          l = l + 1
          k = tcell(i)%rgn(j)
          trsrf(l)%i = iabs(k)
          k = iabs(k) - (1000*tcell(i)%i)
          k = sur_ix(k)

          ! TODO : if usual cell trcl allowed, then need to handle surface here.

          if(tsurf(k)%bc /= BC_TRANSMIT)call fatal('Surface "'// &
            sset(to_str(tsurf(k)%i))//'" of cell "'//sset(to_str(tcell(i)%i))//'"&
            & need to have transmittive boudary as it is used in like cell constr&
            &uction')

          trsrf(l)%peri = tsurf(k)%peri
          trsrf(l)%bc   = tsurf(k)%bc

          if(tsurf(k)%ityp == P_P)then  ! todo: merge p_p with p_x p_y p_z

            trsrf(l)%ctyp = tsurf(k)%ctyp
            trsrf(l)%ityp = tsurf(k)%ityp
            trsrf(l)%ncf  = tsurf(k)%ncf
            allocate(trsrf(l)%coef(tsurf(k)%ncf))
            trsrf(l)%coef = 0.0_8
            do m = 1, tsurf(k)%ncf - 1
              trsrf(l)%coef(m) = tsurf(k)%coef(m)
            end do
            trsrf(l)%coef(4) = -((tsurf(k)%coef(1))*(tcell(i)%trcl(1)) + &
              (tsurf(k)%coef(2))*(tcell(i)%trcl(2)) + &
              (tsurf(k)%coef(3))*(tcell(i)%trcl(3)))

          elseif( any(tsurf(k)%ityp == [P_PX, P_PY, P_PZ]))then

            trsrf(l)%ctyp = tsurf(k)%ctyp
            trsrf(l)%ityp = tsurf(k)%ityp
            trsrf(l)%ncf  = tsurf(k)%ncf
            allocate(trsrf(l)%coef(tsurf(k)%ncf))
            trsrf(l)%coef = 0.0_8
            if(tsurf(k)%ityp == P_PX) &
              trsrf(l)%coef(1) = tsurf(k)%coef(1) + tcell(i)%trcl(1)
            if(tsurf(k)%ityp == P_PY) &
              trsrf(l)%coef(1) = tsurf(k)%coef(1) + tcell(i)%trcl(2)
            if(tsurf(k)%ityp == P_PZ) &
              trsrf(l)%coef(1) = tsurf(k)%coef(1) + tcell(i)%trcl(3)

          elseif(any(tsurf(k)%ityp == [S_O,S_S,S_SX,S_SY,S_SZ]))then

            trsrf(l)%ctyp = 's'
            trsrf(l)%ityp = S_S
            trsrf(l)%ncf  = 4

            allocate(trsrf(l)%coef(trsrf(l)%ncf))
            trsrf(l)%coef = 0.0_8

            if(tsurf(k)%ityp == S_O)then
              trsrf(l)%coef(1:3) = tcell(i)%trcl(1:3)
              trsrf(l)%coef(4)   = tsurf(k)%coef(1)
            end if
            if(tsurf(k)%ityp == S_S)then
              trsrf(l)%coef(1:3) = tsurf(k)%coef(1:3) + tcell(i)%trcl(1:3)
              trsrf(l)%coef(4)   = tsurf(k)%coef(4)
            end if
            if(tsurf(k)%ityp == S_SX)then
              trsrf(l)%coef(1) = tsurf(k)%coef(1) + tcell(i)%trcl(1)
              trsrf(l)%coef(2:3) = tcell(i)%trcl(2:3)
              trsrf(l)%coef(4) = tsurf(k)%coef(2)
            end if
            if(tsurf(k)%ityp == S_SY)then
              trsrf(l)%coef(1) = tcell(i)%trcl(1)
              trsrf(l)%coef(2) = tsurf(k)%coef(1) + tcell(i)%trcl(2)
              trsrf(l)%coef(3) = tcell(i)%trcl(3)
              trsrf(l)%coef(4) = tsurf(k)%coef(2)
            end if
            if(tsurf(k)%ityp == S_SZ)then
              trsrf(l)%coef(1:2) = tcell(i)%trcl(1:2)
              trsrf(l)%coef(3) = tsurf(k)%coef(1) + tcell(i)%trcl(3)
              trsrf(l)%coef(4) = tsurf(k)%coef(2)
            end if

          elseif(tsurf(k)%ityp == C_CX .or. tsurf(k)%ityp == C_CXX)then

            trsrf(l)%ityp = C_CXX
            trsrf(l)%ctyp = 'c/x'
            trsrf(l)%ncf = 3
            allocate(trsrf(l)%coef(trsrf(l)%ncf))
            trsrf(l)%coef = 0.0_8

            if(tsurf(k)%ityp == C_CX)then
              trsrf(l)%coef(1) = tcell(i)%trcl(2)
              trsrf(l)%coef(2) = tcell(i)%trcl(3)
              trsrf(l)%coef(3) = tsurf(k)%coef(1)
            else
              trsrf(l)%coef(1) = tsurf(k)%coef(1) + tcell(i)%trcl(2)
              trsrf(l)%coef(2) = tsurf(k)%coef(2) + tcell(i)%trcl(3)
              trsrf(l)%coef(3) = tsurf(k)%coef(3)
            end if

          elseif(tsurf(k)%ityp == C_CY .or. tsurf(k)%ityp == C_CYY)then

            trsrf(l)%ityp = C_CYY
            trsrf(l)%ctyp = 'c/y'
            trsrf(l)%ncf = 3
            allocate(trsrf(l)%coef(trsrf(l)%ncf))
            trsrf(l)%coef = 0.0_8

            if(tsurf(k)%ityp == C_CY)then
              trsrf(l)%coef(1) = tcell(i)%trcl(1)
              trsrf(l)%coef(2) = tcell(i)%trcl(3)
              trsrf(l)%coef(3) = tsurf(k)%coef(1)
            else
              trsrf(l)%coef(1) = tsurf(k)%coef(1) + tcell(i)%trcl(1)
              trsrf(l)%coef(2) = tsurf(k)%coef(2) + tcell(i)%trcl(3)
              trsrf(l)%coef(3) = tsurf(k)%coef(3)
            end if

          elseif(tsurf(k)%ityp == C_CZ .or. tsurf(k)%ityp == C_CZZ)then

            trsrf(l)%ityp = C_CZZ
            trsrf(l)%ctyp = 'c/z'
            trsrf(l)%ncf = 3
            allocate(trsrf(l)%coef(trsrf(l)%ncf))
            trsrf(l)%coef = 0.0_8

            if(tsurf(k)%ityp == C_CZ)then
              trsrf(l)%coef(1) = tcell(i)%trcl(1)
              trsrf(l)%coef(2) = tcell(i)%trcl(2)
              trsrf(l)%coef(3) = tsurf(k)%coef(1)
            else
              trsrf(l)%coef(1) = tsurf(k)%coef(1) + tcell(i)%trcl(1)
              trsrf(l)%coef(2) = tsurf(k)%coef(2) + tcell(i)%trcl(2)
              trsrf(l)%coef(3) = tsurf(k)%coef(3)
            end if

          end if

        end do
      end if IF_TRCL
    end do FOR_TRCL_CELL

    ! create final surface list in surf (tsurf, trsrf)

    nsrf = tmp_surf + s_trcl_count

    allocate(surf(nsrf))

    ! Initialize

    surf(:)%i = 0
    surf(:)%bc = 0
    surf(:)%ctyp = ''
    surf(:)%ityp = 0
    surf(:)%nnega = 0
    surf(:)%nposi = 0
    surf(:)%peri = 0
    surf(:)%ncf = 0

    do i = 1, nsrf
      if(i<=tmp_surf)then
        surf(i)%i    =  tsurf(i)%i
        surf(i)%ctyp =  tsurf(i)%ctyp
        surf(i)%ityp =  tsurf(i)%ityp
        surf(i)%ncf  =  tsurf(i)%ncf
        surf(i)%peri =  tsurf(i)%peri
        allocate(surf(i)%coef(tsurf(i)%ncf))
        surf(i)%coef = 0.0_8
        surf(i)%coef = tsurf(i)%coef
      else
        j = i - tmp_surf
        surf(i)%i    =  trsrf(j)%i
        surf(i)%ctyp =  trsrf(j)%ctyp
        surf(i)%ityp =  trsrf(j)%ityp
        surf(i)%ncf  =  trsrf(j)%ncf
        surf(i)%peri =  trsrf(j)%peri
        allocate(surf(i)%coef(trsrf(j)%ncf))
        surf(i)%coef = 0.0_8
        surf(i)%coef = trsrf(j)%coef
      end if
    end do

    do i = 1, t_cell
      do j = 1, tcell(i)%nrgn
        if(iabs(tcell(i)%rgn(j))<OP_UNION)then
          if(.not. any(iabs(tcell(i)%rgn(j))==surf(:)%i)) call fatal('Surface &
            & id "'//sset(to_str(tcell(i)%rgn(j)))//'" not found specified for &
            & cell "'//sset(to_str(tcell(i)%i))//'"')
        end if
      end do
    end do

    deallocate(tsurf,trsrf)

    ! surface used inside the complement operator gives reverse neighbours
    ! therefore exclude surfaces within # operator

    ! CHECK : lattice cell surfaces are not excluded !!!!!!!!!!!!!!!!!!!!!!!!!!!---

    do i = 1, t_cell
      do j = 1, tcell(i)%nrgn
        if(tcell(i)%rgn(j)==OP_COMPLEMENT)exit
        if(tcell(i)%rgn(j)>=OP_UNION)cycle
        k = sur_ixf(iabs(tcell(i)%rgn(j)))
        if(k==0)cycle !<<<<<<!!!!!!!!!!!!!!!!!!!!!!!!!! already gave fatal error
        if(tcell(i)%rgn(j)>0)then
          surf(k)%nposi = surf(k)%nposi + 1
        else
          surf(k)%nnega = surf(k)%nnega + 1
        end if
      end do
    end do

    do i = 1, nsrf
      allocate(surf(i)%nneg(surf(i)%nnega))
      surf(i)%nneg = 0
      allocate(surf(i)%npos(surf(i)%nposi))
      surf(i)%npos = 0
    end do

    surf(:)%nnega = 0
    surf(:)%nposi = 0

    do i = 1, t_cell
      do j = 1, tcell(i)%nrgn
        if(tcell(i)%rgn(j)==OP_COMPLEMENT)exit
        if(tcell(i)%rgn(j)>=OP_UNION)cycle
        k = sur_ixf(iabs(tcell(i)%rgn(j)))
        if(k==0)cycle !<<<<<<<<<<!!!!!!!!!!!!!!!!!!!!already gave fatal error
        if(tcell(i)%rgn(j)>0)then
          surf(k)%nposi = surf(k)%nposi + 1
          surf(k)%npos(surf(k)%nposi) = tcell(i)%i
        else
          surf(k)%nnega = surf(k)%nnega + 1
          surf(k)%nneg(surf(k)%nnega) = tcell(i)%i
        end if
      end do
    end do
    if(dev)print*,'surface_lines read over'
  end subroutine i_surf_lines

  subroutine i_cell_lines
    integer :: ic, nw, iimp, ilat, iuni, ifil, icom, icell, ilik, irho, imat, &
      itmp, itmp1, itmp2, i, j, celid, celix, rl, ntkn, k, iopn, icls, itrl
    character(MAX_WORD_LEN) :: wrd(MAX_WORDS),rwrd(3)
    real(8) :: rtmp
    character(1) :: chr1
    logical :: wrdlgc, bnklgc, lskpcom, istart, iend, ixstart

    t_cell = cell_end - cell_start + 1

  !  print*,'str ->', t_cell,cell_start,cell_end

    allocate(cel_store%i(t_cell))
    cel_store%t = t_cell
    cel_store%i(1:t_cell) = 0
    do i = 1, t_cell
      wrd(1:MAX_WORDS) = '' ; nw = 0
      call split (iinp(i+cell_start-1),wrd,nw)
      if(.not.is_pint(wrd(1)))then
        call fatal('Cell id "'//sset(wrd(1))//'" is not a positive integer')
      else
        if(any(to_pint(sset(wrd(1)))==cel_store%i(:))) call fatal('Cell id "'//&
          sset(wrd(1))//'" is already used')
        cel_store%i(i) = to_pint(wrd(1))
      end if
    end do

    allocate(tcell(t_cell))

    tcell(:)%den  = 0.0_8
    tcell(:)%fil  = 0              ; tcell(:)%i    = 0
    tcell(:)%imp  = 0              ; tcell(:)%lat  = 0
    tcell(:)%like = 0              ; tcell(:)%mat  = 0
    tcell(:)%nrgn = 0              ; tcell(:)%nrpn = 0
    tcell(:)%trcl(1) = 0.0_8       ; tcell(:)%typ  = 0
    tcell(:)%trcl(2) = 0.0_8       ; tcell(:)%uni  = 0
    tcell(:)%trcl(3) = 0.0_8       ; tcell(:)%prcd = .false.
    tcell(:)%ncrgn = 0             ; tcell(:)%ltr = .false.
    tcell(:)%simple = .true.

    ic = 1 ; icell = 0 ; s_trcl_count = 0

    FOR_ALL_CELL: do while (any(.false. .eqv. tcell(:)%prcd))

      ! Initialize

      irho = 0 ; imat = 0 ; iimp = 0 ; ilat = 0 ; iuni = 0 ; ifil = 0
      icom = 0 ; ilik = 0

      icell = ic + cell_start - 1

      if(tcell(ic)%prcd)then  ! cycle for processed cells
        ic = ic + 1
        if(icell == cell_end) ic = 1
        cycle
      end if

      imat = index(iinp(icell),'mat' ) ; irho = index(iinp(icell),'rho')
      ilik = index(iinp(icell),'like') ; iuni = index(iinp(icell),'u')
      ifil = index(iinp(icell),'fill') ; iimp = index(iinp(icell),'imp:n')
      ilat = index(iinp(icell),'lat' ) ; icom = index(iinp(icell),'#')
      itrl = index(iinp(icell),'trcl')

      wrd(1:MAX_WORDS) = '' ; nw = 0
      call split(iinp(icell),wrd,nw)

      ! Id

      ! TODO: remove multiple fatal error for cell id

      if(.not.is_pint(wrd(1)))then
        call fatal('Cell id "'//sset(wrd(1))//'" is not a positive integer')
      else
        tcell(ic)%i = to_pint(wrd(1))
      end if

      CELL_SPEC: if(ilik /= 0)then

        ! Cycle if dependent cells are not ready

        if(.not.is_dependent_ready(iinp(icell)))then
          ic = ic + 1
          if(icell == cell_end) ic = 1
          cycle
        end if

        ! Process like cells ( mat, rho, u, fill, trcl, copy rgn, rpn )

        LIKE_CELL: do i = 1, nw

          if(wrd(i)=='like')then

            if(.not.is_pint(wrd(i+1)))call fatal('Can not convert like cell "'&
              //sset(wrd(i+1))//'" to positive number specified for cell "'// &
              sset(wrd(1))//'"')

            tcell(ic)%like = to_pint(wrd(i+1))

          elseif(wrd(i) == 'imp:n')then

            if(.not. is_int(wrd(i+1))) call fatal('Can not convert importance "&
            &'//sset(wrd(i+1))//'" to integer specified for cell "'//sset(wrd(1))&
            //'"')

            tcell(ic)%imp = to_int(wrd(i+1))

          elseif(wrd(i)=='mat')then

            if(.not.is_int(wrd(i+1)))call fatal('Can not convert material "' &
              //sset(wrd(i+1))//'" to integer specified for cell "'// &
              sset(wrd(1))//'"')

            itmp = to_int(wrd(i+1))

            if(itmp < 0)call fatal('Material id "'//sset(wrd(i+1))//'" is &
              & not a positive integer specified for cell "'//sset(wrd(1))//'"')

            tcell(ic)%mat = itmp

          elseif(wrd(i)=='rho')then

            if(.not.is_real(wrd(i+1)))call fatal('Can not convert density "' &
              //sset(wrd(i+1))//'" to real number specified for cell "'// &
              sset(wrd(1))//'"')
            tcell(ic)%den = to_real(wrd(i+1))

          elseif(wrd(i)=='u')then

            if(.not.is_pint(wrd(i+1)))call fatal('Can not convert universe "' &
              //sset(wrd(i+1))//'" to positive number specified for cell "' &
              //sset(wrd(1))//'"')

            tcell(ic)%uni = to_pint(wrd(i+1))

          elseif(wrd(i)=='fill')then

            ! TODO : enable pattern for like cell ??? ------

            if(.not.is_pint(wrd(i+1)))call fatal('Can not convert fill "'// &
              sset(wrd(i+1))//'" to positive number specified for cell "'// &
              sset(wrd(1))//'"')

            tcell(ic)%fil = to_pint(wrd(i+1))

          elseif(wrd(i)=='trcl')then

            iopn = index(iinp(icell),'(')
            icls = index(iinp(icell),')')

            if((iopn /= 0 .and. icls == 0).or.(iopn == 0 .and. icls /= 0))then

              if((iopn /= 0 .and. icls == 0).or.(iopn == 0 .and. icls /= 0)) &
                call fatal('Mismatched paranthesis for trcl for cell "'// &
                sset(wrd(1))//'"')

            elseif(iopn == 0 .and. icls == 0)then

              if(.not.is_pint(wrd(i+1))) call fatal( &
                'Can not convert TR number "'//sset(wrd(i+1))//&
                '" to positive integer specified for cell "'//sset(wrd(1))//'"')

              tcell(ic)%tr = to_pint(wrd(i+1))
              tcell(ic)%ltr = .true.

            else

              rwrd(:) = '' ; itmp = 0
              call split(iinp(icell)(iopn+1:icls-1),rwrd,itmp)

              if(itmp < 3) call fatal('Found less co-ordinates at trcl "('// &
                sset(iinp(icell)(iopn+1:icls-1))//')" for cell "'// &
                sset(wrd(1))//'"')
              if(itmp > 3) call fatal('Found more co-ordinates at trcl "('// &
                sset(iinp(icell)(iopn+1:icls-1))//')" for cell "'// &
                sset(wrd(1))//'"')

              if(.not.is_real(rwrd(1)))call fatal('Can not &
                & convert trcl co-ordinate "'//sset(rwrd(1))//'" &
                & to real specified for cell "'//sset(wrd(1))//'"')
              if(.not.is_real(rwrd(2)))call fatal('Can not &
                & convert trcl co-ordinate "'//sset(rwrd(2))//'" &
                & to real specified for cell "'//sset(wrd(1))//'"')
              if(.not.is_real(rwrd(3)))call fatal('Can not &
                & convert trcl co-ordinate "'//sset(rwrd(3))//'" &
                & to real specified for cell "'//sset(wrd(1))//'"')

              tcell(ic)%trcl(1) = to_real(rwrd(1))
              tcell(ic)%trcl(2) = to_real(rwrd(2))
              tcell(ic)%trcl(3) = to_real(rwrd(3))
              tcell(ic)%ltr = .true.
            end if

          end if

        end do LIKE_CELL

        ! copy things which are not specified at like cell

        ! Combine IFs

        if(imat == 0)then
          celix = cel_ix(tcell(ic)%like)
          tcell(ic)%mat = tcell(celix)%mat
        end if

        if(irho == 0)then
          celix = cel_ix(tcell(ic)%like)
          tcell(ic)%den = tcell(celix)%den
        end if

        if(iuni == 0)then
          celix = cel_ix(tcell(ic)%like)
          tcell(ic)%uni = tcell(celix)%uni
        end if

        if(ifil == 0)then
          celix = cel_ix(tcell(ic)%like)
          tcell(ic)%fil = tcell(celix)%fil
        end if

        if(iimp == 0)then
          celix = cel_ix(tcell(ic)%like)
          tcell(ic)%imp = tcell(celix)%imp
        end if

        if(itrl == 0)then ! if like cell not translated - it is used for replace
          celix = cel_ix(tcell(ic)%like) ! ment of actual cell which is not used
          if(tcell(celix)%ltr)then
            tcell(ic)%ltr = .true.
            tcell(ic)%tr = tcell(celix)%tr
            tcell(ic)%trcl = tcell(celix)%trcl
          end if
        end if

        ! create region string

        celix = cel_ix(tcell(ic)%like)

        if(celix==0)call fatal('cel_ix zero at like process - DEV')

        if(.not.tcell(ic)%ltr)then

          tcell(ic)%ncrgn = tcell(celix)%ncrgn
          allocate(character(tcell(ic)%ncrgn) :: tcell(ic)%crgn)
          tcell(ic)%crgn = tcell(celix)%crgn

        else

          istart = .true. ; iend = .false. ; ixstart = .true.
          ctmp = '' ; j = 0 ; k = 0

          do i = 1, tcell(celix)%ncrgn

            chr1 = tcell(celix)%crgn(i:i)

            ! Todo: Insert checking for " # " in dependent cell and give a fatal
            !       error ! handles if it is not circular depedancy

            ! #(1 -2) can be there in dependent cell string

            ! Remove this
            if(chr1 == '#')call fatal('# operator in dependent cell region string')

            !TODO: final check
            if(index('0123456789',chr1)/=0)then
              if(.not.ixstart)cycle
              ixstart = .false.
              do j = i+1, tcell(celix)%ncrgn
                if(index(' :)(',tcell(celix)%crgn(j:j)) /= 0)exit
              end do
              itmp = to_int(sset(tcell(celix)%crgn(i:j-1)))
              itmp = (tcell(ic)%i * 1000) + itmp
              ctmpx = sset(to_str(itmp))
              rl = len_trim(ctmpx)
              k = k + 1
              ctmp(k:k+rl-1) = ctmpx(1:rl)
              k = k + rl - 1
            elseif(index('() :+-',chr1)/=0)then
              ixstart = .true.
              k = k + 1
              ctmp(k:k) = chr1
            end if

          end do

          tcell(ic)%ncrgn = k
          allocate(character(tcell(ic)%ncrgn) :: tcell(ic)%crgn)
          tcell(ic)%crgn = sset(ctmp)

        end if

      else CELL_SPEC

        ! Cycle if dependent cells are not ready
        ! TODO: 2 IF conditions may needs to be splited.
        if((icom /= 0).and.(.not.is_dependent_ready(iinp(icell))))then
          ic = ic + 1
          if(icell == cell_end) ic = 1
          cycle
        end if

        ! Material Id

        if(.not.is_int(wrd(2)))then
          call fatal('Can not convert material id "'//sset(wrd(2))//'" to &
            & integer specified for cell "'//sset(wrd(1))//'"')
        else
          itmp = to_int(wrd(2))
          if(itmp < 0)call fatal('Material id "'//sset(wrd(2))//'" is not a &
            & positive integer specified for cell "'//sset(wrd(1))//'"')
          tcell(ic)%mat = itmp ; itmp = 0
          if(tcell(ic)%mat /= MAT_VOID)then
            ! Density
            if(.not.is_real(wrd(3))) call fatal('Density "'//sset(wrd(3))// &
              '" specified for cell "'//sset(wrd(1))//'" is not real number ')
            rtmp = to_real(wrd(3))
            tcell(ic)%den = rtmp ; rtmp = 0.0_8
          end if
        end if

        ! fill lat u imp trcl

        do i = 1, nw

          if(wrd(i) == 'fill')then

            ctmp = ''
            do j = i + 1, nw
              if(index('u imp:n lat',sset(wrd(j))) /= 0)exit
              ctmp = sset(ctmp)//' '//sset(wrd(j))
            end do
            ctmp = sset(ctmp) ! fill string or fill number
            if(index(sset(ctmp),':') /= 0)then
              allocate(character(len_trim(ctmp)) :: tcell(ic)%latfill)
              tcell(ic)%latfill = sset(ctmp) ! fill string
            else
              if(.not.is_pint(sset(ctmp))) call fatal('Can not convert fill "' &
                //sset(ctmp)//'" to positive integer specified for cell "' &
                //sset(wrd(1))//'"')
              itmp = to_pint(sset(ctmp)) ! fill number
              tcell(ic)%fil = itmp
            end if

          elseif(wrd(i) == 'u')then

            if(.not.is_pint(wrd(i+1))) call fatal('Can not convert universe "' &
              //sset(wrd(i+1))//'" to positive integer specified for cell "' &
              //sset(wrd(1))//'"')
            itmp = to_pint(wrd(i+1))
            tcell(ic)%uni = itmp

          elseif(wrd(i) == 'lat')then

            if(.not.is_pint(wrd(i+1))) call fatal('Can not convert lattice  &
              & type "'//sset(wrd(i+1))//'" to positive integer specified for &
              & cell "'//sset(wrd(1))//'"')
            itmp = to_pint(wrd(i+1))
            tcell(ic)%lat = itmp

          elseif(wrd(i) == 'imp:n')then

            if(.not.is_int(wrd(i+1))) call fatal('Can not convert importance &
              & "'//sset(wrd(i+1))//'" to integer specified for cell "' &
              //sset(wrd(1))//'"')
            itmp = to_int(wrd(i+1))
            if(itmp < 0)call fatal('Cell importance "'//sset(wrd(i+1))// &
              '" have negative value specified for cell "'//sset(wrd(1))//'"')
            tcell(ic)%imp = itmp

          elseif(wrd(i)=='trcl')then

            ! TODO: allow trcl for usual cell. and tr number
            call fatal('TRCL for usual cell is not allowed')

            iopn = index(iinp(icell),'(')
            icls = index(iinp(icell),')')

            if((iopn /= 0 .and. icls == 0).or.(iopn == 0 .and. icls /= 0))then

              if((iopn /= 0 .and. icls == 0).or.(iopn == 0 .and. icls /= 0)) &
                call fatal('Mismatched paranthesis for trcl for cell "'// &
                sset(wrd(1))//'"')

            elseif(iopn == 0 .and. icls == 0)then

              if(.not.is_pint(wrd(i+1))) call fatal('Can not convert TR number &
                &"'//sset(wrd(i+1))//'" to positive integer specified for cell "'&
                //sset(wrd(1))//'"')

              tcell(ic)%tr = to_pint(wrd(i+1))
              tcell(ic)%ltr = .true.

            else

              rwrd(:) = '' ; itmp = 0
              call split(iinp(icell)(iopn+1:icls-1),rwrd,itmp)

              if(itmp < 3) call fatal('Found less co-ordinates at trcl "('// &
                sset(iinp(icell)(iopn+1:icls-1))//')" for cell "'// &
                sset(wrd(1))//'"')
              if(itmp > 3) call fatal('Found more co-ordinates at trcl "('// &
                sset(iinp(icell)(iopn+1:icls-1))//')" for cell "'// &
                sset(wrd(1))//'"')

              if(.not.is_real(rwrd(1)))call fatal('Can not &
                & convert trcl co-ordinate "'//sset(rwrd(1))//'" &
                & to real specified for cell "'//sset(wrd(1))//'"')
              if(.not.is_real(rwrd(2)))call fatal('Can not &
                & convert trcl co-ordinate "'//sset(rwrd(2))//'" &
                & to real specified for cell "'//sset(wrd(1))//'"')
              if(.not.is_real(rwrd(3)))call fatal('Can not &
                & convert trcl co-ordinate "'//sset(rwrd(3))//'" &
                & to real specified for cell "'//sset(wrd(1))//'"')

              tcell(ic)%trcl(1) = to_real(rwrd(1))
              tcell(ic)%trcl(2) = to_real(rwrd(2))
              tcell(ic)%trcl(3) = to_real(rwrd(3))
              tcell(ic)%ltr = .true.
            end if

          end if

        end do


        ctmp = ''

        ! Pick region string as it is (**VVIMP**)


        ! Pick starting index of region itmp1

        j=0 ; wrdlgc = .true. ; bnklgc = .false. ; itmp1 = 0

        do i = 1, len_trim(iinp(icell))
          chr1 = iinp(icell)(i:i)
          if(len_trim(chr1)==0 .and. bnklgc)then
            j=j+1 ; bnklgc = .false. ; wrdlgc = .true.
          end if
          if(len_trim(chr1) /= 0 .and. wrdlgc)then
            j=j+1 ; bnklgc = .true. ; wrdlgc = .false.
          end if
          if(j==4 .and. tcell(ic)%mat == MAT_VOID)then
            itmp1 = i+1
            exit
          elseif(j==6)then
            itmp1 = i+1
            exit
          end if
        end do

        if(ifil == 0)ifil = huge(0) ; if(iuni == 0)iuni = huge(0)
        if(ilat == 0)ilat = huge(0) ; if(iimp == 0)iimp = huge(0)
        if(itrl == 0)itrl = huge(0)
        itmp2 = min(ifil,ilat,iuni,iimp,itrl)

        if(itmp2 == huge(0))itmp2 = len_trim(iinp(icell)) + 1

        ctmp = iinp(icell)(itmp1:itmp2-1)
        ctmp = sset(ctmp)

        if(icom == 0)then

          ! cells without complement

          tcell(ic)%ncrgn = len_trim(ctmp)
          allocate(character(tcell(ic)%ncrgn) :: tcell(ic)%crgn)
          tcell(ic)%crgn = sset(ctmp)


          ! TODO : if trcl do it here (# not allowed with trcl for usual cell)
          !      : 1 0 1 -2 #3 trcl (0.0 0.0 0.0) is not allowed
        else

          j = 0 ; rl = 0 ; ctmpx = '' ; lskpcom = .false.

          ! Replace #CELL with #(cell region)

          do i = 1, len_trim(ctmp)
            if(lskpcom .and. ctmp(i:i)==' ')lskpcom = .false.
            if(lskpcom)cycle
            if(ctmp(i:i)=='#' .and. ctmp(i+1:i+1) /= '(')then
              j = j + 1
              k = index(ctmp(i:len_trim(ctmp)),' ')
              if(k==0)k=len_trim(ctmp)
              if(.not.is_pint(ctmp(i+1:i+k-1)))call fatal('Can not convert &
                &complement cell "'//sset(ctmp(i+1:i+k-1))//'" to positive number&
                & specified for cell "'//sset(wrd(1))//'"')
              celid = to_pint(ctmp(i+1:i+k-1))
              celix = cel_ix(celid)
              if(celix==0)call fatal('cel_ix zero at complement process - DEV')
              rl = 2 + tcell(celix)%ncrgn
              ctmpx(j:j+rl) = '#('//sset(tcell(celix)%crgn)//')'
              j = j + rl
              lskpcom = .true.
            else
              j = j + 1
              ctmpx(j:j) = ctmp(i:i)
            end if
          end do

          tcell(ic)%ncrgn = len_trim(ctmpx)
          allocate(character(tcell(ic)%ncrgn) :: tcell(ic)%crgn)
          tcell(ic)%crgn = sset(ctmpx)
          ctmp = ''
          ctmp = sset(ctmpx) !  ??? why it is needed
        end if

      end if CELL_SPEC

      ! Tokenize

      allocate(itoken(tcell(ic)%ncrgn))
      call tokenize(tcell(ic)%crgn,itoken,ntkn)
      tcell(ic)%nrgn = ntkn
      allocate(tcell(ic)%rgn(ntkn))
      do i = 1, ntkn
        tcell(ic)%rgn(i) = itoken(i)
      end do

      ! change tcell(:)%crgn for OpenMC

      do i = 1, tcell(ic)%ncrgn
        if(tcell(ic)%crgn(i:i) == ':')tcell(ic)%crgn(i:i) = '|'
        if(tcell(ic)%crgn(i:i) == '#')tcell(ic)%crgn(i:i) = '~'
      end do


!      ! Generate RPNs

      itoken(1:ntkn) = 0 ; ntkn = 0

      call generate_rpn(tcell(ic)%i,tcell(ic)%rgn,tcell(ic)%nrgn,itoken,ntkn)
      tcell(ic)%nrpn = ntkn
      allocate(tcell(ic)%rpn(ntkn))
      do i = 1, ntkn
        tcell(ic)%rpn(i) = itoken(i)
      end do

      deallocate(itoken)

      tcell(ic)%prcd=.true.
      ic = ic + 1
      if(icell == cell_end) ic = 1

    end do FOR_ALL_CELL

    ncel = t_cell !; print*,t_cell ! total cells

    if(dev)print*,'cell lines process over'

  end subroutine i_cell_lines

  function is_dependent_ready(line) result(yesno)
    character(*) :: line
    logical :: yesno
    integer :: ilik, i, nw,celid, celix
    character(MAX_WORD_LEN) :: wrd(MAX_WORDS)

    yesno = .false.
    ilik = index(line,'like')

    if(ilik /= 0)then
      wrd(1:MAX_WORDS) = '' ; nw = 0
      call split(line,wrd,nw)
      if(.not.is_pint(wrd(3)))call fatal('Can not convert like cell "'// &
        sset(wrd(3))//'" to positive number specified for cell "'//sset(wrd(1)) &
        //'"')
      celid = to_pint(wrd(3))
      celix = cel_ix(celid)
      if(celix == 0)then
        if(any(cel_store%i(:)==celid))then
          return
        else
          call fatal('Like cell "'//sset(wrd(3))//'" not found specified for &
            & cell "'//sset(wrd(1))//'"')
          stop
        end if
      end if
      if(tcell(celix)%prcd)yesno = .true.
    else
      wrd(1:MAX_WORDS) = '' ; nw = 0
      call split(line,wrd,nw)
      do i = 1,nw
        if(index(sset(wrd(i)),'(') /= 0 .and.index(sset(wrd(i)),'#') == 1)then
          yesno = .true. ; cycle
        end if
        if(index(sset(wrd(i)),'#') == 1 .and. index(sset(wrd(i)),'(') == 0)then
          if(.not.is_pint(wrd(i)(2:len_trim(wrd(i)))))call fatal('Can not&
            & convert complement cell "'//sset(wrd(i)(2:len_trim(wrd(i))))// &
            '" to positive number specified for cell "'//sset(wrd(1))//'"')
          celid = to_pint(wrd(i)(2:len_trim(wrd(i))))
          celix = cel_ix(celid)
          if(celix == 0)then
            if(any(cel_store%i(:)==celid))then
              return
            else
              call fatal('Complement cell "#'//sset((wrd(i)(2:len_trim(wrd(i)) &
                )))//'" not found specified for cell "'//sset(wrd(1))//'"')
              stop
            end if
          end if
          if(tcell(celix)%prcd)then
            yesno = .true.
          else
            yesno = .false. ; return
          end if
        end if
      end do
    end if
  end function

  subroutine i_surf_templines

    integer  :: n, ncfs, typ, i, j, k, nwrds
    integer  :: iast, ipls, imin
    logical  :: isnum
    character(MAX_WORD_LEN)  :: wrd(MAX_WORDS)
    character(3)  :: chr3

    i = 0 ; j = 0 ; k = 0
    isnum = .false. ; chr3 = ''
    sp => null()

    tmp_surf = surf_end - surf_start + 1

    allocate(tsurf(tmp_surf))
    tsurf(:)%i     = 0
    tsurf(:)%peri  = 0
    tsurf(:)%ix    = 0
    tsurf(:)%ctyp  = ''
    tsurf(:)%ityp  = 0
    tsurf(:)%ncf   = 0
    tsurf(:)%bc    = BC_TRANSMIT

    j = 1
    do i = surf_start, surf_end
      n = 0 ; ncfs = 0 ; typ = 0 ; nwrds = 0
      wrd(1:MAX_WORDS) = ''
      call split(iinp(i),wrd,nwrds)
      iast = index(wrd(1),'*')
      ipls = index(wrd(1),'+')
      imin = index(wrd(2),'-')

      ! Get surface id

      if(iast == 0 .and. ipls == 0)then
        if(is_pint(wrd(1)))then
          if(any(tsurf(:)%i==to_pint(wrd(1)))) &
            call fatal(' Surface id "'//wrd(1)(1:len_trim(wrd(1))) &
            //'" is already used')
          tsurf(j)%i = to_pint(wrd(1))
          tsurf(j)%ix = j
        else
          call fatal('"'//sset(wrd(1))// &
            '" is not a proper surface id - use only numbers')
        end if
      else
        if(is_int(wrd(1)(2:len_trim(wrd(1)))))then
          tsurf(j)%i = to_int(wrd(1)(2:len_trim(wrd(1))))
        else
          call fatal('"'//sset(wrd(1)(2:len_trim(wrd(1))))// &
            '" is not a proper surface id - use only numbers')
        end if
      end if

      ! Reflective or white boundary

      if(iast /= 0 .or. ipls /= 0)then
        if(iast /= 0)tsurf(j)%bc = BC_REFLECT
        if(ipls /= 0)tsurf(j)%bc = BC_WHITE
        if(imin == 0)chr3 = sset(wrd(2))
        if(imin /= 0)chr3 = sset(wrd(3))
        call surfid(chr3,typ,ncfs)
        if(typ > 4 .and. tsurf(j)%bc == BC_REFLECT) &
          call fatal('Surface "'//sset(wrd(1))// &
          '" is not a plane to have reflective boundary ')
        if(typ > 4 .and. tsurf(j)%bc == BC_WHITE) &
          call fatal('Surface "'//sset(wrd(1))// &
          '" is not a plane to have white boundary ')
        if(imin /= 0 .and. tsurf(j)%bc == BC_REFLECT) &
          call fatal('Reflective surface "'//sset(wrd(1))// &
          '" can not be periodic')
        if(imin /= 0 .and. tsurf(j)%bc == BC_WHITE) &
          call fatal('White boundary surface "'//sset(wrd(1))// &
          '" can not be periodic')
      end if

      if(imin /= 0)then

        ! Periodic surface handling

        tsurf(j)%bc = BC_PERIODIC
        chr3 = sset(wrd(3))
        call surfid(chr3,typ,ncfs)
        if(typ > 4)call fatal(' Surface "'//sset(wrd(1))// &
          '" is not a plane to have periodic boundary ')
        tsurf(j)%ctyp = chr3
        tsurf(j)%ityp = typ
        tsurf(j)%ncf  = ncfs
        tsurf(j)%peri = IABS(to_nint(wrd(2)))
        allocate(tsurf(j)%coef(ncfs))
        tsurf(j)%coef(1:ncfs) = 0.0_8
        if(nwrds > ncfs+3) call fatal('Found more co-efficients than required  &
          & for surface "'//sset(to_str(tsurf(j)%i))//'"')
        if(nwrds < ncfs+3) call fatal('Found less co-efficients than required  &
          & for surface "'//sset(to_str(tsurf(j)%i))//'"')
        do k = 1,ncfs
          tsurf(j)%coef(k) = to_real(sset(wrd(k+3)))
        end do
      else

        ! Save co-efficients for Normal, Reflective and White-boundary

        chr3 = sset(wrd(2))
        call surfid(chr3,typ,ncfs)
        tsurf(j)%ctyp = chr3
        tsurf(j)%ityp = typ
        tsurf(j)%ncf  = ncfs
        allocate(tsurf(j)%coef(ncfs))
        tsurf(j)%coef(1:ncfs) = 0.0_8
        if(nwrds > ncfs+2) call fatal('Found more co-efficients than required  &
          & for surface "'//sset(to_str(tsurf(j)%i))//'"')
        if(nwrds < ncfs+2) call fatal('Found less co-efficients than required  &
          & for surface "'//sset(to_str(tsurf(j)%i))//'"')
        do k = 1,ncfs
          tsurf(j)%coef(k) = to_real(sset(wrd(k+2)))
        end do
      end if
      j = j + 1
    end do

    ! Check for periodic surfaces

    do i = 1, tmp_surf
      if(tsurf(i)%bc == BC_PERIODIC)then
        if(.not.ANY(tsurf(:)%i == tsurf(i)%peri))then
          call fatal('Periodic surface "'//sset(to_str(tsurf(i)%peri)) &
            //'" for surface "'//sset(to_str(tsurf(i)%i))//'" not found')
        else
          if(tsurf(i)%ityp /= tsurf(sur_ix(tsurf(i)%peri))%ityp)then
            call fatal('Periodic surfaces "'//sset(to_str(tsurf(i)%i)) &
              //'" and "'//sset(to_str(tsurf(i)%peri))//'" are not of &
              & same surface type')
          else
            if(tsurf(sur_ix(tsurf(i)%peri))%peri /= tsurf(i)%i) call fatal &
              ('Periodic surfaces "'//sset(to_str(tsurf(i)%i))//'" and "' &
              //sset(to_str(tsurf(i)%peri))//'" are not bounded to each other')
          end if
        end if
      end if
    end do
if(dev)print*,'i_surf_temp_lines over'
  end subroutine

  subroutine i_process

    ! 1.Reading input user file and storing
    ! 2.Dividing input into cell, surface and data block
    ! 3.Concatenate multiple lines to single line

    logical :: file_exist
    integer :: ic, ik, idol, iamp, i, j, k, io, bln1, bln2, bln3
    character(5) :: chr5
    character(1) :: chr1
    character(MAX_LINE_LEN) :: str

    ! Initialize

    long_line = 0
    file_exist = .false.
    ic = 0 ; ik = 0 ; idol = 0 ; iamp = 0 ; i = 0 ; j = 0 ; k = 0
    io = 0 ; bln1 = 0 ; bln2 = 0 ; bln3 = 0
    chr5 = '' ; chr1 = '' ; str = ''

    inpfile = sset(inpfile)
!    if(dev)call color1(inpfile,4,1)
    inquire(file=inpfile,exist=file_exist)
    if(.not. file_exist) &
      call fatal('Input file "'//sset(inpfile)//'" does not exists',.true.)
    open(unit=inp, file=inpfile, status='old', action='read', iostat=io)
    if(io /= 0) call fatal('Input file opening error')

    do while(bln3 == 0)

      ! Not to count blank lines after data block as input lines
      ! Error check for absence of blank line between different blocks

      read(inp,'(A)',iostat=io)str
      if(io == 0)i = i + 1
      long_line = max(long_line,len_trim(str))

      if(len_trim(str) == 0 .and. bln1 == 0)then
        if(i <= 1) call fatal('Title line is blank',.true.)
        bln1 = i ; cycle
      end if
      if(len_trim(str) == 0 .and. bln1 /= 0 .and. bln2 == 0)then
        bln2 = i ; cycle
      end if
      if(len_trim(str) == 0 .and. bln2 /= 0 .and. bln3 == 0)then
        bln3 = i
      end if
      if(io /= 0 .and. bln1 /= 0 .and. bln2 /= 0) bln3 = i + 1
      if(io /= 0) exit
    end do

    if((bln3 - bln2 < 2) .or. (bln2 - bln1 < 2))then
      call fatal('No blank line/s between the cell, surface or data block/s OR &
        &multiple blank lines after a block')
    end if

    t_lines = bln3 - 1
    allocate(character(long_line) :: iusr(t_lines))
    rewind(inp)

    do i = 1, t_lines
      read(inp,'(A)')iusr(i)
      iusr(i) = trim(lower(iusr(i)))
    end do

    long_line = 0

    do i = 1, t_lines

      ! Count input lines without comment and remove end of line comment

      if(i == 1)then
        j = j + 1 ; cycle
      end if

      if(.not.is_comment(iusr(i)))then
        j = j + 1
        idol = index(iusr(i),'$')
        if(idol > 0)iusr(i) = iusr(i)(1:idol-1)
        long_line = max(long_line,len_trim(iusr(i)))
      end if
    end do

    allocate(character(long_line) :: iprc(j))
    k = 0

    do i = 1, t_lines

      ! Transfer lines to iprc (lines without comment)

      if(i == 1)then
        k = k + 1
        iprc(k) = trim(iusr(i))
        cycle
      end if

      if(.not.is_comment(iusr(i)))then
        k = k + 1
        iprc(k) = trim(iusr(i))
      end if
    end do

    t_lines = k
    iproc     = 1 ! Processing line
    kinp      = 0 ! Actual input lines for further processing
    long_line = 0 ! Length of long line for allocation
    allocate(tinp(t_lines)) ! temporary big array for process
    tinp = ''

    SINGLE: do
      if(.not.is_continue(iproc))then
        kinp = kinp + 1
        tinp(kinp) = trim(iprc(iproc))
        long_line = max(long_line,len_trim(iprc(iproc)))
        if(iproc == t_lines)exit SINGLE
        iproc = iproc + 1
      else
        ctmp = ''  ! Temporary long string for processing
        CONTINUES: do
          if(is_continue(iproc))then
            iamp = index(iprc(iproc),'&')
            if(iamp > 0)iprc(iproc) = iprc(iproc)(1:iamp-1)
            ctmp = trim(ctmp)//' '//sset(iprc(iproc))
            iproc = iproc + 1
            if(len_trim(ctmp) > 2000)then
              call fatal('Input line number "'//sset(to_str(kinp))//'" exceeds &
              &max allowed length of 2000 character',.true.)
            end if
          else
            kinp = kinp + 1
            ctmp = trim(ctmp)//' '//sset(iprc(iproc))
            long_line = max(long_line,len_trim(ctmp))
            tinp(kinp) = sset(ctmp)
            iproc = iproc + 1
            exit CONTINUES
          end if
        end do CONTINUES
      end if
      if(iproc > t_lines)exit SINGLE
    end do SINGLE

    t_lines = kinp
    allocate(character(long_line) :: iinp(t_lines))
    cell_start = 2 ; cell_end   = 0 ; surf_start = 0
    surf_end   = 0 ; data_start = 0 ; data_end   = t_lines

    do i = 1, t_lines
      !print*,i,'-- ',sset(tinp(i))
      iinp(i) = sset(tinp(i))
      if(len_trim(iinp(i)) == 0)then
        if(cell_end == 0)cell_end = i - 1
        if(surf_start == 0)surf_start = i + 1
        surf_end = i - 1
        data_start = i + 1
      end if
    end do

    deallocate(tinp)
    deallocate(iprc)

    close(inp)

    if(dev)print*,'i_process over'

  end subroutine

  function is_continue(iproc) result(contline)

    ! Checks whether a line has continuation line.

    integer :: iproc
    logical :: contline
    integer :: iamp
    character(5) :: chr5

    contline = .false. ; iamp = 0 ; chr5 = ''

    if(iproc == 1)return

    if(iproc < t_lines) iamp = index(iprc(iproc),'&')
    if(iproc < t_lines)chr5 = iprc(iproc + 1) (1:5)
    if(iproc < t_lines)then
      if((iamp > 0 .or. trim(chr5) == '').and.len_trim(iprc(iproc+1)) /= 0)then
        contline = .true.
      end if
    else
      contline = .false.
    end if

  end function

  function is_comment(iline) result(comment)

    ! Checks whether a line is a comment.

    integer :: ic, ik
    logical :: comment
    character(5) :: chr5
    character(1) :: chr1
    character(*) :: iline

    comment = .false. ; chr5 = '' ; ic = 0 ; ik = 0 ; chr1 = ''

    chr5 = iline(1:5)
    ic = index(lower(chr5),'c')
    ik = index(lower(chr5),'ksrc')
    chr1 = iline(ic+1:ic+1)
    if(trim(chr5) /= '' .and. ic > 0 .and. ik == 0 .and. chr1 == ' ' ) &
      comment = .true.

  end function


!  subroutine i_mats_process
!    integer :: i,j,k,index_list
!    type(Material), pointer :: mat => null()
!    real(8) :: sum_percent, awr, x
!    logical :: percent_in_atom, density_in_atom
!
!    ! normalize ao_
!
!    ! first find the index in the xs_listings array for each nuclide in each
!    ! material
!
!    do i = 1, ncel ! nmat
!
!      if(tcell(i)%mat == MAT_VOID) cycle
!
!      mat => mats(mat_ix(tcell(i)%mat))
!
!      percent_in_atom = (mat%atom_density(1) > ZERO)
!      density_in_atom = (tcell(i)%den > ZERO)
!
!      allocate(tcell(i)%atom_density(mat%n_nuclides))
!      ! init-----------------------------------------------------------
!
!      sum_percent = ZERO
!      do j = 1, mat%n_nuclides
!        ! determine atomic weight ratio
!        if(xsl_ix(mat%names(j)) == -huge(0)) print*,mat%names(j)
!        awr = xslists(xsl_ix(mat%names(j)))%awr
!!
!!        ! if given weight percent, convert all values so that they are divided
!!        ! by awr. thus, when a sum is done over the values, it's actually
!!        ! sum(w/awr)
!        if (.not. percent_in_atom) then
!          mat%atom_density(j) = -mat%atom_density(j) / awr
!        end if
!      end do
!!
!!      ! determine normalized atom percents. if given atom percents, this is
!!      ! straightforward. if given weight percents, the value is w/awr and is
!!      ! divided by sum(w/awr)
!      sum_percent = sum(mat%atom_density)
!      mat%atom_density = mat%atom_density / sum_percent
!
!      ! Change density in g/cm^3 to atom/b-cm. Since all values are now in atom
!      ! percent, the sum needs to be re-evaluated as 1/sum(x*awr)
!      if (.not. density_in_atom) then
!        sum_percent = ZERO
!        do j = 1, mat%n_nuclides
!
!          awr = xslists(xsl_ix(mat%names(j)))%awr
!          x = mat%atom_density(j)
!          sum_percent = sum_percent + x*awr
!        end do
!        sum_percent = ONE / sum_percent
!        tcell(i)%den = -tcell(i)%den * N_AVOGADRO &
!             / MASS_NEUTRON * sum_percent
!      end if
!
!      ! Calculate nuclide atom densities
!    !  mat%atom_density = tcell(i)%den * mat%atom_density
!      tcell(i)%atom_density = tcell(i)%den * mat%atom_density
!
!     ! print*,i,'--',tcell(i)%den,tcell(i)%atom_density
!
!    !___________________________________________________________________________
!    !  in mat atom density
!
!    end do
!
!    ! unique nuc
!
!    nnuc = sum(mats(:)%n_nuclides)
!
!    allocate(nuc_store%c(nnuc))
!
!    nuc_store%t = 0
!    nuc_store%c(:) = ''
!
!    do i = 1, nmat
!      do j = 1, mats(i)%n_nuclides
!        if(.not.any(nuc_store%c == mats(i)%names(j)))then
!          nuc_store%t = nuc_store%t + 1
!          nuc_store%c(nuc_store%t) = mats(i)%names(j)
!        end if
!      end do
!    end do
!
!    nnuc = nuc_store%t
!
!!    print*,' unique nuc'
!!    print'(a15)',nuc_store%c(1:nnuc)
!
!    ! unique sab
!
!    nsab = sum(mats(:)%n_sab)
!
!    allocate(sab_store%c(nsab))
!
!    sab_store%t = 0
!    sab_store%c(:) = ''
!
!    do i = 1, nmat
!      if(mats(i)%n_sab == 0)cycle
!      if(.not.any(sab_store%c == mats(i)%sab_names(1)))then
!        sab_store%t = sab_store%t + 1
!        sab_store%c(sab_store%t) = mats(i)%sab_names(1)
!      end if
!    end do
!
!    nsab = sab_store%t
!if(dev)print*,'i_mat_process over'
!  end subroutine

end module

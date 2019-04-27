module xmlwrite
  use xmlparse
  use global
  use input

  implicit none

  type(XML_PARSE) :: info
  character(len=20)                  :: tag
  character(len=900), dimension(2,10) :: attribs
  character(len=500), dimension(10)   :: data
  integer                            :: no_attribs,i
  integer                            :: no_data
  character(len=20)                  :: type

  contains

  subroutine write_xmls
    ! Write OpenMC input files
    call w_geometry
    call w_settings
    call w_materils
  end subroutine

  subroutine w_geometry
    ! Write geometry.xml file

    integer :: i
    real(8) :: r1, r2, r3, r4

    write(*,*)'Writing geometry.xml file...'

    ! open a geometry.xml file
    call xml_open( info, "geometry.xml", .false. )

    ! creates <geometry>   open tag
    call xml_put( info, "geometry", attribs, no_attribs, data, no_data, "open" )

    do i = 1, ncel
      ! Write all cells

      ! Do not write importance zero cell -- need to check
      if(tcell(i)%imp == 0)cycle

      info%level = 1

      no_attribs   = 4
      if(tcell(i)%uni == 0)no_attribs = 3

      attribs(1,1) = "id"
      attribs(2,1) = sset(to_str(tcell(i)%i)) ! "1"

      if(tcell(i)%uni /= 0)then
        attribs(1,2) = "universe"
        attribs(2,2) = sset(to_str(tcell(i)%uni))
      if(tcell(i)%fil /= 0)then
        attribs(1,3) = "fill"
        attribs(2,3) = sset(to_str(tcell(i)%fil))
      else
        attribs(1,3) = "material"
        if(tcell(i)%mat /= MAT_VOID)then
          attribs(2,3) = sset(to_str(tcell(i)%mat))
        else
          attribs(2,3) = 'VOID'
        end if
      end if
      attribs(1,4) = "region"
      attribs(2,4) = tcell(i)%crgn
    else
      if(tcell(i)%fil /= 0)then
        attribs(1,2) = "fill"
        attribs(2,2) = sset(to_str(tcell(i)%fil))
      else
        attribs(1,2) = "material"
        if(tcell(i)%mat /= MAT_VOID)then
          attribs(2,2) = sset(to_str(tcell(i)%mat))
        else
          attribs(2,2) = 'VOID'
        end if
      end if
      attribs(1,3) = "region"
      attribs(2,3) = tcell(i)%crgn
    end if

    call xml_put( info, "cell", attribs, no_attribs, data, no_data, "elem")

  end do

  write( info%lun,*)''

  do i = 1, nsrf
    no_data = 0
    no_attribs   = 3
    if(surf(i)%bc == BC_VACUUM)no_attribs   = 4

      attribs(1,1) = 'id'
      attribs(2,1) = sset(to_str(surf(i)%i))

      attribs(1,2) = 'type'

      if(surf(i)%ityp == P_P) attribs(2,2) = 'plane'
      if(surf(i)%ityp == P_PX)attribs(2,2) = 'x-plane'
      if(surf(i)%ityp == P_PY)attribs(2,2) = 'y-plane'
      if(surf(i)%ityp == P_PZ)attribs(2,2) = 'z-plane'
      if(any(surf(i)%ityp == [S_S,S_SX,S_SY,S_SZ,S_O]))attribs(2,2) = 'sphere'
      if(any(surf(i)%ityp == [C_CX,C_CXX]))attribs(2,2) = 'x-cylinder'
      if(any(surf(i)%ityp == [C_CY,C_CYY]))attribs(2,2) = 'y-cylinder'
      if(any(surf(i)%ityp == [C_CZ,C_CZZ]))attribs(2,2) = 'z-cylinder'

      attribs(1,3) = 'coeffs'

      if(surf(i)%ityp == P_P) attribs(2,3) = sset(to_str(surf(i)%coef(1)))//' '//&
        sset(to_str(surf(i)%coef(2)))//' '//sset(to_str(surf(i)%coef(3)))// ' '//&
        sset(to_str(surf(i)%coef(4)))
      if(any(surf(i)%ityp == [P_PX,P_PY,P_PZ])) &
        attribs(2,3) = sset(to_str(surf(i)%coef(1)))

      if(surf(i)%ityp == S_S)then
        r1 = surf(i)%coef(1)
        r2 = surf(i)%coef(2)
        r3 = surf(i)%coef(3)
        r4 = surf(i)%coef(4)
      elseif(surf(i)%ityp == S_SX)then
        r1 = surf(i)%coef(1) ; r2 = 0.0_8 ; r3 = 0.0_8
        r4 = surf(i)%coef(2)
      elseif(surf(i)%ityp == S_SY)then
        r2 = surf(i)%coef(1) ; r1 = 0.0_8 ; r3 = 0.0_8
        r4 = surf(i)%coef(2)
      elseif(surf(i)%ityp == S_SZ)then
        r3 = surf(i)%coef(1) ; r2 = 0.0_8 ; r1 = 0.0_8
        r4 = surf(i)%coef(2)
      elseif(surf(i)%ityp == S_O)then
        r1 = 0.0_8 ; r2 = 0.0_8 ; r3 = 0.0_8
        r4 = surf(i)%coef(1)
      end if
      if(any(surf(i)%ityp == [S_S,S_SX,S_SY,S_SZ,S_O])) &
        attribs(2,3) = sset(to_str(r1))//' '//sset(to_str(r2))//' '// &
        sset(to_str(r3))//' '//sset(to_str(r4))

      if(any(surf(i)%ityp == [C_CXX,C_CYY,C_CZZ])) &
        attribs(2,3) = sset(to_str(surf(i)%coef(1)))//' '//&
        sset(to_str(surf(i)%coef(2)))//' '//sset(to_str(surf(i)%coef(3)))
      if(any(surf(i)%ityp == [C_CX,C_CY,C_CZ])) &
        attribs(2,3) = '0.0 0.0 '//sset(to_str(surf(i)%coef(1)))

      if(no_attribs == 4)then
        attribs(1,4) = 'boundary'
        attribs(2,4) = 'vacuum'
      end if

      call xml_put( info, "surface", attribs, no_attribs, data, no_data, "elem")

    end do

    info%level = 0
    call xml_put( info, "geometry", attribs, no_attribs, data, no_data, "close")

  end subroutine

  subroutine w_settings
    ! open a settings.xml file

    write(*,*)'Writing Settings.xml file...'

    call xml_open( info, "settings.xml", .false. )
    no_attribs = 0
    call xml_put( info, "settings", attribs, no_attribs, data, no_data, "open")
    info%level = 1
    !  <eigenvalue>
    !call xml_put(info, "eigenvalue", attribs, no_attribs, data, no_data, "open")

    !  <batches>3000</batches>

    no_attribs = 0
    no_data = 1
    data(1) = 'eigenvalue'
    call xml_put( info, "run_mode", attribs, no_attribs, data, no_data, "elem")

    no_attribs = 0
    no_data = 1
    data(1) = sset(to_str(n_batches))
    call xml_put( info, "batches", attribs, no_attribs, data, no_data, "elem")

    !  <inactive>20</inactive>
    data(1) = sset(to_str(n_inactive))
    call xml_put( info, "inactive", attribs, no_attribs, data, no_data, "elem")

    !  <particles>10000</particles>
    data(1) = sset(to_str(n_particles))
    call xml_put( info, "particles", attribs, no_attribs, data, no_data, "elem")

    !  </eigenvalue>
    !call xml_put(info,"eigenvalue", attribs, no_attribs, data, no_data, "close")

    !  <source>
    call xml_put( info, "space", attribs, no_attribs, data, no_data, "open" )
    call xml_put( info, "space", attribs, no_attribs, data, no_data, "open" )

    data(1) = 'point'
    call xml_put( info, "type", attribs, no_attribs, data, no_data, "elem" )

    data(1) = ''

    do i = 1, nksrc
      data(1) = trim(data(1))//' '//sset(to_str(ksrc(i,1),2))//' '// &
      sset(to_str(ksrc(i,2),2))//' '//sset(to_str(ksrc(i,3),2))
    end do

    call xml_put(info,"parameters", attribs, no_attribs, data, no_data, "elem")
    call xml_put( info, "space", attribs, no_attribs, data, no_data, "close")
    call xml_put( info, "space", attribs, no_attribs, data, no_data, "close")

    info%level = 0
    call xml_put( info, "settings",attribs,no_attribs, data, no_data, "close")

  end subroutine

  subroutine w_materils
    ! open a materials.xml file

    integer :: i,j
    real(8) :: r1
    write(*,*)'Writing Materials.xml file...'

    call xml_open( info, "materials.xml", .false. )

    no_attribs = 0
    no_data = 0

    call xml_put( info, "materials",attribs,no_attribs, data, no_data, "open")

    info%level = 1

    do i =1, nmat
      no_attribs = 1
      attribs(1,1) = "id"
      attribs(2,1) = sset(to_str(mats(i)%i))
      call xml_put( info,"material",attribs, no_attribs, data, no_data, "open")

      no_attribs = 2
      info%level = 2
      do j = 1, ncel
        if(mats(i)%i == tcell(j)%mat)then
          r1 = tcell(j)%den
          exit
        end if
      end do

      if(r1>0.0_8)then
        attribs(1,1) = "value"
        attribs(2,1) = sset(to_str(r1))
        attribs(1,2) = "units"
        attribs(2,2) = 'atom/b-cm'
      else
        attribs(1,1) = "value"
        attribs(2,1) = sset(to_str(-r1))
        attribs(1,2) = "units"
        attribs(2,2) = 'g/cc'
      end if

      call xml_put( info, "density", attribs, no_attribs, data, no_data, "elem")

      no_attribs = 2

      do j = 1, mats(i)%n_nuclides
        attribs(1,1) = 'name'
        attribs(2,1) = mats(i)%names(j)
        if(mats(i)%atom_density(j)>0.0_8)then
          attribs(1,2) = 'ao'
          attribs(2,2) = sset(to_str(mats(i)%atom_density(j)))
        else
          attribs(1,2) = 'wo'
          attribs(2,2) = sset(to_str(- mats(i)%atom_density(j)))
        end if
        call xml_put(info,"nuclide",attribs, no_attribs, data, no_data, "elem")
      end do

      if(mats(i)%n_sab /= 0)then
        no_attribs = 1
        attribs(1,1) = 'name'
        attribs(2,1) = mats(i)%sab_names(1)
        call xml_put(info,"sab",attribs, no_attribs, data, no_data, "elem")
      end if

      info%level = 2

      call xml_put( info, "material", attribs,no_attribs, data, no_data,"close")
    end do

    info%level = 0
    call xml_put( info,"materials",attribs, no_attribs, data, no_data, "close" )

  end subroutine

end module

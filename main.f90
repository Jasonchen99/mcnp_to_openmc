
program mcnp_to_openmc

!!  Converts MCNS input to provide OpenMC input XML files
!!
!!  user guidelines
!!
!!   I hope you are familiar with the mcnp input specifications
!!
!!   For now this code handles only basic input cards of mcnp
!!
!!   The list of things to be taken care for now
!!
!!     1. In cell card only fill, u and imp:n are allowed
!!     2. trcl not allowed.
!!     3. Complement operator can be used.  Eg.,  1 -2 #3
!!        #3 will be replaced by #(region of cell 3)
!!     4. Surfaces of the cell which has zero importances take vacuum boundary condition
!!     5. Periodic and reflective surfaces are not allowed
!!     6. Cones and toriod are not allowed
!!     7. Isotopes names needd to be replaced with the names as per your OpenMC
!!        library in the generated materials.xml file
!!     8. m, mt, ksrc and kcode are the only processable cards.
!!     9. And need to declare cross_sections.xml in created inputs.
!!
!!
!!  Installation:
!!
!!    1. if you have the git installed in your computer then:
!!       Run "git clone https://github.com/sachinshet1992/mcnp_to_openmc.git && make"
!!
!!		  or
!!
!!    2. Download to your desktop and run "make" in the source directory
!!
!!	  After this you will have mcnp_to_openmc executable in the /bin/Release directory
!!  Using:
!!
!!    1. keep the MCNP input file with filename "inp" the executable directory and run.
!!    2. files with other names can be given as command line argument eg., ./mcnp_to_openmc filename
!!
!!  Output:
!!	  settings.xml, materials.xml and geometry.xml files will be created.
!!
!!
!!  Contact:
!!  iamsachinshet@gmail.com

  use input
  use xmlwrite

  implicit none

  dev = .false.
  fatalstop = .false.

  call get_command(str1)

  inpfile = sset(inputfilename(str1))

  write(*,*)'!-------------------------!'
  write(*,*)'!    MCNP to OpenMC       !'
  write(*,*)'!     input convert       !'
  write(*,*)'!-------------------------!'
  write(*,*)
  write(*,*)'MCNP input file = "',trim(adjustl(inpfile)),'"'
  write(*,*)
  write(*,*)'MCNP ->'
  write(*,*)

  call i_routines

  if(fatalcount /= 0)call fatal(' Errors in input',.true.)

  write(*,*)
  write(*,*)'OpenMC ->'
  write(*,*)

  call write_xmls

  deallocate(iinp)
  deallocate(iusr)
  deallocate(tcell)
  deallocate(surf)
  deallocate(mats)

contains

  ! Handle command line

  function inputfilename(str)result(files)
    integer :: n
    character(125) :: str , files,wrd(10)
    call split(str,wrd,n)
    if(n<=1)then
      files = 'inp'
      return
    end if
    files = trim(adjustl(wrd(2)))
  end function

end program


# mcnp_to_openmc
Converts MCNS input to provide OpenMC input XML files

  ! user guidelines

  ! I hope you are familiar with the mcnp input specifications

  ! For now this code handles only basic input cards of mcnp

  ! The list of things to be taken care for now

  !   1. In cell card lattices are not allowed for now
  !   2. trcl allowed only in like cells
  !   3. Complement operator can be used.  Eg.,  1 -2 #3
  !   4. Surfaces of the cell which has zero importances take vacuum boundary condition
  !   5. Periodic and reflective surfaces are not allowed
  !   6. Cones and toriod are not allowed
  !   7. Isotopes names needd to be replaced with the names as per your OpenMC
  !      library in th generated materials.xml file
  !   8. m, mt, ksrc and kcode are the only processable cards.
  !   9. And need to declare cross_sections.xml in created inputs.


  ! Other useful stuffs:

  !  https://youtu.be/mrfjxpSUFCg
  !  https://youtu.be/5FIVyWB4MRc

Contact:
iamsachinshet@gmail.com

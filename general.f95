module general
  use constants
  implicit none

  interface to_str
    module procedure int4_to_str, int8_to_str, real_to_str
  end interface

  integer :: fatalcount, warncount

  contains

  pure function sset(str) result(strr)
    character(*), intent(in) :: str
    character(len_trim(adjustl(str))) :: strr
    strr = trim(adjustl(str))
  end function

  subroutine warn(str)
    character(*) :: str
    warncount = warncount + 1
    print*,'WARNING : '//trim(adjustl(str))
  end subroutine



  subroutine fatal(str,stops)
    character(*) :: str
    logical, optional :: stops
    fatalcount = fatalcount + 1
    if(present(stops))fatalstop = stops
    print*,'FATAL ERROR : '//trim(adjustl(str))
    if(fatalstop)stop
  end subroutine

  subroutine msg(str)
    character(*) :: str
    print*,trim(adjustl(str))
  end subroutine
!_______________________________________________________________________________

  function to_int(word) result(num)
    character(*) :: word
    integer :: num, io
    character(len=(len_trim(adjustl(word)))) :: sac
    sac = trim(adjustl(word))
    read(sac,*,iostat=io)num
    if(io /= 0 .or. len_trim(word) == 0) &
      call fatal('Cannot convert "'//sset(word)//'" to integer')
  end function

  function is_int(word) result(num)
    character(*), intent(in) :: word
    logical                  :: num
    integer :: numm
    integer :: io
    character(len=(len_trim(adjustl(word)))) :: sac
    sac = trim(adjustl(word))
    num = .true.
    read(sac,*,iostat=io)numm
    if(io /= 0 .or. len_trim(word) == 0)then
      num = .false.
    end if
  end function is_int

  function to_pint(word) result(num)
    character(*), intent(in) :: word
    integer :: num
    integer :: io
    character(len=(len_trim(adjustl(word)))) :: sac
    sac = trim(adjustl(word))
    num = -1
    read(sac,'(I11)',iostat=io)num
    if(io /= 0 .or. len_trim(word) == 0)then
      call fatal('Cannot convert "'//sset(word)//'" to integer')
    elseif(num <= 0)then
      call fatal('"'//sset(word)//'" is not a positive integer')
    end if
  end function to_pint

  function is_nint(word) result(num)
    character(*), intent(in) :: word
    logical :: num
    integer :: io, numm
    character(len=(len_trim(adjustl(word)))) :: sac
    sac = trim(adjustl(word))
    num = .true.
    read(sac,*,iostat=io)numm
    if(io /= 0 .or. len_trim(word) == 0)then
      num = .false.
    elseif(numm >= 0)then
      num = .false.
    end if
  end function is_nint

  function to_nint(word) result(num)
    character(*), intent(in) :: word
    integer :: num
    integer :: io
    character(len=(len_trim(adjustl(word)))) :: sac
    sac = trim(adjustl(word))
    num = 1
    read(sac,*,iostat=io)num
    if(io /= 0 .or. len_trim(word) == 0)then
      call fatal('Cannot convert "'//sset(word)//'" to integer')
    elseif(num >= 0)then
      call fatal('"'//sset(word)//'" is not a negative integer')
    end if
  end function to_nint

  function is_pint(word) result(num)
    character(*), intent(in) :: word
    logical :: num
    integer :: io, numm
    character(len=(len_trim(adjustl(word)))) :: sac
    sac = trim(adjustl(word))
    num = .true.
    read(sac,'(I11)',iostat=io)numm
    if(io /= 0 .or. len_trim(word) == 0)then
      num = .false.
    elseif(numm <= 0)then
      num = .false.
    end if
  end function is_pint

!===============================================================================
! INT4_TO_STR converts an integer(4) to a string.
!===============================================================================

  pure function int4_to_str(num) result(str)

    integer, intent(in) :: num
    character(11) :: str

    write (str, '(I11)') num
    str = adjustl(str)

  end function int4_to_str

!===============================================================================
! INT8_TO_STR converts an integer(8) to a string.
!===============================================================================

  pure function int8_to_str(num) result(str)

    integer(8), intent(in) :: num
    character(21) :: str

    write (str, '(I21)') num
    str = adjustl(str)

  end function int8_to_str

!===============================================================================
! REAL_TO_STR converts a real(8) to a string based on how large the value is and
! how many significant digits are desired. By default, six significants digits
! are used.
!===============================================================================

  pure function real_to_str(num, sig_digits) result(string)

    real(8),           intent(in) :: num        ! number to convert
    integer, optional, intent(in) :: sig_digits ! # of significant digits
    character(15)                 :: string     ! string returned

    integer      :: decimal ! number of places after decimal
    integer      :: width   ! total field width
    real(8)      :: num2    ! absolute value of number
    character(9) :: fmt     ! format specifier for writing number

    ! set default field width
    width = 15

    ! set number of places after decimal
    if (present(sig_digits)) then
      decimal = sig_digits
    else
      decimal = 6
    end if

    ! Create format specifier for writing character
    num2 = abs(num)
    if (num2 == 0.0_8) then
      write(fmt, '("(F",I2,".",I2,")")') width, 1
    elseif (num2 < 1.0e-1_8) then
      write(fmt, '("(ES",I2,".",I2,")")') width, decimal - 1
    elseif (num2 >= 1.0e-1_8 .and. num2 < 1.0_8) then
      write(fmt, '("(F",I2,".",I2,")")') width, decimal
    elseif (num2 >= 1.0_8 .and. num2 < 10.0_8) then
      write(fmt, '("(F",I2,".",I2,")")') width, max(decimal-1, 0)
    elseif (num2 >= 10.0_8 .and. num2 < 100.0_8) then
      write(fmt, '("(F",I2,".",I2,")")') width, max(decimal-2, 0)
    elseif (num2 >= 100.0_8 .and. num2 < 1000.0_8) then
      write(fmt, '("(F",I2,".",I2,")")') width, max(decimal-3, 0)
    elseif (num2 >= 100.0_8 .and. num2 < 10000.0_8) then
      write(fmt, '("(F",I2,".",I2,")")') width, max(decimal-4, 0)
    elseif (num2 >= 10000.0_8 .and. num2 < 100000.0_8) then
      write(fmt, '("(F",I2,".",I2,")")') width, max(decimal-5, 0)
    else
      write(fmt, '("(ES",I2,".",I2,")")') width, decimal - 1
    end if

    ! Write string and left adjust
    write(string, fmt) num
    string = adjustl(string)

  end function real_to_str

  subroutine split(string, words, n)
    character(*), intent(in)  :: string
    character(*), intent(out) :: words(MAX_WORDS)
    integer,      intent(out) :: n
    character(1)  :: chr
    integer       :: i
    integer       :: i_start
    integer       :: i_end
    i_start = 0
    i_end = 0
    n = 0
    do i = 1, len_trim(string)
      chr = string(i:i)
      if ((i_start == 0) .and. (chr /= ' ') .and. (chr /= achar(9)) .and. &
          chr /= '=') then
        i_start = i
      end if
      if (i_start > 0) then
        if ((chr == ' ') .or. (chr == achar(9)) .or. chr == '=') i_end = i - 1
        if (i == len_trim(string))   i_end = i
        if (i_end > 0) then
          n = n + 1
          words(n) = string(i_start:i_end)
          i_start = 0
          i_end = 0
        end if
      end if
    end do
  end subroutine split

  pure function lower(word) result(word_lower)
    character(*), intent(in) :: word
    character(len=len(word)) :: word_lower
    integer :: i
    integer :: ic
    do i = 1, len(word)
      ic = ichar(word(i:i))
      if (ic >= 65 .and. ic <= 90) then
        word_lower(i:i) = char(ic+32)
      else
        word_lower(i:i) = word(i:i)
      end if
    end do
  end function lower

  pure function upper(word) result(word_upper)
    character(*), intent(in) :: word
    character(len=len(word)) :: word_upper
    integer :: i
    integer :: ic
    do i = 1, len(word)
      ic = ichar(word(i:i))
      if (ic >= 97 .and. ic <= 122) then
        word_upper(i:i) = char(ic-32)
      else
        word_upper(i:i) = word(i:i)
      end if
    end do
  end function upper

  function to_real(word) result(num)
    character(*), intent(in) :: word
    real(8) :: num
    integer :: io
    character(len=(len_trim(adjustl(word)))) :: sac
    sac = trim(adjustl(word))
    read(sac,*,iostat=io)num
    if(io /= 0 .or. len_trim(word) == 0) &
      call fatal('Cannot convert "'//sset(word)//'" to real number')
  end function

  function is_real(word) result(num)
    character(*) :: word
    logical :: num
    integer :: io
    real(8) :: realnum
    character(len=(len_trim(adjustl(word)))) :: sac
    sac = trim(adjustl(word))
    realnum = 0.0_8
    num = .true.
    read(sac,*,iostat=io)realnum
    if(io /=0 )num = .false.
  end function

end module

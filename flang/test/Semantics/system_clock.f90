! RUN: %flang_fc1 -fsyntax-only %s 2>&1
! RUN: not %flang_fc1 -fsyntax-only -std=f2023 %s 2>&1 | FileCheck --check-prefix=MISMATCH-ERROR,SIZE-ERROR %s
! RUN: not %flang_fc1 -fsyntax-only -std=f202Y %s 2>&1 | FileCheck --check-prefix=MISMATCH-ERROR,SIZE-ERROR %s
! RUN: %flang_fc1 -fsyntax-only -pedantic %s 2>&1 | FileCheck --check-prefix=MISMATCH-WARNING,SIZE-WARNING %s
! RUN: %flang_fc1 -fsyntax-only -Wsystem-clock-multiple-kinds %s 2>&1 | FileCheck --check-prefix=MISMATCH-WARNING %s
! RUN: %flang_fc1 -fsyntax-only -Wsystem-clock-min-size %s 2>&1 | FileCheck --check-prefix=SIZE-WARNING %s

! Tests for SYSTEM_CLOCK argument constraints

program test_system_clock
  implicit none

  ! Valid calls
  call test_valid()

  ! Error cases
  call test_kind_mismatch()
  call test_min_size()

contains

  subroutine test_valid()
    integer(4) :: count4, rate4, max4
    integer(8) :: count8, rate8, max8
    real(4) :: rate_real4
    real(8) :: rate_real8

    ! All valid
    call system_clock()
    call system_clock(count4)
    call system_clock(count4, rate4)
    call system_clock(count4, rate_real4)
    call system_clock(count4, rate4, max4)
    call system_clock(count8, rate8, max8)
    call system_clock(count=count4)
    call system_clock(count_rate=rate4)
    call system_clock(count_max=max4)
    call system_clock(count4, rate_real4, max4)
    call system_clock(count8, rate_real8, max8)
    call system_clock(count=count4, count_rate=rate4, count_max=max4)
  end subroutine

  subroutine test_kind_mismatch()
    integer(4) :: count4, rate4, max4
    integer(8) :: rate8, max8
    real(4) :: rate_real4

    !MISMATCH-ERROR: Integer arguments to SYSTEM_CLOCK must be have the same kind.
    !MISMATCH-WARNING: Integer arguments to SYSTEM_CLOCK should have the same kind.
    call system_clock(count4, rate8)

    !MISMATCH-ERROR: Integer arguments to SYSTEM_CLOCK must be have the same kind.
    !MISMATCH-WARNING: Integer arguments to SYSTEM_CLOCK should have the same kind.
    call system_clock(count4, rate8, max4)

    !MISMATCH-ERROR: Integer arguments to SYSTEM_CLOCK must be have the same kind.
    !MISMATCH-WARNING: Integer arguments to SYSTEM_CLOCK should have the same kind.
    call system_clock(count4, rate4, max8)

    !MISMATCH-ERROR: Integer arguments to SYSTEM_CLOCK must be have the same kind.
    !MISMATCH-WARNING: Integer arguments to SYSTEM_CLOCK should have the same kind.
    call system_clock(count4, rate_real4, max8)
  end subroutine

  subroutine test_min_size()
    integer(4) :: count4, rate4, max4
    real(4) :: rate_real4
    integer(2) :: count2, rate2, max2
    integer(1) :: count1, rate1, max1

    !SIZE-ERROR: Integer argument to SYSTEM_CLOCK must be an integer with kind >= 4.
    !SIZE-WARNING: Integer argument to SYSTEM_CLOCK should be an integer with kind >= 4.
    call system_clock(count2)

    !SIZE-ERROR: Integer argument to SYSTEM_CLOCK must be an integer with kind >= 4.
    !SIZE-WARNING: Integer argument to SYSTEM_CLOCK should be an integer with kind >= 4.
    call system_clock(count2, rate2)

    !SIZE-ERROR: Integer argument to SYSTEM_CLOCK must be an integer with kind >= 4.
    !SIZE-WARNING: Integer argument to SYSTEM_CLOCK should be an integer with kind >= 4.
    call system_clock(count2, rate_real4)

    !SIZE-ERROR: Integer argument to SYSTEM_CLOCK must be an integer with kind >= 4.
    !SIZE-WARNING: Integer argument to SYSTEM_CLOCK should be an integer with kind >= 4.
    call system_clock(count2, rate2, max2)

    !SIZE-ERROR: Integer argument to SYSTEM_CLOCK must be an integer with kind >= 4.
    !SIZE-WARNING: Integer argument to SYSTEM_CLOCK should be an integer with kind >= 4.
    call system_clock(count2, rate_real4, max2)

    !SIZE-ERROR: Integer argument to SYSTEM_CLOCK must be an integer with kind >= 4.
    !SIZE-WARNING: Integer argument to SYSTEM_CLOCK should be an integer with kind >= 4.
    call system_clock(count1)

    !SIZE-ERROR: Integer argument to SYSTEM_CLOCK must be an integer with kind >= 4.
    !SIZE-WARNING: Integer argument to SYSTEM_CLOCK should be an integer with kind >= 4.
    call system_clock(count1, rate1)

    !SIZE-ERROR: Integer argument to SYSTEM_CLOCK must be an integer with kind >= 4.
    !SIZE-WARNING: Integer argument to SYSTEM_CLOCK should be an integer with kind >= 4.
    call system_clock(count1, rate_real4)

    !SIZE-ERROR: Integer argument to SYSTEM_CLOCK must be an integer with kind >= 4.
    !SIZE-WARNING: Integer argument to SYSTEM_CLOCK should be an integer with kind >= 4.
    call system_clock(count1, rate1, max1)

    !SIZE-ERROR: Integer argument to SYSTEM_CLOCK must be an integer with kind >= 4.
    !SIZE-WARNING: Integer argument to SYSTEM_CLOCK should be an integer with kind >= 4.
    call system_clock(count1, rate_real4, max1)
  end subroutine

end program

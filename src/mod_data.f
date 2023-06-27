! a derived type for storing data.
module mod_data

  implicit none

  private
  public :: data_t

  ! data is stored in data_t
  type :: data_t
    integer :: id
  end type data_t

end module mod_data
! a generic linked list object
! adapted from https://fortranwiki.org/fortran/show/gen_list
! list packed from the end
module mod_list
  use mod_data
  implicit none
  private
  public :: list_t
  public :: list_init
  public :: list_insert
  public :: list_free
  public :: list_prev
  public :: list_next
  public :: list_delete
  public :: list_put
  public :: list_find
  ! linked list node data type
  type :: list_t
    type(data_t), pointer :: data => null()
    type(list_t), pointer :: prev => null()
    type(list_t), pointer :: next => null()
  end type list_t
  contains
    ! initialize the first node and store the provided data.
    function list_init(data) result(first)
      type(list_t), pointer :: first
      type(data_t), pointer :: data
      allocate(first)
      first%data => data
      nullify(first%prev)
      nullify(first%next)
    end function list_init
    ! insert and return a list node after last
    function list_insert(last, data) result(new_last)
      type(list_t), pointer :: last
      type(data_t), pointer :: data
      type(list_t), pointer :: new_last
      allocate(new_last)
      new_last%data => data
      new_last%prev => last
      last%next => new_last
    end function list_insert
    ! free the entire list and all data, beginning at first
    subroutine list_free(first)
      type(list_t), pointer:: first
      type(list_t), pointer :: current
      type(list_t), pointer :: next
      current => first
      do while (associated(current))
        next => current%next
        if (associated(current%data)) then
          deallocate(current%data)
          nullify(current%data)
        end if
        deallocate(current)
        nullify(current)
        current => next
      end do
    end subroutine list_free
    ! return the prev node before self
    function list_prev(self) result(prev)
      type(list_t), pointer :: self
      type(list_t), pointer :: prev
      prev => self%prev
    end function list_prev
    ! return the next node after self
    function list_next(self) result(next)
      type(list_t), pointer :: self
      type(list_t), pointer :: next
      next => self%next
    end function list_next
    ! delete self from the list, returning previous node
    ! if the first node is deleted this should return the node following (if it exists)
    ! if the last node is deleted this will return the new last (if it exists)
    ! if the only node is deleted this will return null
    function list_delete(self) result(node)
      type(list_t), pointer :: self
      type(list_t), pointer :: node
      if (associated(self)) then
        if (associated(self%data)) then
          deallocate(self%data)
          nullify(self%data)
        end if
        if (associated(self%prev)) then
          if (associated(self%next)) then
            self%prev%next => self%next
            self%next%prev => self%prev
          else
            nullify(self%prev%next)
          end if
          node => self%prev
        else
          if (associated(self%next)) then
            node => self%next
            nullify(self%next%prev)
          else
            node => null()
          end if
        end if
        deallocate(self)
        nullify(self)
      end if
    end function list_delete
    ! overwrite the data in list node self
    subroutine list_put(self, data)
      type(list_t), pointer :: self
      type(data_t), pointer :: data
      if (associated(self%data)) then
        deallocate(self%data)
        nullify(self%data)
      end if
      self%data => data
    end subroutine list_put
    ! find node using data%id
    function list_find(first, id) result(current)
      type(list_t), pointer :: first
      type(list_t), pointer :: current
      type(list_t), pointer :: next
      integer id
      current => first
      do while (associated(current))
        next => current%next
        if (current%data%id.eq.id) exit
        current => next
      end do
    end function list_find
end module mod_list
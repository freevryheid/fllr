program check
  use mod_list
  use mod_data
  implicit none
  type(list_t), pointer :: first   => null()
  type(list_t), pointer :: last    => null()
  type(list_t), pointer :: current => null()
  type(list_t), pointer :: prev    => null()
  type(list_t), pointer :: next    => null()
  type(data_t), pointer :: data
  integer i
  ! Initialize data objects
  allocate(data)
  data = data_t(id=1) ! instantiate a new data instance
  first => list_init(data)
  last => first
  if (.not.associated(first)) error stop "list_init failed"
  do i = 2, 100
    allocate(data)
    data = data_t(id=i) ! instantiate a new data instance
    last => list_insert(last, data)
  end do
  if (.not.associated(last)) &
    error stop "list_insert failed"
  current => list_find(first, 77)
  if (.not.associated(current)) &
    error stop "list_find failed"
  current => list_delete(current)
  if (current%data%id.ne.76) &
    error stop "list_delete failed - not returning previous node "
  if (current%next%data%id.ne.78) &
    error stop "list_delete failed - not returning previous node with correct next"
  if (current%prev%data%id.ne.75) &
    error stop "list_delete failed - not returning previous node with correct prev"
  current => list_find(first, 77)
  if (associated(current)) &
    error stop "list_delete failed"
  current => list_find(first, 78)
  prev => list_prev(current)
  if (prev%data%id.ne.76) &
    error stop "list_prev failed"
  next => list_next(current)
  if (next%data%id.ne.79) &
    error stop "list_next failed"
  first => list_delete(first)
  if (first%data%id.ne.2) &
    error stop "list_delete failed for first node"
  if (associated(first%prev)) &
    error stop "list_delete failed for first node - returned prev node still associated"
  if (.not.associated(first%next).or.first%next%data%id.ne.3) &
    error stop "list_delete failed for first node - next node not associated correctly"
  last => list_delete(last)
  if (last%data%id.ne.99) &
    error stop "list_delete failed for last node"
  if (associated(last%next)) &
    error stop "list_delete failed for last node - returned next node still associated"
  if (.not.associated(last%prev).or.last%prev%data%id.ne.98) &
    error stop "list_delete failed for last node - prev node not associated correctly"
  allocate(data)
  data = data_t(id=666) ! instantiate a new data instance
  call list_put(last, data)
  if (last%data%id.ne.666) &
    error stop "list_put failed"
  call list_free(first) ! free the list
  allocate(data)
  data = data_t(id=1) ! instantiate a new data instance
  first => list_init(data)
  first => list_delete(first)
  if (associated(first)) &
    error stop "list_delete failed for only node 1"
  call list_free(first)
end program check

! grassy@alien:~/f/fllr$ fpm test --runner='valgrind --leak-check=full'
! Project is up to date
! ==8739== Memcheck, a memory error detector
! ==8739== Copyright (C) 2002-2017, and GNU GPL'd, by Julian Seward et al.
! ==8739== Using Valgrind-3.18.1 and LibVEX; rerun with -h for copyright info
! ==8739== Command: build/gfortran_E167FD2A985B468F/test/check
! ==8739==
! ==8739==
! ==8739== HEAP SUMMARY:
! ==8739==     in use at exit: 0 bytes in 0 blocks
! ==8739==   total heap usage: 224 allocs, 224 frees, 18,840 bytes allocated
! ==8739==
! ==8739== All heap blocks were freed -- no leaks are possible
! ==8739==
! ==8739== For lists of detected and suppressed errors, rerun with: -s
! ==8739== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
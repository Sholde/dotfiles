;; mpi autocompletion

(eval-when-compile (require 'cl))

(require 'auto-complete)

(defvar ac-mpi-source-user-keywords* nil
  "A list of user keywords.")

(defvar ac-mpi-sources
  '(
    ac-mpi-source-basic-commands
    ))

(defun ac-mpi-setup ()
  (setq ac-sources (append ac-mpi-sources ac-sources)))

(defun ac-mpi-after-init-setup ()
  (setq ac-modes (append ac-modes '(mpi-mode)))
  (add-hook 'mpi-mode-hook 'ac-mpi-setup)
  (when (fboundp 'c-mode)
    (setq ac-modes (append ac-modes '(c-mode)))
    (add-hook 'c-mode-hook 'ac-mpi-setup))
  (when (fboundp 'c++-mode)
    (setq ac-modes (append ac-modes '(c++-mode)))
    (add-hook 'c++-mode-hook 'ac-mpi-setup)))

(add-hook 'after-init-hook 'ac-mpi-after-init-setup)

;;;; keywords

(defmacro ac-mpi-define-dictionary-source (name list)
  `(defconst ,name
     '((candidates . (lambda () (all-completions ac-prefix ,list)))
       )))

;; user keywords (command, option or varoable)
(ac-mpi-define-dictionary-source
 ac-mpi-source-user-keywords
 ac-mpi-source-user-keywords*)

;; mpi basic function
(ac-mpi-define-dictionary-source
 ac-mpi-source-basic-commands
 '(;;Environment Management Routines
   "MPI_Abort" "MPI_Errhandler_create*"  "MPI_Errhandler_free" "MPI_Errhandler_get*"
   "MPI_Errhandler_set*" "MPI_Error_class" "MPI_Error_string" "MPI_Finalize"
   "MPI_Get_processor_name"  "MPI_Get_version" "MPI_Init" "MPI_Initialized"
   "MPI_Wtick" "MPI_Wtime"
   ;;Point-to-Point Communication Routines
   "MPI_Bsend" "MPI_Bsend_init" "MPI_Buffer_attach" "MPI_Buffer_detach"
   "MPI_Cancel" "MPI_Get_count" "MPI_Get_elements" "MPI_Ibsend"
   "MPI_Iprobe" "MPI_Irecv" "MPI_Irsend" "MPI_Isend"
   "MPI_Issend" "MPI_Probe" "MPI_Recv" "MPI_Recv_init"
   "MPI_Request_free" "MPI_Rsend" "MPI_Rsend_init" "MPI_Send"
   "MPI_Send_init" "MPI_Sendrecv" "MPI_Sendrecv_replace" "MPI_Ssend"
   "MPI_Ssend_init" "MPI_Start" "MPI_Startall" "MPI_Test"
   "MPI_Test_cancelled" "MPI_Testall" "MPI_Testany" "MPI_Testsome"
   "MPI_Wait" "MPI_Waitall" "MPI_Waitany" "MPI_Waitsome"
   ;;CollectiveCommunicationRoutines
   "MPI_Allgather" "MPI_Allgatherv" "MPI_Allreduce" "MPI_Alltoall"
   "MPI_Alltoallv" "MPI_Barrier" "MPI_Bcast" "MPI_Gather"
   "MPI_Gatherv" "MPI_Op_create" "MPI_Op_free" "MPI_Reduce"
   "MPI_Reduce_scatter" "MPI_Scan" "MPI_Scatter" "MPI_Scatterv"
   ;;ProcessGroupRoutines
   "MPI_Group_compare" "MPI_Group_difference" "MPI_Group_excl" "MPI_Group_free"
   "MPI_Group_incl" "MPI_Group_intersection" "MPI_Group_range_excl" "MPI_Group_range_incl"
   "MPI_Group_rank" "MPI_Group_size" "MPI_Group_translate_ranks" "MPI_Group_union"
   ;;CommunicatorsRoutines
   "MPI_Comm_compare" "MPI_Comm_create" "MPI_Comm_dup" "MPI_Comm_free"
   "MPI_Comm_group" "MPI_Comm_rank" "MPI_Comm_remote_group" "MPI_Comm_remote_size"
   "MPI_Comm_size" "MPI_Comm_split" "MPI_Comm_test_inter" "MPI_Intercomm_create"
   "MPI_Intercomm_merge"
   ;;DerivedTypesRoutines
   "MPI_Type_commit" "MPI_Type_contiguous" "MPI_Type_extent*" "MPI_Type_free"
   "MPI_Type_hindexed*" "MPI_Type_hvector*" "MPI_Type_indexed" "MPI_Type_lb"
   "MPI_Type_size" "MPI_Type_struct*" "MPI_Type_ub*" "MPI_Type_vector"
   ;;VirtualTopologyRoutines
   "MPI_Cart_coords" "MPI_Cart_create" "MPI_Cart_get" "MPI_Cart_map"
   "MPI_Cart_rank" "MPI_Cart_shift" "MPI_Cart_sub" "MPI_Cartdim_get"
   "MPI_Dims_create" "MPI_Graph_create" "MPI_Graph_get" "MPI_Graph_map"
   "MPI_Graph_neighbors" "MPI_Graph_neighbors_count" "MPI_Graphdims_get" "MPI_Topo_test"
   ;;" "MiscellaneousRoutines
   "MPI_Address*" "MPI_Attr_delete*" "MPI_Attr_get*" "MPI_Attr_put*"
   "MPI_Keyval_create*" "MPI_Keyval_free*" "MPI_Pack" "MPI_Pack_size"
   "MPI_Pcontrol" "MPI_Unpack"
   ))


(provide'auto-complete-mpi)

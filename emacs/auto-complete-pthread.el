;; pthread autocompletion

(eval-when-compile (require 'cl))

(require 'auto-complete)

(defvar ac-pthread-source-user-keywords* nil
  "A list of user keywords.")

(defvar ac-pthread-sources
  '(
    ac-pthread-source-functions
    ac-pthread-source-keywords
    ))

(defun ac-pthread-setup ()
  (setq ac-sources (append ac-pthread-sources ac-sources)))

(defun ac-pthread-after-init-setup ()
  (setq ac-modes (append ac-modes '(pthread-mode)))
  (add-hook 'pthread-mode-hook 'ac-pthread-setup)
  (when (fboundp 'c-mode)
    (setq ac-modes (append ac-modes '(c-mode)))
    (add-hook 'c-mode-hook 'ac-pthread-setup))
  (when (fboundp 'c++-mode)
    (setq ac-modes (append ac-modes '(c++-mode)))
    (add-hook 'c++-mode-hook 'ac-pthread-setup)))

(add-hook 'after-init-hook 'ac-pthread-after-init-setup)

;;;; keywords

(defmacro ac-pthread-define-dictionary-source (name list)
  `(defconst ,name
     '((candidates . (lambda () (all-completions ac-prefix ,list)))
       )))

;; user keywords (command, option or variable)
(ac-pthread-define-dictionary-source
 ac-pthread-source-user-keywords
 ac-pthread-source-user-keywords*)

;; pthread basic function
(ac-pthread-define-dictionary-source
 ac-pthread-source-functions
 '(;;Brute force adding
   "pthread_create" "pthread_create" "pthread_join" "pthread_join" "pthread_detach"
   "pthread_detach" "pthread_key_create" "pthread_key_create" "pthread_key_delete"
   "pthread_key_delete" "pthread_setspecific" "pthread_setspecific"
   "pthread_getspecific" "pthread_getspecific" "pthread_self" "pthread_self"
   "pthread_equal" "pthread_equal" "pthread_once" "pthread_once" "sched_yield" "sched_yield"
   "pthread_setname_np" "pthread_setname_np" "pthread_getname_np" "pthread_getname_np"
   "pthread_attr_setname_np" "pthread_attr_setname_np" "pthread_attr_getname_np"
   "pthread_attr_getname_np" "pthread_setschedparam" "pthread_setschedparam"
   "pthread_getschedparam" "pthread_getschedparam" "pthread_setschedprio"
   "pthread_setschedprio" "pthread_kill" "pthread_kill" "pthread_sigmask"
   "pthread_sigmask" "pthread_atfork" "pthread_atfork" "pthread_exit" "pthread_exit"
   "pthread_cancel" "pthread_cancel" "pthread_setcancelstate" "pthread_setcancelstate"
   "pthread_testcancel" "pthread_setcanceltype" "pthread_testcancel"
   "pthread_testcancel" "pthread_cleanup_push" "pthread_cleanup_push"
   "pthread_cleanup_pop" "pthread_cleanup_pop"
   ))

;; pthread basic keywords
(ac-pthread-define-dictionary-source
 ac-pthread-source-keywords
 '(;;Structure
   "pthread_t" "pthread_attr_t" "pthread_mutex_t" "pthread_mutexattr_t"
   "pthread_cond_t" "pthread_condattr_t" "pthread_key_t" "pthread_rwlock_t"
   "pthread_barrier_t"
   ;;Static initializer
   "PTHREAD_MUTEX_INITIALIZER" "PTHREAD_COND_INITIALIZER"
   ))

;; Enable mpi-mode
(provide 'auto-complete-pthread)

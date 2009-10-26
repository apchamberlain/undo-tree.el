

(defvar buffer-undo-tree nil
  "Undo history tree in current buffer.")
(make-variable-buffer-local 'buffer-undo-tree)



(defstruct
  (undo-tree
   :named
   (:constructor nil)
   (:constructor make-undo-tree (&aux
				 (root (make-undo-tree-node nil nil))
				 (current root)))
   (:copier nil))
  root current)


(defstruct
  (undo-tree-node
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor make-undo-tree-node
		 (previous undo &aux (timestamp (current-time)) (branch 0)))
   (:constructor make-undo-tree-node-backwards
		 (next-node undo
		  &aux
		  (next (list next-node))
		  (timestamp (current-time))
		  (branch 0)))
   (:copier nil))
  previous next undo redo timestamp branch)


(defun undo-tree-grow (undo)
  "Add an UNDO node to current branch of `buffer-undo-tree'."
  (let* ((current (undo-tree-current buffer-undo-tree))
  	 (new (make-undo-tree-node current undo)))
    (push new (undo-tree-node-next current))
    (setf (undo-tree-current buffer-undo-tree) new)))


(defun undo-tree-grow-backwards (node undo)
  "Add an UNDO node *above* undo-tree NODE, and return new node.
Note that this will overwrite NODE's \"previous\" link, so should
only be used on detached nodes, never on nodes that are already
part of `buffer-undo-tree'."
  (let* ((new (make-undo-tree-node-backwards node undo)))
    (setf (undo-tree-node-previous node) new)
    new))


(defun undo-list-pop-changeset ()
  "Pop changeset from `buffer-undo-list'."
  ;; discard undo boundaries at head of list
  (while (null (car buffer-undo-list))
    (setq buffer-undo-list (cdr buffer-undo-list)))
  ;; pop elements up to next undo boundary
  (let* ((changeset (cons (pop buffer-undo-list) nil))
	 (p changeset))
    (while (car buffer-undo-list)
      (setcdr p (cons (pop buffer-undo-list) nil))
      (setq p (cdr p)))
    changeset))


(defun undo-list-transfer-to-tree ()
  "Transfer entries accumulated in `buffer-undo-list'
to `buffer-undo-tree'."
  (when buffer-undo-list
    (let* ((node (make-undo-tree-node nil (undo-list-pop-changeset)))
	   (splice (undo-tree-current buffer-undo-tree)))
      (setf (undo-tree-current buffer-undo-tree) node)
      (while buffer-undo-list
	(setq node (undo-tree-grow-backwards node (undo-list-pop-changeset))))
      (setf (undo-tree-node-previous node) splice)
      (push node (undo-tree-node-next splice))
      (setf (undo-tree-node-branch splice) 0))))


(defmacro undo-tree-num-branches ()
  "Return number of branches at current undo tree node."
  '(length (undo-tree-node-next (undo-tree-current buffer-undo-tree))))


(defun undo-tree-undo (&optional arg)
  "Undo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  ;; if `buffer-undo-tree' is empty, create initial undo-tree
  (when (null buffer-undo-tree)
    (setq buffer-undo-tree (make-undo-tree)))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)

  (dotimes (i arg)
    ;; check if at top of undo tree
    (if (null (undo-tree-node-previous (undo-tree-current buffer-undo-tree)))
	(error "No further undo information")
      ;; undo one record from undo tree
      (primitive-undo 1 (undo-copy-list
			 (undo-tree-node-undo
			  (undo-tree-current buffer-undo-tree))))
      ;; pop redo entries that `primitive-undo' has added to
      ;; `buffer-undo-list' and record them in current node's redo record
      (setf (undo-tree-node-redo (undo-tree-current buffer-undo-tree))
	    (undo-list-pop-changeset))
      ;; rewind current node
      (setf (undo-tree-current buffer-undo-tree)
	    (undo-tree-node-previous (undo-tree-current buffer-undo-tree)))
      ))
  ;; inform user if at branch point
  (when (> (undo-tree-num-branches) 1)
    (message "Undo branch point!")))


(defun undo-tree-redo (&optional arg)
  "Redo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  ;; if `buffer-undo-tree' is empty, create initial undo-tree
  (when (null buffer-undo-tree)
    (setq buffer-undo-tree (make-undo-tree)))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)

  (let ((current (undo-tree-current buffer-undo-tree)))
    (dotimes (i arg)
      ;; check if at bottom of undo tree
      (if (null (undo-tree-node-next (undo-tree-current buffer-undo-tree)))
	  (error "No further redo information")
	;; advance current node
	(setq current
	      (setf (undo-tree-current buffer-undo-tree)
		    (nth (undo-tree-node-branch current)
			 (undo-tree-node-next current))))
	;; redo one record from undo tree
	(primitive-undo 1 (undo-copy-list (undo-tree-node-redo current)))
	;; discard undo entries that `primitive-undo' has added to
	;; `buffer-undo-list' since we already know how to undo from here
	;; (note: could overwrite old undo entry instead for safety's sake?)
	(setq buffer-undo-list nil)
	)))
  ;; inform user if at branch point
  (when (> (undo-tree-num-branches) 1)
    (message "Undo branch point!")))


(defun undo-tree-switch-branch (branch)
  "Switch to a different BRANCH of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo'."
  (interactive (list (or (and prefix-arg (prefix-numeric-value prefix-arg))
			 (read-number
			  (format "Branch (0-%d): "
				  (1- (undo-tree-num-branches))))
			 )))
  ;; sanity check branch number
  (if (or (< branch 0) (> branch (1- (undo-tree-num-branches))))
      (error "Invalid branch number")
    ;; switch branch
    (setf (undo-tree-node-branch (undo-tree-current buffer-undo-tree))
	  branch)))

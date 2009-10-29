



;;; =====================================================================
;;;              Global variables and customization options

(defvar buffer-undo-tree nil
  "Tree of undo entries in current buffer.")
(make-variable-buffer-local 'buffer-undo-tree)


(defgroup undo-tree nil
  "Tree undo/redo."
  :group 'undo)


(defcustom undo-tree-visualizer-spacing 3
  "Horizontal spacing in undo-tree visualization.
Must be an odd integer."
  :group 'undo-tree
  :type '(integer
 	  :match (lambda (w n) (and (integerp n) (= (mod n 2) 1)))))




;;; =====================================================================
;;;                     Undo-tree data structure

(defstruct
  (undo-tree
   :named
   (:constructor nil)
   (:constructor make-undo-tree
		 (&aux
		  (root (make-undo-tree-node nil nil))
		  (current root)))
   (:copier nil))
  root current)


(defstruct
  (undo-tree-node
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor make-undo-tree-node
		 (previous undo
		  &aux
		  (timestamp (current-time))
		  (branch 0)))
   (:constructor make-undo-tree-node-backwards
		 (next-node undo
		  &aux
		  (next (list next-node))
		  (timestamp (current-time))
		  (branch 0)))
   (:copier nil))
  previous next undo redo timestamp branch visualizer)


(defstruct
  (undo-tree-visualizer-data
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor make-undo-tree-visualizer-data)
   (:copier nil))
  lwidth cwidth rwidth marker)


(defmacro undo-tree-visualizer-data-p (v)
  (let ((len (length (make-undo-tree-visualizer-data))))
    `(and (vectorp ,v) (= (length ,v) ,len))))

(defmacro undo-tree-node-lwidth (node)
  `(when (vectorp (undo-tree-node-visualizer ,node))
     (undo-tree-visualizer-data-lwidth
      (undo-tree-node-visualizer ,node))))

(defmacro undo-tree-node-cwidth (node)
  `(when (vectorp (undo-tree-node-visualizer ,node))
     (undo-tree-visualizer-data-cwidth
      (undo-tree-node-visualizer ,node))))

(defmacro undo-tree-node-rwidth (node)
  `(when (vectorp (undo-tree-node-visualizer ,node))
     (undo-tree-visualizer-data-rwidth
      (undo-tree-node-visualizer ,node))))

(defmacro undo-tree-node-marker (node)
  `(when (vectorp (undo-tree-node-visualizer ,node))
     (undo-tree-visualizer-data-marker
      (undo-tree-node-visualizer ,node))))


(defsetf undo-tree-node-lwidth (node) (val)
  `(let ((v (undo-tree-node-visualizer ,node)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-visualizer ,node)
	     (setq v (make-undo-tree-visualizer-data))))
     (setf (undo-tree-visualizer-data-lwidth v) ,val)))

(defsetf undo-tree-node-cwidth (node) (val)
  `(let ((v (undo-tree-node-visualizer ,node)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-visualizer ,node)
	     (setq v (make-undo-tree-visualizer-data))))
     (setf (undo-tree-visualizer-data-cwidth v) ,val)))

(defsetf undo-tree-node-rwidth (node) (val)
  `(let ((v (undo-tree-node-visualizer ,node)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-visualizer ,node)
	     (setq v (make-undo-tree-visualizer-data))))
     (setf (undo-tree-visualizer-data-rwidth v) ,val)))

(defsetf undo-tree-node-marker (node) (val)
  `(let ((v (undo-tree-node-visualizer ,node)))
     (unless (undo-tree-visualizer-data-p v)
       (setf (undo-tree-node-visualizer ,node)
	     (setq v (make-undo-tree-visualizer-data))))
     (setf (undo-tree-visualizer-data-marker v) ,val)))



;;; =====================================================================
;;;              Basic undo-tree data structure functions

(defun undo-tree-grow (undo)
  "Add an UNDO node to current branch of `buffer-undo-tree'."
  (let* ((current (undo-tree-current buffer-undo-tree))
  	 (new (make-undo-tree-node current undo)))
    (push new (undo-tree-node-next current))
    (setf (undo-tree-current buffer-undo-tree) new)))


(defun undo-tree-grow-backwards (node undo)
  "Add an UNDO node *above* undo-tree NODE, and return new node.
Note that this will overwrite NODE's \"previous\" link, so should
only be used on a detached NODE, never on nodes that are already
part of `buffer-undo-tree'."
  (let ((new (make-undo-tree-node-backwards node undo)))
    (setf (undo-tree-node-previous node) new)
    new))


(defun undo-tree-compute-widths (undo-tree)
  "Recursively compute widths for all UNDO-TREE's nodes."
  (undo-tree-node-compute-widths (undo-tree-root undo-tree)))


(defun undo-tree-node-compute-widths (node)
  "Compute NODE's left- and right-subtree widths."
  (let ((num-children (length (undo-tree-node-next node)))
	(lwidth 0) (cwidth 0) (rwidth 0)
	p w)
    (cond
     ;; leaf nodes have 0 width
     ((= 0 num-children)
      (setf cwidth 1
	    (undo-tree-node-lwidth node) 0
	    (undo-tree-node-cwidth node) 1
	    (undo-tree-node-rwidth node) 0))

     ;; odd number of children
     ((= (mod num-children 2) 1)
      (setq p (undo-tree-node-next node))
      ;; compute left-width
      (dotimes (i (/ num-children 2))
	(setq w (undo-tree-node-compute-widths (car p)))
	(setq lwidth (+ lwidth (aref w 0) (aref w 1) (aref w 2)))
	(setq p (cdr p)))
      (setq w (undo-tree-node-compute-widths (car p))
	    lwidth (+ lwidth (aref w 0)))
      (setf (undo-tree-node-lwidth node) lwidth)
      ;; centre-width is inherited from middle child
      (setf cwidth (undo-tree-node-cwidth (car p))
	    (undo-tree-node-cwidth node) cwidth)
      ;; compute right-width
      (setq rwidth (+ rwidth (aref w 2)))
      (setq p (cdr p))
      (dotimes (i (/ num-children 2))
	(setq w (undo-tree-node-compute-widths (car p)))
	(setq rwidth (+ rwidth (aref w 0) (aref w 1) (aref w 2)))
	(setq p (cdr p)))
      (setf (undo-tree-node-rwidth node) rwidth))

     ;; even number of children
     (t
      (setq p (undo-tree-node-next node))
      ;; compute left-width
      (dotimes (i (/ num-children 2))
	(setq w (undo-tree-node-compute-widths (car p)))
	(setq lwidth (+ lwidth (aref w 0) (aref w 1) (aref w 2)))
	(setq p (cdr p)))
      (setf (undo-tree-node-lwidth node) lwidth)
      ;; compute right-width
      (dotimes (i (/ num-children 2))
	(setq w (undo-tree-node-compute-widths (car p)))
	(setq rwidth (+ rwidth (aref w 0) (aref w 1) (aref w 2)))
	(setq p (cdr p)))
      (setf (undo-tree-node-rwidth node) rwidth)
      ;; centre-width is 0 when number of children is even
      (setf cwidth 0
	    (undo-tree-node-cwidth node) 0)))

    ;; return left-, centre- and right-widths
    (vector lwidth cwidth rwidth)))



(defun undo-tree-clear-visualizer (undo-tree)
  ;; Clear visualizer data from UNDO-TREE.
  (undo-tree-node-clear-visualizer (undo-tree-root undo-tree)))


(defun undo-tree-node-clear-visualizer (node)
  ;; Recursively clear visualizer data from NODE and descendents.
  (setf (undo-tree-node-visualizer node) nil)
  (dolist (n (undo-tree-node-next node))
    (undo-tree-node-clear-visualizer n)))




;;; =====================================================================
;;;                        Undo/redo commands

(defmacro undo-tree-num-branches ()
  ;; Return number of branches at current undo tree node.
  '(length (undo-tree-node-next (undo-tree-current buffer-undo-tree))))


(defun undo-list-pop-changeset ()
  ;; Pop changeset from `buffer-undo-list'.
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
  ;; Transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'.
  (when buffer-undo-list
    ;; create new node from first changeset in `buffer-undo-list', save old
    ;; `buffer-undo-tree' current node, and make new node the current node
    (let* ((node (make-undo-tree-node nil (undo-list-pop-changeset)))
	   (splice (undo-tree-current buffer-undo-tree)))
      (setf (undo-tree-current buffer-undo-tree) node)
      ;; grow tree fragment backwards from new node using `buffer-undo-list'
      ;; changesets
      (while buffer-undo-list
	(setq node (undo-tree-grow-backwards node (undo-list-pop-changeset))))
      ;; splice tree fragment onto end of old `buffer-undo-tree' current node
      (setf (undo-tree-node-previous node) splice)
      (push node (undo-tree-node-next splice))
      (setf (undo-tree-node-branch splice) 0))))



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




;;; =====================================================================
;;;                     Undo-tree Visualization

(defun undo-tree-visualize ()
  "Visualize the current buffer's undo tree."
  (interactive)
  ;; if `buffer-undo-tree' is empty, create initial undo-tree
  (when (null buffer-undo-tree)
    (setq buffer-undo-tree (make-undo-tree)))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)
  ;; prepare *undo-tree* buffer, then draw tree in it
  (let ((undo-tree buffer-undo-tree))
    (switch-to-buffer-other-window " *undo-tree*")
    (setq cursor-type nil)
    (erase-buffer)
    (undo-tree-move-down 1)  ; top margin
    (undo-tree-compute-widths undo-tree)
    (undo-tree-move-forward
     (+ (undo-tree-node-char-lwidth (undo-tree-root undo-tree))
	2))  ; left margin
    (undo-tree-draw-subtree (undo-tree-root undo-tree))
    (goto-char (undo-tree-node-marker (undo-tree-current undo-tree)))
    (put-text-property (point) (1+ (point)) 'face '(foreground-color . "red"))
    ))



(defun undo-tree-draw-subtree (node)
  ;; Draw subtree rooted at node. The subtree will start from the point.
  (let ((num-children (length (undo-tree-node-next node)))
	pos l p)
    ;; draw node itself
    (undo-tree-insert ?o)
    (backward-char 1)
    (move-marker (setf (undo-tree-node-marker node) (make-marker)) (point))

    (cond
     ;; if we're at a leaf node, we're done
     ((= num-children 0))

     ;; if node has only one child, draw it (not strictly necessary to deal
     ;; with this case separately, but as it's by far the most common case
     ;; this makes the code clearer and more efficient)
     ((= num-children 1)
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (backward-char 1)
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (backward-char 1)
      (undo-tree-move-down 1)
      (undo-tree-draw-subtree (car (undo-tree-node-next node))))

     ;; if node had multiple children, draw branches
     (t
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (backward-char 1)
      ;; horizontal part of left branch
      (setq l (- (undo-tree-node-char-lwidth node)
		 (undo-tree-node-char-lwidth
		  (car (undo-tree-node-next node)))))
      (backward-char l)
      (setq pos (point))
      (unless (= num-children 2)
	(undo-tree-move-forward 2)
	(undo-tree-insert ?_ (- l 2)))
      ;; left subtrees
      (goto-char pos)
      (setq p (cons nil (undo-tree-node-next node)))
      (dotimes (i (/ num-children 2))
	(setq p (cdr p))
	(undo-tree-move-forward 1)
	(undo-tree-move-down 1)
	(undo-tree-insert ?/)
	(backward-char 2)
	(undo-tree-move-down 1)
	(undo-tree-draw-subtree (car p))
	(goto-char pos)
	(undo-tree-move-forward
	 (+ (undo-tree-node-char-rwidth (car p))
	    (undo-tree-node-char-lwidth (cadr p))
	    undo-tree-visualizer-spacing 1))
	(setq pos (point)))
      ;; middle subtree (only when number of children is odd)
      (when (= (mod num-children 2) 1)
	(setq p (cdr p))
	(undo-tree-move-down 1)
	(undo-tree-insert ?|)
	(backward-char 1)
	(undo-tree-move-down 1)
	(undo-tree-draw-subtree (car p))
	(goto-char pos)
	(undo-tree-move-forward
	 (+ (undo-tree-node-char-rwidth (car p))
	    (if (cadr p) (undo-tree-node-char-lwidth (cadr p)) 0)
	    undo-tree-visualizer-spacing 1))
	(setq pos (point)))
      ;; right subtrees
      (dotimes (i (/ num-children 2))
	(setq p (cdr p))
	(backward-char 1)
	(undo-tree-move-down 1)
	(undo-tree-insert ?\\)
	(undo-tree-move-down 1)
	(undo-tree-draw-subtree (car p))
	(goto-char pos)
	(undo-tree-move-forward
	 (+ (undo-tree-node-char-rwidth (car p))
	    (if (cadr p) (undo-tree-node-char-lwidth (cadr p)) 0)
	    undo-tree-visualizer-spacing 1))
	(setq pos (point)))
      ;; horizontal part of right branch
      (unless (= num-children 2)
	(backward-char undo-tree-visualizer-spacing)
	(setq l (undo-tree-node-char-rwidth node))
	(backward-char l)
	(undo-tree-insert ?_ (- l (undo-tree-node-char-rwidth (car p)) 2))))
     )))



(defun undo-tree-node-char-lwidth (node)
  ;; Return left-width of NODE measured in characters.
  (if (= (length (undo-tree-node-next node)) 0) 0
    (- (* (+ undo-tree-visualizer-spacing 1) (undo-tree-node-lwidth node))
       (if (= (undo-tree-node-cwidth node) 0)
	   (1+ (/ undo-tree-visualizer-spacing 2)) 0))))


(defun undo-tree-node-char-rwidth (node)
  ;; Return right-width of NODE measured in characters.
  (if (= (length (undo-tree-node-next node)) 0) 0
    (- (* (+ undo-tree-visualizer-spacing 1) (undo-tree-node-rwidth node))
       (if (= (undo-tree-node-cwidth node) 0)
	   (1+ (/ undo-tree-visualizer-spacing 2)) 0))))


(defun undo-tree-insert (char &optional arg)
  ;; Insert character CHAR ARG times, overwriting.
  (unless arg (setq arg 1))
  (insert (make-string arg char))
  (undo-tree-move-forward arg)
  (backward-delete-char arg))


(defun undo-tree-move-down (&optional arg)
  ;; Move down, extending buffer if necessary.
  (let ((col (current-column))
	(next-line-add-newlines t))
    (unless arg (setq arg 1))
    (with-no-warnings (next-line arg))
    (unless (= (current-column) col)
      (insert (make-string (- col (current-column)) ? )))))


(defun undo-tree-move-forward (&optional arg)
  ;; Move forward, extending buffer if necessary.
  (unless arg (setq arg 1))
  (let ((n (- (line-end-position) (point))))
    (if (> n arg)
	(forward-char arg)
      (end-of-line)
      (insert (make-string (- arg n) ? )))))

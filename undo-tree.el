



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


(defvar undo-tree-map nil
  "Keymap used in undo-tree-mode.")



(defface undo-tree-visualizer-default-face
  '((((class color)) :foreground "gray"))
  "*Face used to draw undo-tree in visualizer.")


(defface undo-tree-visualizer-current-face
  '((((class color)) :foreground "red"))
  "*Face used to highlight current undo-tree node
in visualizer.")


(defface undo-tree-visualizer-active-branch-face
  '((((class color)) :foreground "white" :weight bold))
  "*Face used to highlight active undo-tree branch
in visualizer.")


(defvar undo-tree-visualizer-buffer nil
  "Parent buffer in visualizer.")
(make-variable-buffer-local 'undo-tree-visualizer-buffer)


(defvar undo-tree-visualizer-map nil
  "Keymap used in undo-tree visualizer.")



;;; =================================================================
;;;                     Setup default keymaps

(unless undo-tree-map
  (setq undo-tree-map (make-sparse-keymap))
  ;; remap `undo' to `undo-tree-undo'
  (define-key undo-tree-map [remap undo] 'undo-tree-undo)
  ;; redo doesn't exist normally, so define out own keybindings
  (define-key undo-tree-map (kbd "C-?") 'undo-tree-redo)
  (define-key undo-tree-map (kbd "C-+") 'undo-tree-redo)
  ;; just in case something has defined it...
  (define-key undo-tree-map [remap redo] 'undo-tree-redo)
  ;; we use "C-x u" for the undo-tree visualizer
  (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize))


(unless undo-tree-visualizer-map
  (setq undo-tree-visualizer-map (make-keymap))
  ;; vertical motion keys undo/redo
  (define-key undo-tree-visualizer-map [up]
    'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-map "p"
    'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-map "\C-p"
    'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-map [down]
    'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-map "n"
    'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-map "\C-n"
    'undo-tree-visualize-redo)
  ;; horizontal motion keys switch branch
  (define-key undo-tree-visualizer-map [right]
    'undo-tree-visualize-switch-next-branch)
  (define-key undo-tree-visualizer-map "f"
    'undo-tree-visualize-switch-next-branch)
  (define-key undo-tree-visualizer-map "\C-f"
    'undo-tree-visualize-undo)
  (define-key undo-tree-visualizer-map [left]
    'undo-tree-visualize-switch-previous-branch)
  (define-key undo-tree-visualizer-map "b"
    'undo-tree-visualize-switch-previous-branch)
  (define-key undo-tree-visualizer-map "\C-b"
    'undo-tree-visualize-switch-previous-branch)
  ;; mouse sets buffer state to node at click
  (define-key undo-tree-visualizer-map [mouse-1]
    'undo-tree-visualizer-set)
  ;; quit visualizer
  (define-key undo-tree-visualizer-map "q"
    'undo-tree-visualizer-quit)
  (define-key undo-tree-visualizer-map "\C-q"
    'undo-tree-visualizer-quit))




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



(defun undo-tree-clear-visualizer-data (undo-tree)
  ;; Clear visualizer data from UNDO-TREE.
  (undo-tree-node-clear-visualizer-data (undo-tree-root undo-tree)))


(defun undo-tree-node-clear-visualizer-data (node)
  ;; Recursively clear visualizer data from NODE and descendents.
  (setf (undo-tree-node-visualizer node) nil)
  (dolist (n (undo-tree-node-next node))
    (undo-tree-node-clear-visualizer-data n)))



(defun undo-tree-position (node list)
  "Find the first occurrence of NODE in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with 'eq."
  (let ((i 0))
    (catch 'found
      (while (progn
	       (when (eq node (car list)) (throw 'found i))
	       (incf i)
	       (setq list (cdr list))))
      nil)))


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




;;; =====================================================================
;;;                        Undo-tree commands

(define-minor-mode undo-tree-mode
  "Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree."
  nil             ; init value
  ""              ; lighter
  undo-tree-map)  ; keymap


(defun turn-on-undo-tree-mode ()
  "Enable undo-tree-mode."
  (undo-tree-mode 1))


(define-globalized-minor-mode global-undo-tree-mode
  undo-tree-mode turn-on-undo-tree-mode)



(defun undo-tree-undo (&optional arg)
  "Undo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  ;; if `buffer-undo-tree' is empty, create initial undo-tree
  (when (null buffer-undo-tree)
    (setq buffer-undo-tree (make-undo-tree)))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)

  (dotimes (i (or arg 1))
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
    (dotimes (i (or arg 1))
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
				  (1- (undo-tree-num-branches)))))))
  ;; sanity check branch number
  (if (or (< branch 0) (> branch (1- (undo-tree-num-branches))))
      (error "Invalid branch number")
    ;; switch branch
    (setf (undo-tree-node-branch (undo-tree-current buffer-undo-tree))
	  branch)))


(defun undo-tree-set (node)
  ;; Set buffer to state corresponding to NODE. Returns intersection point
  ;; between path back from current node and path back from selected NODE.
  (let ((path (make-hash-table :test 'eq))
	(n node))
    (puthash (undo-tree-root buffer-undo-tree) t path)
    ;; build list of nodes leading back from selected node to root, updating
    ;; branches as we go to point down to selected node
    (while (progn
	     (puthash n t path)
	     (when (undo-tree-node-previous n)
	       (setf (undo-tree-node-branch (undo-tree-node-previous n))
		     (undo-tree-position
		      n (undo-tree-node-next (undo-tree-node-previous n))))
	       (setq n (undo-tree-node-previous n)))))
    ;; work backwards from current node until we intersect path back from
    ;; selected node
    (setq n (undo-tree-current buffer-undo-tree))
    (while (not (gethash n path))
      (setq n (undo-tree-node-previous n)))
    ;; ascend tree until intersection node
    (while (not (eq (undo-tree-current buffer-undo-tree) n))
      (undo-tree-undo))
    ;; descend tree until selected node
    (while (not (eq (undo-tree-current buffer-undo-tree) node))
      (undo-tree-redo))
    n))  ; return intersection node




;;; =====================================================================
;;;                       Undo-tree visualizer

(defun undo-tree-visualize ()
  "Visualize the current buffer's undo tree."
  (interactive)
  ;; if `buffer-undo-tree' is empty, create initial undo-tree
  (when (null buffer-undo-tree)
    (setq buffer-undo-tree (make-undo-tree)))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
  (undo-list-transfer-to-tree)
  ;; prepare *undo-tree* buffer, then draw tree in it
  (let ((undo-tree buffer-undo-tree)
	(buff (current-buffer)))
    (switch-to-buffer-other-window " *undo-tree*")
    (undo-tree-visualizer-mode)
    (setq undo-tree-visualizer-buffer buff)
    (setq buffer-undo-tree undo-tree)
    (setq cursor-type nil)
    (undo-tree-draw-tree undo-tree)))


(defun undo-tree-draw-tree (undo-tree)
  ;; Draw undo tree in current buffer.
  (erase-buffer)
  (undo-tree-move-down 1)  ; top margin
  (undo-tree-compute-widths undo-tree)
  (undo-tree-move-forward
   (max (/ (window-width) 2)
	(+ (undo-tree-node-char-lwidth (undo-tree-root undo-tree))
	   2)))  ; left margin
  ;; draw undo-tree
  (let ((undo-tree-insert-face 'undo-tree-visualizer-default-face))
    (save-excursion (undo-tree-draw-subtree (undo-tree-root undo-tree))))
  ;; highlight active branch
  (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face))
    (undo-tree-draw-subtree (undo-tree-root undo-tree) 'active))
  ;; highlight current node
  (goto-char (undo-tree-node-marker (undo-tree-current undo-tree)))
  (put-text-property (point) (1+ (point))
		     'face 'undo-tree-visualizer-current-face))


(defun undo-tree-draw-subtree (node &optional active-branch)
  ;; Draw subtree rooted at NODE. The subtree will start from point.
  ;; If ACTIVE-BRANCH is positive, just draw active branch below NODE.
  (let ((num-children (length (undo-tree-node-next node)))
	pos trunk-pos n)

    ;; draw node itself, and link it to node in tree
    (undo-tree-insert ?o)
    (backward-char 1)
    (unless (markerp (undo-tree-node-marker node))
      (setf (undo-tree-node-marker node) (make-marker)))
    (move-marker (undo-tree-node-marker node) (point))
    (put-text-property (point) (1+ (point)) 'undo-tree-node node)

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
      (undo-tree-draw-subtree (car (undo-tree-node-next node)) active-branch))

     ;; if node had multiple children, draw branches
     (t
      (undo-tree-move-down 1)
      (undo-tree-insert ?|)
      (backward-char 1)
      (setq trunk-pos (point))
      ;; left subtrees
      (backward-char
       (- (undo-tree-node-char-lwidth node)
	  (undo-tree-node-char-lwidth
	   (car (undo-tree-node-next node)))))
      (setq pos (point))
      (setq n (cons nil (undo-tree-node-next node)))
      (dotimes (i (/ num-children 2))
	(setq n (cdr n))
	(when (or (null active-branch)
		  (eq (car n)
		      (nth (undo-tree-node-branch node)
			   (undo-tree-node-next node))))
	  (undo-tree-move-forward 2)
	  (undo-tree-insert ?_ (- trunk-pos pos 2))
	  (goto-char pos)
	  (undo-tree-move-forward 1)
	  (undo-tree-move-down 1)
	  (undo-tree-insert ?/)
	  (backward-char 2)
	  (undo-tree-move-down 1)
	  (undo-tree-draw-subtree (car n) active-branch))
	(goto-char pos)
	(undo-tree-move-forward
	 (+ (undo-tree-node-char-rwidth (car n))
	    (undo-tree-node-char-lwidth (cadr n))
	    undo-tree-visualizer-spacing 1))
	(setq pos (point)))
      ;; middle subtree (only when number of children is odd)
      (when (= (mod num-children 2) 1)
	(setq n (cdr n))
	(when (or (null active-branch)
		  (eq (car n)
		      (nth (undo-tree-node-branch node)
			   (undo-tree-node-next node))))
	  (undo-tree-move-down 1)
	  (undo-tree-insert ?|)
	  (backward-char 1)
	  (undo-tree-move-down 1)
	  (undo-tree-draw-subtree (car n) active-branch))
	(goto-char pos)
	(undo-tree-move-forward
	 (+ (undo-tree-node-char-rwidth (car n))
	    (if (cadr n) (undo-tree-node-char-lwidth (cadr n)) 0)
	    undo-tree-visualizer-spacing 1))
	(setq pos (point)))
      ;; right subtrees
      (incf trunk-pos)
      (dotimes (i (/ num-children 2))
	(setq n (cdr n))
	(when (or (null active-branch)
		  (eq (car n)
		      (nth (undo-tree-node-branch node)
			   (undo-tree-node-next node))))
	  (goto-char trunk-pos)
	  (undo-tree-insert ?_ (- pos trunk-pos 1))
	  (goto-char pos)
	  (backward-char 1)
	  (undo-tree-move-down 1)
	  (undo-tree-insert ?\\)
	  (undo-tree-move-down 1)
	  (undo-tree-draw-subtree (car n) active-branch))
	(goto-char pos)
	(undo-tree-move-forward
	 (+ (undo-tree-node-char-rwidth (car n))
	    (if (cadr n) (undo-tree-node-char-lwidth (cadr n)) 0)
	    undo-tree-visualizer-spacing 1))
	(setq pos (point)))
      ))))



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
  (backward-delete-char arg)
  (when (boundp 'undo-tree-insert-face)
    (put-text-property (- (point) arg) (point)
		       'face undo-tree-insert-face)))


(defun undo-tree-move-down (&optional arg)
  ;; Move down, extending buffer if necessary.
  (let ((row (line-number-at-pos))
	(col (current-column))
	line)
    (unless arg (setq arg 1))
    (forward-line arg)
    (setq line (line-number-at-pos))
    ;; if buffer doesn't have enough lines, add some
    (when (/= line (+ row arg))
      (insert (make-string (- arg (- line row)) ?\n)))
    (undo-tree-move-forward col)))


(defun undo-tree-move-forward (&optional arg)
  ;; Move forward, extending buffer if necessary.
  (unless arg (setq arg 1))
  (let ((n (- (line-end-position) (point))))
    (if (> n arg)
	(forward-char arg)
      (end-of-line)
      (insert (make-string (- arg n) ? )))))



;;; =====================================================================
;;;                    Visualizer mode commands

(defun undo-tree-visualizer-mode ()
  "Major mode used in undo-tree visualizer."
  (kill-all-local-variables)
  (setq major-mode 'undo-tree-visualizer-mode)
  (setq mode-name "undo-tree-visualizer-mode")
  (use-local-map undo-tree-visualizer-map))


(defun undo-tree-visualize-undo (&optional arg)
  "Undo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
  (put-text-property (point) (1+ (point)) 'face 'default)
  (switch-to-buffer-other-window undo-tree-visualizer-buffer)
  (unwind-protect
      (undo-tree-undo arg)
    (switch-to-buffer-other-window " *undo-tree*")
    (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
    (put-text-property (point) (1+ (point))
		       'face 'undo-tree-visualizer-current-face)))


(defun undo-tree-visualize-redo (&optional arg)
  "Redo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
  (put-text-property (point) (1+ (point)) 'face 'default)
  (switch-to-buffer-other-window undo-tree-visualizer-buffer)
  (unwind-protect
      (undo-tree-redo arg)
    (switch-to-buffer-other-window " *undo-tree*")
    (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
    (put-text-property (point) (1+ (point))
		       'face 'undo-tree-visualizer-current-face)))


(defun undo-tree-visualize-switch-next-branch (arg)
  "Switch to next branch of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo' or `undo-tree-visualizer-redo'."
  (interactive "p")
  (switch-to-buffer-other-window undo-tree-visualizer-buffer)
  (switch-to-buffer-other-window " *undo-tree*")
  ;; un-highlight old active branch below current node
  (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
  (let ((undo-tree-insert-face 'undo-tree-visualizer-default-face))
    (save-excursion
      (undo-tree-draw-subtree (undo-tree-current buffer-undo-tree) 'active)))
  ;; increment branch
  (let ((branch (undo-tree-node-branch (undo-tree-current buffer-undo-tree))))
  (setf (undo-tree-node-branch (undo-tree-current buffer-undo-tree))
	(cond
	 ((>= (+ branch arg) (undo-tree-num-branches))
	  (1- (undo-tree-num-branches)))
	 ((<= (+ branch arg) 0) 0)
	 (t (+ branch arg))))
  ;; highlight new active branch below current node
  (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face))
    (save-excursion
      (undo-tree-draw-subtree (undo-tree-current buffer-undo-tree) 'active)))
  ;; re-highlight current node
  (put-text-property (point) (1+ (point))
		     'face 'undo-tree-visualizer-current-face)))


(defun undo-tree-visualize-switch-previous-branch (arg)
  "Switch to previous branch of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo' or `undo-tree-visualizer-redo'."
  (interactive "p")
  (undo-tree-visualize-switch-next-branch (- arg)))


(defun undo-tree-visualizer-quit ()
  "Quit the undo-tree visualizer."
  (interactive)
  (undo-tree-clear-visualizer-data buffer-undo-tree)
  (kill-buffer-and-window))


(defun undo-tree-visualizer-set (pos)
  "Set buffer to state corresponding to undo tree node
at POS."
  (interactive "@e")
  (setq pos (event-start (nth 1 pos)))
  (let ((node (get-text-property pos 'undo-tree-node)))
    (when node
      ;; set parent buffer to state corresponding to node at POS
      (set-buffer undo-tree-visualizer-buffer)
      (undo-tree-set node)
      (set-buffer " *undo-tree*")
      (undo-tree-draw-tree buffer-undo-tree))))

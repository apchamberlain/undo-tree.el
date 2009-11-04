
;;; undo-tree.el --- Treat undo history as a tree


;; Copyright (C) 2009 Toby Cubitt

;; Author: Toby Cubitt <toby-undo-tree@dr-qubit.org>
;; Version: 0.1
;; Keywords: undo, redo, history, tree
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Commentary:
;;
;; Emacs has a powerful undo-system. Unlike the standard undo/redo system in
;; most software, it allows you to recover *any* past state of a buffer
;; (whereas the standard undo/redo system loses past states as soon as you
;; redo). However, this power comes at a price: people can find Emacs' undo
;; confusing and difficult to use, spawning a number of packages that replace
;; it with the less powerful but more intuitive undo/redo system.
;;
;; Both the loss of data with standard undo/redo, and the confusion of Emacs'
;; undo, stem from trying to treat undo history as a linear sequence of
;; changes. It's not. Undo-tree-mode replaces Emacs' undo system with a system
;; that treats undo history as what it is: a branching tree of changes. This
;; simple idea allows the more intuitive behaviour of the standard undo/redo
;; system to be combined with the power of never losing any history. An added
;; side bonus is that undo history can be stored more efficiently, allowing
;; more changes to accumulate before Emacs starts discarding history.
;;
;; The only downside to this more advanced yet simpler undo system is that it
;; was inspired by Vim. But, after all, successful religions always steal the
;; best ideas from other religions.
;;
;;
;; Quick-Start
;; ===========
;;
;; If you're the kind of person who likes jump in the car and drive, without
;; bothering to first figure out whether the button on the left dips the
;; headlights or opens sun-roof (after all, you'll soon figure it out when you
;; push it), then here's the minimum you need to know:
;;
;; `undo-tree-mode' and `global-undo-tree-mode'
;;   Enable undo-tree mode (either in the current buffer or globally).
;;
;; C-_  C-/  (`undo-tree-undo')
;;   Undo changes.
;;
;; C-+  C-?  (`undo-tree-redo')
;;   Redo changes.
;;
;; `undo-tree-switch-branch'
;;   Switch undo-tree branch.
;;   (What does this mean? Better press that button and see!)
;;
;; C-x u  (`undo-tree-visualize')
;;   Visualize undo tree.
;;   (Better press this button too!)
;;
;;
;; In the undo visualizer:
;;
;; <up>  p  C-p  (`undo-tree-visualize-undo')
;;   Undo changes.
;;
;; <down>  n  C-n  (`undo-tree-visualize-undo')
;;   Undo changes.
;;
;; <left>  b  C-b  (`undo-tree-visualize-switch-previous-branch')
;;   Switch to previous undo-tree branch.
;;
;; <right>  f  C-f  (`undo-tree-visualize-switch-next-branch')
;;   Switch to next undo-tree branch.
;;
;; q  C-q
;;   Quit undo-tree-visualizer.
;;
;; ,  <
;;   Scroll left.
;;
;; .  >
;;   Scroll right.
;;
;; <pgup>
;;   Scroll up.
;;
;; <pgdown>
;;   Scroll down.
;;
;;
;;
;; Explanation
;; ===========
;;
;; To understand the different undo systems, it's easiest to consider an
;; example. Imagine you make a few edits in a buffer. As you edit, you
;; accumulate a history of changes, which we might visualize as a string of
;; past buffer states, growing downwards:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o  (first edit)
;;                                |
;;                                |
;;                                o  (second edit)
;;                                |
;;                                |
;;                                x  (current buffer state)
;;
;;
;; Now imagine that you undo the last two changes. We can visualize this as
;; rewinding the current state back two steps:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                x  (current buffer state)
;;                                |
;;                                |
;;                                o
;;                                |
;;                                |
;;                                o
;;
;;
;; However, this isn't a good representation of what Emacs' undo system
;; does. Instead, it treats the undos as *new* changes to the buffer, and adds
;; them to the history:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o  (first edit)
;;                                |
;;                                |
;;                                o  (second edit)
;;                                |
;;                                |
;;                                x  (buffer state before undo)
;;                                |
;;                                |
;;                                o  (first undo)
;;                                |
;;                                |
;;                                x  (second undo)
;;
;;
;; Actually, since the buffer returns to a previous state after an undo, a
;; better way to visualize it is to imagine the string of changes turning back
;; on itself:
;;
;;        (initial buffer state)  o
;;                                |
;;                                |
;;                  (first edit)  o  x  (second undo)
;;                                |  |
;;                                |  |
;;                 (second edit)  o  o  (first undo)
;;                                | /
;;                                |/
;;                                o  (buffer state before undo)
;;
;; Treating undos as new changes might seem a strange thing to do. But the
;; advantage becomes clear as soon as we imagine what happens when you edit
;; the buffer again. Since you've undone a couple of changes, new edits will
;; branch off from the buffer state that you've rewound to:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o
;;                                |\
;;                                | \
;;                                o  x  (new edit)
;;                                |
;;                                |
;;                                o
;;
;; The standard undo/redo system only lets you go backwards and forwards
;; linearly. So as soon as you make that new edit, it discards the old
;; branch. Emacs' undo just keeps adding changes to the end of the string. So
;; the undo history in the two systems now looks like this:
;;
;;            Undo/Redo:                      Emacs' undo
;;
;;               o     	       	       	       	  o
;;               |	       	       	       	  |
;;               |	      	      		  |
;;               o	      	   		  o  o
;;               .\                               |  |\
;;               . \                              |  | \
;;               .  x  (new edit)                 o  o	|
;;   (discarded  .                                | /   |
;;     branch)   .                                |/    |
;;               .                                o     |
;;                                                      |
;;                                                      |
;;                                                      x  (new edit)
;;
;; Now, what if you change your mind about those undos, and decide you did
;; like those other changes you'd made after all? With the standard undo/redo
;; system, you're dead. There's no way to recover them, because that branch
;; was discarded when you made the new edit.
;;
;; However, in Emacs' undo system, those old buffer states are still there in
;; the undo history. You just have to rewind back through the new edit, and
;; back through the changes made by the undos, until you reach them. Of
;; course, since Emacs treats undos (even undos of undos!) as new changes,
;; you're really weaving backwards and forwards through the history, adding
;; new changes to the end of the string as you go:
;;
;;				  o
;;     	       	       	       	  |
;;                                |
;;                                o  o	   o  (undo new edit)
;;                                |  |\	   |\
;;                                |  | \   | \
;;                                o  o  |  |  o	 (undo the undo)
;;                                | /   |  |  |
;;                                |/    |  |  |
;;               (trying to get   o     |  |  x	 (undo the undo)
;;                to this state)        | /
;;                                      |/
;;                                      o
;;
;; So far, this is still reasonably intuitive to use. It doesn't behave so
;; differently to standard undo/redo, except that by going back far enough you
;; can access changes that would be lost in standard undo/redo.
;;
;; However, imagine that after undoing as just described, you decide you
;; actually want to rewind right back to the initial state. If you're lucky,
;; and haven't invoked any command since the last undo, you can just keep on
;; undoing until you get back to the start:
;;
;;               (trying to get   o		 x  (got there!)
;;     	       	  to this state)  |		 |
;;                                |		 |
;;                                o  o	   o  	 o  (keep undoing)
;;                                |  |\	   |\	 |
;;                                |  | \   | \	 |
;;                                o  o  |  |  o	 o  (keep undoing)
;;                                | /   |  |  |	/
;;                                |/    |  |  |/
;;               (already undid   o     |  |  o  (got this far)
;;                to this state)        | /
;;                                      |/
;;                                      o
;;
;; But if you're unlucky, you've moved the point (say) after getting to the
;; point labelled "got this far". In that case, you've "broken the undo
;; chain". If you try to undo now, Emacs thinks you're trying to undo the
;; undos. So to get back to the initial state you now have to rewind through
;; *all* the changes, including the undos you just did:
;;
;;      (trying to get   o                          x  (finally got there!)
;;       to this state)  |                          |
;;                       |                          |
;;                       o  o     o     o     o     o
;;                       |  |\    |\    |\    |\    |
;;                       |  | \   | \	| \   | \   |
;;                       o  o  |  |  o	o  o  |  o  o
;;                       | /   |  |  | /   |  |  | /
;;                       |/    |  |  |/    |  |  |/
;;      (already undid   o     |  |  o<.   |  |  o
;;       to this state)        | /     :   | /
;;                             |/      :   |/
;;                             o       :   o
;;                                     :
;;                             (got this far, but
;;                              broke undo chain)
;;
;; Confused?
;;
;; In practice you can just hold down the undo key until you reach the buffer
;; state that you want. But whatever you do, don't move around in the buffer
;; to check! Because you'll break the undo chain, and then you'll have to
;; traverse the entire string of undos again to get back to the point at which
;; you broke the chain. Commands such as `undo-only', and undo in region (in
;; transient-mark-mode), help make using Emacs' undo a little easier, but
;; nonetheless it remains confusing.
;;
;;
;; So what does undo-tree mode do? Remember the diagram we drew to represent
;; the history we've been discussing (make a few edits, undo a couple of
;; times, and edit again)? The diagram that conceptually represented our undo
;; history, before we started discussing specific undo systems? It looked like
;; this:
;;
;;                                o  (initial buffer state)
;;                                |
;;                                |
;;                                o
;;                                |\
;;                                | \
;;                                o  x  (current state)
;;                                |
;;                                |
;;                                o
;;
;; Well, that's *exactly* what the undo history looks like to undo-tree-mode.
;; It doesn't discard the old branch (as standard undo/redo does), nor does it
;; treat undos as new changes to be added to the end of a linear string of
;; buffer states (as Emacs' undo does). It just keeps track of the tree of
;; branching changes that make up the entire undo history.
;;
;; If you undo from this point, you'll rewind back up the tree to the previous
;; state:
;;
;;                                o
;;                                |
;;                                |
;;                                x  (undo)
;;                                |\
;;                                | \
;;                                o  o
;;                                |
;;                                |
;;                                o
;;
;; If you were to undo again, you'd rewind back to the initial state. If on
;; the other hand you redo the change, you'll end up back at the bottom of the
;; most recent branch:
;;
;;                                o
;;                                |
;;                                |
;;                                o  (start here)
;;                                |\
;;                                | \
;;                                o  x  (redo)
;;                                |
;;                                |
;;                                o
;;
;; So far, this is just like the standard undo/redo system. But what if you
;; want to return to a buffer state located on a previous branch of the
;; history? Since undo-tree-mode keeps the entire history, you simply need to
;; tell it to switch to a different branch, and then redo the changes you
;; want:
;;
;;                                o
;;                                |
;;                                |
;;                                o  (start here, but switch
;;                                |\  to the other branch)
;;                                | \
;;                        (redo)  o  o
;;                                |
;;                                |
;;                        (redo)  x
;;
;; Now you're on the other branch, and if you undo and redo changes you'll
;; stay on that branch, moving up and down through the buffer states located
;; on that branch. Until you decide to switch branches again, of course.
;;
;; Real undo trees might have multiple branches and sub-branches:
;;
;;                                o
;;                            ____|______
;;                           /           \
;;                          o             o
;;                      ____|__         __|
;;                     /    |  \       /   \
;;                    o     o   o     o     x
;;                    |               |
;;                   / \             / \
;;                  o   o           o   o
;;
;; Trying to imagine what Emacs' undo is doing as you move about such a tree
;; will likely frazzle your brain circuits! But in undo-tree-mode, you're just
;; moving up and down this undo history tree. Most of the time, you'll
;; probably only need to stay on the most recent branch, in which case it
;; behaves like standard undo/redo, so is just as simple to understand. But if
;; you ever need to recover a buffer state on a different branch, the
;; possibility of switching between branches and accessing the full undo
;; history is still there.
;;
;;
;; Actually, it gets better. You don't have to imagine all these diagrams,
;; because undo-tree-mode includes an undo-tree visualizer which draws them
;; for you. In fact, it draws even better diagrams: it highlights the node
;; representing the current buffer state, and it also highlights the current
;; branch. (There's one other tiny difference: the visualizer puts the most
;; recent branch on the left rather than the right, because it's slightly more
;; convenient for really big trees.)



;;; Change Log:
;;
;; Version 0.1
;; * initial release


;;; Code:

(provide 'undo-tree)


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
  ;; horizontal scrolling may be needed if tree is very wide
  (define-key undo-tree-visualizer-map ","
    (lambda () (interactive) "Scroll right." (scroll-right 1 t)))
  (define-key undo-tree-visualizer-map "."
    (lambda () (interactive) "Scroll left." (scroll-left 1 t)))
  (define-key undo-tree-visualizer-map "<"
    (lambda () (interactive) "Scroll right." (scroll-right 1 t)))
  (define-key undo-tree-visualizer-map ">"
    (lambda () (interactive) "Scroll left." (scroll-left 1 t)))
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

(defmacro undo-tree-node-p (n)
  (let ((len (length (make-undo-tree-node nil nil))))
    `(and (vectorp ,n) (= (length ,n) ,len))))



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
  (let ((stack (list (undo-tree-root undo-tree)))
	res)
    (while stack
      ;; try to compute widths for node at top of stack
      (if (undo-tree-node-p
	   (setq res (undo-tree-node-compute-widths (car stack))))
	  ;; if computation fails, it returns a node whose widths still need
	  ;; computing, which we push onto the stack
	  (push res stack)
	;; otherwise, store widths and remove it from stack
	(setf (undo-tree-node-lwidth (car stack)) (aref res 0)
	      (undo-tree-node-cwidth (car stack)) (aref res 1)
	      (undo-tree-node-rwidth (car stack)) (aref res 2))
	(pop stack)))))


(defun undo-tree-node-compute-widths (node)
  ;; Compute NODE's left-, centre-, and right-subtree widths. Returns widths
  ;; (in a vector) if successful. Otherwise, returns a node whose widths need
  ;; calculating before NODE's can be calculated.
  (let ((num-children (length (undo-tree-node-next node)))
	(lwidth 0) (cwidth 0) (rwidth 0)
	p w)
    (catch 'need-widths
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
	  (if (undo-tree-node-lwidth (car p))
	      (setq lwidth (+ lwidth
			      (undo-tree-node-lwidth (car p))
			      (undo-tree-node-cwidth (car p))
			      (undo-tree-node-rwidth (car p))))
	    ;; if child's widths haven't been computed, return that child
	    (throw 'need-widths (car p)))
	  (setq p (cdr p)))
	(if (undo-tree-node-lwidth (car p))
	    (setq lwidth (+ lwidth (undo-tree-node-lwidth (car p))))
	  (throw 'need-widths (car p)))
	;; centre-width is inherited from middle child
	(setf cwidth (undo-tree-node-cwidth (car p)))
	;; compute right-width
	(setq rwidth (+ rwidth (undo-tree-node-rwidth (car p))))
	(setq p (cdr p))
	(dotimes (i (/ num-children 2))
	  (if (undo-tree-node-lwidth (car p))
	      (setq rwidth (+ rwidth
			      (undo-tree-node-lwidth (car p))
			      (undo-tree-node-cwidth (car p))
			      (undo-tree-node-rwidth (car p))))
	    (throw 'need-widths (car p)))
	  (setq p (cdr p))))

       ;; even number of children
       (t
	(setq p (undo-tree-node-next node))
	;; compute left-width
	(dotimes (i (/ num-children 2))
	  (if (undo-tree-node-lwidth (car p))
	      (setq lwidth (+ lwidth
			      (undo-tree-node-lwidth (car p))
			      (undo-tree-node-cwidth (car p))
			      (undo-tree-node-rwidth (car p))))
	    (throw 'need-widths (car p)))
	  (setq p (cdr p)))
	;; centre-width is 0 when number of children is even
	(setq cwidth 0)
	;; compute right-width
	(dotimes (i (/ num-children 2))
	  (if (undo-tree-node-lwidth (car p))
	      (setq rwidth (+ rwidth
			      (undo-tree-node-lwidth (car p))
			      (undo-tree-node-cwidth (car p))
			      (undo-tree-node-rwidth (car p))))
	    (throw 'need-widths (car p)))
	  (setq p (cdr p)))))

      ;; return left-, centre- and right-widths
      (vector lwidth cwidth rwidth))))



(defun undo-tree-clear-visualizer-data (undo-tree)
  ;; Clear visualizer data from UNDO-TREE.
  (let ((stack (list (undo-tree-root undo-tree)))
	node)
    (while stack
      (setq node (pop stack))
      (setf (undo-tree-node-visualizer node) nil)
      (dolist (n (undo-tree-node-next node))
	(push n stack)))))


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
    (setq buffer-read-only nil)
    (undo-tree-draw-tree undo-tree)
    (setq buffer-read-only t)))


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
  (let ((undo-tree-insert-face 'undo-tree-visualizer-default-face)
	(max-lisp-eval-depth 1000000)
	(max-specpdl-size 1000000))
    (save-excursion (undo-tree-draw-subtree (undo-tree-root undo-tree))))
  ;; highlight active branch
  (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face)
	(max-lisp-eval-depth 1000000)
	(max-specpdl-size 1000000))
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
  (use-local-map undo-tree-visualizer-map)
  (setq truncate-lines t)
  (setq buffer-read-only t))


(defun undo-tree-visualize-undo (&optional arg)
  "Undo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
  (setq buffer-read-only nil)
  (put-text-property (point) (1+ (point)) 'face 'default)
  (switch-to-buffer-other-window undo-tree-visualizer-buffer)
  (unwind-protect
      (undo-tree-undo arg)
    (switch-to-buffer-other-window " *undo-tree*")
    (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
    (put-text-property (point) (1+ (point))
		       'face 'undo-tree-visualizer-current-face)
    (setq buffer-read-only t)))


(defun undo-tree-visualize-redo (&optional arg)
  "Redo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
  (setq buffer-read-only nil)
  (put-text-property (point) (1+ (point)) 'face 'default)
  (switch-to-buffer-other-window undo-tree-visualizer-buffer)
  (unwind-protect
      (undo-tree-redo arg)
    (switch-to-buffer-other-window " *undo-tree*")
    (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
    (put-text-property (point) (1+ (point))
		       'face 'undo-tree-visualizer-current-face)
    (setq buffer-read-only t)))


(defun undo-tree-visualize-switch-next-branch (arg)
  "Switch to next branch of the undo tree.
This will affect which branch to descend when *redoing* changes
using `undo-tree-redo' or `undo-tree-visualizer-redo'."
  (interactive "p")
  (switch-to-buffer-other-window undo-tree-visualizer-buffer)
  (switch-to-buffer-other-window " *undo-tree*")
  ;; un-highlight old active branch below current node
  (setq buffer-read-only nil)
  (goto-char (undo-tree-node-marker (undo-tree-current buffer-undo-tree)))
  (let ((undo-tree-insert-face 'undo-tree-visualizer-default-face)
	(max-lisp-eval-depth 1000000)
	(max-specpdl-size 1000000))
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
  (let ((undo-tree-insert-face 'undo-tree-visualizer-active-branch-face)
	(max-lisp-eval-depth 1000000)
	(max-specpdl-size 1000000))
    (save-excursion
      (undo-tree-draw-subtree (undo-tree-current buffer-undo-tree) 'active)))
  ;; re-highlight current node
  (put-text-property (point) (1+ (point))
		     'face 'undo-tree-visualizer-current-face)
  (setq buffer-read-only t)))


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
      (setq buffer-read-only nil)
      (let ((max-lisp-eval-depth 1000000)
	    (max-specpdl-size 1000000))
	(undo-tree-draw-tree buffer-undo-tree))
      (setq buffer-read-only t))))


;;; undo-tree.el ends here

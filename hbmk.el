;;; hbmk.el --- Manage bookmarks in HTML

;; Copyright (C) 1999, 2000 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: hypermedia
;; Version: 2.2

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; After you used your web browser for a while, a few months perhaps,
;; bookmarking pages as you go, you probably noticed that you had a
;; mess of bookmarks in no particular order, unorganized or barely
;; organized.

;; You can read the bookmark files easily enuff in emacs, and moving a
;; bookmark or two is easy, but managing the whole mess is a chore, if
;; you do it at all.  I basically didn't for a long time, and when I
;; decided to do it properly, it was such a chore that I wrote this
;; package to make it easier.

;; NB: You have to work kind of the way I work, moving bookmarks from
;; the accumulating files your browser writes into permanent storage,
;; one list in <ol></ol> to a file.  This code is a little more
;; user-friendly than the earlier version, but you still need clue
;; about HTML and bookmarks.

;; Prerequisites:

;; psgml (http://www.lysator.liu.se/projects/about_psgml.html)

;; tehom-psgml.el, my extensions to psgml

;; cl, the Common Lisp emulation.

;; tehom-4.el

;; You need to have sgml-custom-dtd set up so that "html" inserts the
;; html doctype.  See the psgml docs.

;; If you want to rearrange bookmarks on the fly, you need my
;; `arrange' package.

;; You need to basically work the same way I do; see above.

;; Installation

;; If you want to handle Netscape-style bookmarks, you must customize
;; `hbmk-fake-netscape-dtd-file' to point to the fake Netscape
;; bookmark dtd that comes with hbmk.

;; You probably want to customize `hbmk-bookmark-directory' to point
;; to wherever you will keep your new bookmark files.

;; Usage:

;; Open a file of bookmarks that was written by either Lynx or
;; Netscape.  Put point before a bookmark of interest and call
;; `hbmk-process-next-bookmark'.

;; You will be prompted for a file to put that bookmark in.  If the
;; file doesn't exist, hbmk will create it.  The bookmark will be
;; moved into that file if possible.

;; The files hbmk creates will always place the bookmarks in an
;; ordered list in HTML.

;; If it is tiresome to call `hbmk-process-next-bookmark' repeatedly,
;; use `hbmk-process-many', which will repeat until it is aborted or
;; encounters something it can't handle.  

;; To add a destination manually, `hbmk-add-destination-file' prompts
;; you for a filename, which then is available as a destination for
;; clippings.

;; `hbmk-add-reference' creates a reference at point to an HTML file.
;; If that file doesn't already exist, it will be created and filled
;; out appropriately.  It expects the point to be in an ordered HTML
;; list, <ol></ol>.  You use `hbmk-add-reference' when editing a file
;; that should contain a link to the new file, which usually means
;; you're making some sort of index file that indexes lesser pages.

;; The older function `hbmk-dispatch-region' deletes the currently
;; selected region and pastes it to a destination which the user
;; selects from a list.  It could still be useful for
;; non-machine-structured text.

;; `hbmk-bookmark-url-at-point' and `hbmk-create-bookmark' are useful
;; for handling bookmarks that are not already in HTML.
;; `hbmk-create-bookmark' is completely interactive: You type the url
;; and the description.  `hbmk-bookmark-url-at-point' uses the url at
;; point.

;;; Advice: 

;; Because psgml has to re-parse the file from the last changed
;; position, it sometimes makes sense to process a large bookmark file
;; starting near the end.

;; Known Bugs:

;; `hbmk-make-file' works for filenames ending with .html (or similar
;; according to auto-mode-alist), but gets weird on other files,
;; probably because of the underlying dtd behavior.  It could be
;; changed to force sgml-mode, but that would cause as many problems
;; as it solved.

;; Possibilities:

;; This code supports only Lynx-style and Netscape-style bookmarks.
;; but it is now extensible by adding new entries to hbmk-style-alist,
;; with appropriate support functions.  In theory the bookmarks need
;; not even by SGML-based.

;; `hbmk-dispatch-region' lacks support, so it uses the hbmk HTML
;; support, which doesn't do anything useful for unstructured text.
;; In theory, other support could build 

;; Different write configurations could be supported, prolly as
;; elements of hbmk-style-alist, and the user would customize by
;; choosing a write-style.

;;; Code:

;;;;;;;;;;;;;
;;Prerequisites

(require 'cl)  ;;Needed for `some', defstruct, etc
(require 'tehom-psgml)
(require 'tehom-4)
(eval-when-compile
  (require 'thingatpt)
  (require 'arrange))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Configuration parameters


(defgroup hbmk '()
  "hbmk for handling bookmarks."
  :group 'hypertext
  :group 'local)

(defcustom hbmk-fake-netscape-dtd-file 
  "~/dtd/fake-netscape.dtd"
  "Path to the fake Netscape dtd that comes with hbmk."
  :type  '(file :must-match t)
  :group 'hbmk)

(defcustom hbmk-bookmark-directory
  "~/"
  "Directory to default to when looking for hbmk bookmark files."
  :type 'directory
  :group 'hbmk)

(defcustom hbmk-write-style
  "hbmk"
  "Style in which hbmk should write bookmarks and bookmark files.
Unused for now; style is always hbmk."
  :type '(choice (const "hbmk"))
  :group 'hbmk)


(defvar hbmk-destination-alist-init-data 
  '()
  "Data used to initialize `hbmk-destination-alist'.
Not used or designed yet.")


;;;;;;;;;;;;;;;;;;;;;;;;;
;;The write configuration
;;These variables may be merged into hbmk-style later.  See
;;hbmk.el.rmb.xml 

(defvar hbmk-sgml-path-to-bookmarks '("BODY" "OL")
  "*Path to find a good place to insert bookmarks.  This must accord
with what hbmk-build-bookmark-holder-f writes." )

(defvar hbmk-write-bookmark-f 
  #'hbmk-write-bookmark
  "*Function to write a bookmark.
It must be a function taking 2 arguments, the url and the description." )

(defvar hbmk-build-bookmark-holder-f
  #'hbmk-build-bookmark-holder 
  "*Function to build a bookmark-holder file. 

It must be a function taking 2 arguments, the buffer to build the
bookmark holder in and a title for the document." )



;;;;;;;;;;;;;;;;;;
;;Internal configuration

(defstruct (hbmk-style (:type list))
  ""
  name
  description
  setup-func
  is-bookmark-p-func
  bookmark-get-data-func
  kill-bookmark-func)


(defconst hbmk-style-alist
  (eval-when-compile
    (list
      (make-hbmk-style 
	:name "Lynx"
	:description
	"Lynx-style bookmarks, stored in OL ordered lists."
	:setup-func
	#'ignore
	:is-bookmark-p-func
	#'hbmk-lynx-el-is-bookmark-p 
	:bookmark-get-data-func
	#'hbmk-lynx-get-data
	:kill-bookmark-func
	#'hbmk-lynx-kill-bookmark
	)
      (make-hbmk-style 
	:name "Netscape" 
	:description
	"Netscape-style bookmarks, a pseudo-HTML format that calls
itself doctype NETSCAPE-Bookmark-file-1 but really isn't a SGML markup
at all." 
	:setup-func
	#'hbmk-setup-netscape-bookmarks
	:is-bookmark-p-func
	#'hbmk-netscape-el-is-bookmark-p 
	:bookmark-get-data-func
	#'hbmk-netscape-get-data
	:kill-bookmark-func
	#'hbmk-netscape-kill-bookmark
	)

      (make-hbmk-style 
	:name "hbmk"
	:description
	"hbmk-style bookmarks, stored in OL ordered lists."
	:setup-func
	#'ignore
	:is-bookmark-p-func
	#'hbmk-lynx-el-is-bookmark-p 
	:bookmark-get-data-func
	#'hbmk-lynx-get-data
	:kill-bookmark-func
	#'hbmk-lynx-kill-bookmark
	)))
  

  "The styles of bookmarks hbmk knows about.")


(defconst hbmk-magic-destinations 
  '(("*Add a destination*" . nil))
  "" )

;;;;;;;;;;;;;
;;Internal variables.

(defvar hbmk-destination-alist 
  hbmk-magic-destinations
  "List of possible destinations for clippings.
Most elements are in the form \(FILENAME . MARKER\).  Other strings
besides FILENAME are possible, but there's no easy way to add them.
\(STRING . NIL\) is an element that indicates that the user wants to
add a new destination." )


(defvar hbmk-style nil 
  "Buffer-local variable indicating the style of the current buffer" )


;;;;;;;;;;;;
;;;Functions

;;;Write functions for the default write style.

(defun hbmk-write-bookmark (url description)
  "Insert an anchor to point to URL.

Specifically, insert DESCRIPTION inside an anchor inside a list
element.  The point must be inside a list."
  
  (tehom-psgml-add-element-to-element "li" nil 
    nil)

  (tehom-psgml-add-element-to-element "a" nil 
    (list (cons "HREF" url )))
  (insert 
    (concat "\n" description)))


;;

(defun hbmk-build-bookmark-holder (buf title)
  ""
  (with-current-buffer buf
    (erase-buffer)
    (sgml-custom-dtd "html")

    ;;We could force the buffer into sgml mode here, but that
    ;;would cause more problems than it solves.
    (goto-char (point-max))
    
    ;;Add the document in its full structure.  Meta is ignored because
    ;;for some reason, the HTML content model leaves it out.
    (tehom-psgml-add-element-to-element "html" nil
      `(
	 (sub-nodes
	   ("HEAD"
	     (sub-nodes
	       ("TITLE" (sub-nodes ("#PCDATA" ,title)))
	       ("META"  
		 ("CONTENT" . "Created-by")
		 (sub-nodes ("#PCDATA" "Created by hbmk")))))

	   ("BODY"
	     (sub-nodes
	       ("OL"))))))))


;;;General write functions, not specific to any style.

(defun hbmk-make-file (filename)
  "Create a file suitable for containing bookmarks.

The new file will be named FILENAME.  It will be immediately available
as a destination for clippings."

  (interactive "FDestination Filename? ")  
  (if
    (file-exists-p filename)
    (hbmk-add-destination-file filename)
    (let* 
      (
	(buf (find-file-noselect filename))
	(prompt (format "Title for %s: " filename))
	(title (read-string prompt)))

      (funcall hbmk-build-bookmark-holder-f buf title)
      (with-current-buffer buf
	(save-buffer)
	(hbmk-make-insertion-target buf)))))



(defun hbmk-add-reference  (name description)
  "Add an HTML anchor pointing to a destination file NAME.
Must be in a Lynx-style bookmark file.

The destination file will be created and initialized if it doesn't
already exist.

The anchor is inserted into the current file, which should be HTML
with the point in a list.  The text DESCRIPTION is inserted into the
anchor."
  
  (interactive "FFilename? \nsDescription? ")

  (hbmk-setup-buffer)

  ;;Hack: prevent it from writing it styles that it doesn't know how
  ;;to handle.
  (unless
    (string= (hbmk-style-name hbmk-style) "Netscape")
    (let
      ((true-file-name
	 (file-relative-name 
	   (expand-file-name name))))
    
      (hbmk-make-file name)

      (funcall hbmk-write-bookmark-f true-file-name description))))


(defun hbmk-make-insertion-target (&optional buffer)
  "Make BUFFER a destination for bookmarks.

This function tries to find a good place to insert them, using
hbmk-sgml-path-to-bookmarks.

If BUFFER is not given, the current buffer will be used."
  
  (save-excursion
    (when buffer
      (set-buffer buffer))

    (let* 
      (ol-element
	(already-in 
	  (some
	    #'(lambda (cell)
		(and
		  (markerp (cdr cell))
		  (eq (current-buffer) (marker-buffer (cdr cell)))))
	
	    hbmk-destination-alist)))
      
      (if already-in
	(progn
	  (message "The destination %s is already known." (buffer-name))
	  nil)
	
	(progn

	  ;;Go to the proper element, if any.
	  (setq ol-element
	    (tehom-psgml-goto-element-stag-end hbmk-sgml-path-to-bookmarks))
      
	  (if ol-element
	    (let* 
	      ;;Make a marker pointing there.
	      ((marker (point-marker))
		name)

	      ;;Set the marker type so we always insert after it.
	      (set-marker-insertion-type marker t)

	      ;;We'll use the buffer name as the key
	      (setq name (buffer-name (current-buffer)))

	      ;;Save the marker on the list.  cl version:
	      (push 
		(cons name marker)
		hbmk-destination-alist)
	  
	      marker)

	    (progn
	      (message "There is no good place to put bookmarks in %s"
		(buffer-name))

	      nil)))))))



;;;Destination-management functions


(defun hbmk-add-magic-destinations ()
  "Force certain entries to be included in destinations."

  (dolist 
    (dest hbmk-magic-destinations)
    (add-to-list
      'hbmk-destination-alist
      dest)))



(defun hbmk-add-destination-file (filename)
  "Make a buffer for FILENAME available as a destination for
clippings." 
  
  (interactive "f")
  
  (let* 
    ((buf (find-file-noselect filename)))
    (hbmk-make-insertion-target buf)))


(defun hbmk-pick-destination (text)
   "Pick a destination for text.

When this encounters killed buffers, it adds new markers, leaving the
dead marker still present in `hbmk-destination-alist'."

   (when (null hbmk-destination-alist)
      (hbmk-add-magic-destinations))

   (let*
      (
	 (truncated-text
	    (subseq text 0 (min 20 (length text))))
      
	 (prompt
	    (format 
	       "Move %s to which file? "
	       truncated-text))
	 (destination 
	    (tehom-completing-read-assoc 
	       prompt
	       hbmk-destination-alist)))
    
      (cond
	 (  ;;If the destination exists and points to an unkilled
	    ;;buffer, use it.
	    (and
	       (markerp destination)
	       (marker-buffer destination))
	    destination)
	 (t ;;Otherwise look on disk.
	    (let
	       ((default-directory hbmk-bookmark-directory))
	       (call-interactively #'hbmk-make-file))))))


(defun hbmk-arrange-destinations ()
  "Remove zero or more entries from the list of destinations."

  (interactive)
  (require 'arrange)
  
  (let* 
    (
      (old-strings (mapcar #'car hbmk-destination-alist))
      ;;Remove items from the list.  
      (new-strings
	(arrange-strings old-strings))
      ;;Rebuild the remaining entries.
      (new-entries
	(mapcar
	  #'(lambda (name)
	      (assoc name hbmk-destination-alist))
	  new-strings)))

    (setq hbmk-destination-alist new-entries)
    ;;Add the entries (one entry) that should always be present.
    (hbmk-add-magic-destinations)))





;;; Bookmark recognition functions

(defun hbmk-setup-buffer (&optional force)
  "Set up the current buffer as a bookmark file"
  
  (unless
    (and
      hbmk-style
      (not force))

    (let*
      ((style-name
	 (completing-read
	   "What type of bookmark file is this? "
	   hbmk-style-alist))
	(style
	  (assoc style-name hbmk-style-alist)))
    
      ;;Call that style's setup function.
      (funcall 
	(hbmk-style-setup-func style))

      ;;Remember the style as a buffer-local variable.
      (make-variable-buffer-local 'hbmk-style)
      (setq hbmk-style style))))


(defun hbmk-get-bookmark-element ()
  "Get a nearby element that is a bookmark."

  ;;Find the apparent element
  (let
    ((next-el (tehom-psgml-next-el (point)))
      (bookmark-p (hbmk-style-is-bookmark-p-func hbmk-style)))
    
    
    (when next-el
      ;;Look at the element and its descendants.  We test the root
      ;;explicitly, because `tehom-psgml-get-child-by-test-recursive'
      ;;only looks at proper descendants.
      (if 
	(funcall bookmark-p next-el)
	next-el
	(tehom-psgml-get-child-by-test-recursive 
	  next-el bookmark-p)))))


;;; Main bookmark processing


(defun hbmk-insert-bookmark (marker url description)
  ""
  
  (when
    marker
    (with-current-buffer
      (marker-buffer marker)
      (goto-char marker)
      (funcall hbmk-write-bookmark-f url description))))

(defun hbmk-process-bookmark (bookmark)
  ""
  
  (let* 
    ( 
      (get-data-f (hbmk-style-bookmark-get-data-func hbmk-style))
      (data 
	(funcall get-data-f bookmark))
      (url         (car  data))
      (description (cadr data))

      ;;Pick a destination.  
      (marker
	(hbmk-pick-destination description)))

    (when
      marker
      (let
	;;Possibly get a new description.
	((new-description
	   (read-string "Description: " description)))
	  
	;;Cut the original bookmark.  
	(funcall
	  (hbmk-style-kill-bookmark-func hbmk-style)
	  bookmark)

	(hbmk-insert-bookmark marker url new-description)))))


;;; Entry points to move bookmarks.


;;;###autoload
(defun hbmk-process-next-bookmark ()
  "Cut the next bookmark and dispatch it to a chosen destination."

  (interactive)

  (hbmk-setup-buffer)
  (let
    ((bookmark (hbmk-get-bookmark-element)))
    
    (when bookmark
      (hbmk-process-bookmark bookmark))))


;;;###autoload
(defun hbmk-process-many ()
  "Process bookmarks from point onwards until error or until aborted."
  
  (interactive)
  (hbmk-setup-buffer)
  (let
    ((bookmark (hbmk-get-bookmark-element)))
    
    (while bookmark
      (hbmk-process-bookmark bookmark)
      (setq bookmark (hbmk-get-bookmark-element)))))

;;;###autoload
(defun hbmk-create-bookmark (url description)
  "Interactively create a bookmark."
  
  (interactive "surl: \nsDescription: ")

  (let
    ( 
      (marker
	(hbmk-pick-destination description)))

    (hbmk-insert-bookmark marker url description)))


;;;###autoload
(defun hbmk-bookmark-url-at-point (description)
  "Create a bookmark from a buffer in a non-markup mode"
  
  (interactive "sDescription: ")
  (require 'thingatpt)

  (let
    ((url (thing-at-point-url-at-point)))
    (hbmk-create-bookmark url description)))



;;Obsolete: We now find an exact node.  Possibly still useful for
;;categorizing&moving text whose structure is not machine-readable,
;;but to really use it this way, we'd need different writing functions
;;and a system for managing them.
(defun hbmk-dispatch-region (start end)
  "Cut the current region and dispatch it to a chosen destination.

The user selects the destination from hbmk-destination-alist."
  
  (interactive "r")
  (let*
    (
      (region-text (buffer-substring start end))
      (marker
	(hbmk-pick-destination region-text)))
    
    (when
      marker
      ;;Cut the region.
      (delete-region start end)

      ;;In the destination buffer, insert the text
      (with-current-buffer
	(marker-buffer marker)
	(goto-char marker)
	(insert region-text)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Support for Lynx-style

;;No special Lynx setup function

(defun hbmk-lynx-el-is-bookmark-p (el)
  ""

  (and
    (string= (sgml-element-gi el) "LI")
    ;;We assume that the first element is the "A" anchor.
    (let
      ((child (sgml-element-content el)))
      (string= (sgml-element-gi child) "A"))))

;;Assumes el meets the test hbmk-lynx-el-is-bookmark-p
(defun hbmk-lynx-get-data (el)
  ""
  
  (let*
    ;;We assume that the first element is the "A" anchor.
    (
      (anchor-el (sgml-element-content el))
      (url
	(sgml-element-attval
	  anchor-el "HREF"))
      (description 
	(tehom-psgml-index-get-el-contents anchor-el)))
    
    (list url description)))

;;Works around Lynx-style weirdness where li elements (list elements)
;;are not terminated, and therefore appear to swallow the rest of the
;;bookmark file. 
(defun hbmk-lynx-kill-bookmark (el)
  "Cut a lynx-style bookmark."
  (let*
    (
      (li-child
	(tehom-psgml-get-child-by-test el
	  #'(lambda (child-el)
	      (string= (sgml-element-gi child-el) "LI"))))
      
      (true-end
	(if
	  li-child
	  (sgml-element-start li-child)
	  (sgml-element-end   el))))
    
    (kill-region
      (sgml-element-start el)
      true-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Support for Netscape-style


(defun hbmk-setup-netscape-bookmarks ()
  "Prepare the current buffer to be treated as Netscape bookmarks.
Basically we have to do work to make Netscape bookmarks behave like
SGML, because Netscape just fakes it."

  (save-excursion
    (widen)
    ;;Hack:  Search past TITLE and H1
    (goto-char (point-min))
    (search-forward "</TITLE>")
    (search-forward "</H1>")

    (narrow-to-region (point) (point-max))
  
    (sgml-setup-doctype "DL"
      `(nil ,hbmk-fake-netscape-dtd-file . ,default-directory))))


(defun hbmk-netscape-el-is-bookmark-p (el)
  ""

  (and
    (string= (sgml-element-gi el) "DT")
    (let
      ((child (sgml-element-content el)))
      (string= (sgml-element-gi child) "A"))))

;;Untested.
'(hbmk-netscape-get-data (sgml-find-element-of (point)))

;;Assumes el passes the test hbmk-netscape-el-is-bookmark-p
(defun hbmk-netscape-get-data (el)
  ""
  
  (let*
    ;;We assume that the first element is the "A" anchor.
    ((anchor-el (sgml-element-content el))
      (url
	(sgml-element-attval
	  anchor-el "HREF"))

      (description 
	(tehom-psgml-index-get-el-contents anchor-el)))
    
    (list url description)))


(defun hbmk-netscape-kill-bookmark (el)
  ""

  (kill-region
    (sgml-element-start el)
    (sgml-element-end   el)))



;;Local variables:
;;change-log-default-name: "hbmk.changelog"
;;End:

;;; hbmk.el ends here

(in-package #:lgame.state)

(defvar *screen* nil
  "If using a single window, this is a convenient reference to the underlying sdl-window returned by lgame.display:create-window")

(defvar *screen-rect* nil
  "Deprecated, should use *screen-box*, but represents the sdl-rect of the window's size (logical)")

(defvar *screen-box* nil
  "If using a single window, this is a convenient reference to a box covering its (logical) size.")

(defvar *renderer* nil
  "If using a single window, this is a convenient reference to its main renderer returned by lgame.display:create-renderer")

(defvar *texture-loader* nil
  "This should probably not be used by outside code directly, as it represents a global texture cache from calls to lgame.texture:get-texture and
   is automatically cleared on lgame.quit")

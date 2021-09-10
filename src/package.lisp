(in-package #:cl-user)

(defpackage #:lgame
  (:shadowing-import-from #:sdl2-ffi.functions #:sdl-thread-id)
  (:use #:common-lisp #:sdl2-ffi.functions #:sdl2-ffi #:annot.std #:annot.class)
  )
;  (:shadowing-import-from #:sdl2-ffi
;                          sdl-event
;                          +sdl-window-opengl+ +sdl-messagebox-error+ +sdl-init-everything+ +sdl-hint-render-scale-quality+
;                          +sdl-quit+ +sdl-keydown+ +sdl-scancode-q+ +sdl-mousebuttondown+
;                          )
;  (:export
;    #:main ; launches game and blocks until window closes
;    #:launch-game ; meant for dev to run to get a non-blocking game window
;    ))

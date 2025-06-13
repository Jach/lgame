(in-package #:lgame.font.ffi)

(cffi:define-foreign-library lgame-sdl2-ttf
  (:unix "libSDL2_ttf.so")
  (:windows "SDL2_ttf.dll")
  (:darwin (:or (:framework "SDL2_ttf") (:default "libSDL2_ttf"))))

(cffi:use-foreign-library lgame-sdl2-ttf)

;; Currently only implementing what I need.

(cffi:defcfun ("TTF_Init" ttf-init)
              :int)

(cffi:defcfun ("TTF_Quit" ttf-quit)
              :void)

(cffi:defcfun ("TTF_OpenFont" ttf-open-font)
              :pointer
              (file :string)
              (ptsize :int))

(cffi:defcfun ("TTF_CloseFont" ttf-close-font)
              :void
              (font :pointer))

(cffi:defcfun ("TTF_RenderUTF8_Blended" ttf-render-utf8-blended)
              :pointer
              (font :pointer)
              (text :string)
              (color :unsigned-int))


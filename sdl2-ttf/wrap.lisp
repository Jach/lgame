(defpackage #:lgame-sdl2-ttf
  (:use #:common-lisp))
(in-package :lgame-sdl2-ttf)

(cffi:define-foreign-library lgame-sdl2-ttf
  (:unix "libSDL2_ttf.so")
  (:windows "SDL2_ttf.dll")
  (:darwin (:or (:framework "SDL2_ttf") (:default "libSDL2_ttf"))))

(cffi:use-foreign-library lgame-sdl2-ttf)

(defpackage #:lgame-sdl2-ttf.ffi
  (:use))
(in-package :lgame-sdl2-ttf.ffi)

(autowrap:c-include '(lgame sdl2-ttf/ "SDL_ttf.h")
                    :spec-path '(lgame sdl2-ttf-spec)
                    :exclude-definitions ("(scanf)$"
                                          "reallocarray"
                                          "posix_memalign"
                                          "va_list"
                                          "va-list"
                                          "SDL_LogMessageV"
                                          "__gnuc_va_list"
                                          "acoshl" "acosl" "asinhl" "asinl" "atan2l" "atanhl" "atanl" "cbrtl" "ceill"
                                          "copysignl" "coshl" "cosl" "dreml" "erfcl" "erfl" "exp2l" "expl" "expm1l"
                                          "fabsl" "fdiml" "finitel" "floorl" "fmal" "fmaxl" "fminl" "fmodl" "gammal"
                                          "hypotl" "ilogbl" "isinfl" "isnanl" "j0l" "j1l" "jnl" "lgammal" "lgammal_r"
                                          "llrintl" "llroundl" "log10l" "log1pl" "log2l" "logbl" "logl" "lrintl" "lroundl"
                                          "modfl" "nanl" "nearbyintl" "nextafterl" "nexttoward" "nexttowardf" "nexttowardl"
                                          "powl" "qecvt" "qecvt_r" "qfcvt" "qfcvt_r" "qgcvt" "remainderl" "remquol" "rintl"
                                          "roundl" "scalbl" "scalblnl" "scalbnl" "SDL_vasprintf" "sdl_vsnprintf"
                                          "significandl" "sinhl" "sinl" "sqrtl" "strtold" "tanhl" "tanl" "tgammal"
                                          "truncl" "vdprintf" "vfprintf" "vfwprintf" "vprintf" "vsnprintf" "vsprintf"
                                          "vswprintf" "vwprintf" "wcstold" "y0l" "y1l" "ynl" "_FLOAT64X" "_tile1024i"
                                          "__fmodl" "__fpclassifyl" "__gammal" "__hypotl" "__ilogbl" "__iseqsigl"
                                          "__isinfl" "__isnanl" "__issignalingl" "__j0l" "__j1l" "__jnl" "__lgammal"
                                          "__lgammal_r" "__llrintl" "__llroundl" "__log10l" "__log1pl" "__log2l" "__logbl"
                                          "__logl" "__lrintl" "__lroundl" "__m128" "__m128bh" "__m128d" "__m128d_u" "__m128i"
                                          "__m128i_u" "__m128_u" "__m256" "__m256bh" "__m256d" "__m256d_u" "__m256i"
                                          "__m256i_u" "__m256_u" "__m512" "__m512bh" "__m512d" "__m512d_u"
                                          "__m512i" "__m512i_u" "__m512_u" "__m64" "__modfl" "__nanl" "__nearbyintl"
                                          "__nextafterl" "__nexttoward" "__nexttowardf" "__nexttowardl" "__powl"
                                          "__remainderl" "__remquol" "__rintl" "__roundl" "__scalbl" "__scalblnl"
                                          "__scalbnl" "__signbitl" "__significandl" "__sinhl" "__sinl" "__sqrtl"
                                          "__tanhl" "__tanl" "__tgammal" "__truncl" "__v16hi" "__v16hu" "__v16qi"
                                          "__v16qs" "__v16qu" "__v16sf" "__v16si" "__v16su" "__v1di" "__v2df" "__v2di"
                                          "__v2du" "__v2hi" "__v2qi" "__v2si" "__v32hi" "__v32hu" "__v32qi" "__v32qs"
                                          "__v32qu" "__v4df" "__v4di" "__v4du" "__v4hi" "__v4qi" "__v4sf" "__v4si" "__v4su"
                                          "__v64qi" "__v64qs" "__v64qu" "__v8df" "__v8di" "__v8du" "__v8hi" "__v8hu" "__v8qi"
                                          "__v8sf" "__v8si" "__v8su" "__y0l" "__y1l" "__ynl"
                                          )
                    :release-p cl:t)

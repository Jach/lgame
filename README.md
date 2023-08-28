# lgame

lgame is a [pygame](https://www.pygame.org/news)-inspired library for making
simple games in Common Lisp using
[cl-sdl2](https://github.com/lispgames/cl-sdl2).

**This is very very pre-alpha quality** and as such I don't really recommend it to
others yet. Still, perhaps it will be useful alongside
[lgame-examples](https://github.com/Jach/lgame/tree/master/examples) as a reference or
study.

If you do end up using lgame anyway, it'd be nice to know so I can try and avoid
breaking your code with future changes, or at least provide a fix-up patch!

# Example

Compared to the 4-line example shown by
[trivial-gamekit](https://github.com/borodust/trivial-gamekit), lgame takes a
bit more effort. But for the full control it may be worth it. Assuming you have
loaded lgame already (see next [Usage](#usage) section) you can copy this to a
file and load it, or paste it directly into your REPL:

```lisp
(defpackage #:lgame.example.hello
  (:use #:cl))
(in-package :lgame.example.hello)

(defun main ()
  (lgame:init)
  (lgame.display:create-centered-window "Hello Comparison" 800 600)
  (lgame.display:create-renderer)

  (let* ((font (lgame.font:load-font (lgame.font:get-default-font) 15))
         (txt (lgame.font:render-text font "Hello, lgame!" 0 0 0))
         (txt-rect (lgame.rect:get-texture-rect txt)))
    (lgame.rect:move-rect txt-rect 240 (- 600 240 (sdl2:rect-height txt-rect))) ; gamekit's origin is bottom-left, we are top-left following SDL
    (lgame.time:clock-start)
    (unwind-protect
      (loop while (lgame.time:clock-running?) do
            (livesupport:continuable
              (game-tick txt txt-rect)))

      (sdl2:free-rect txt-rect)
      (sdl2:destroy-texture txt)
      (lgame:quit))))

(defun game-tick (txt txt-rect)
  (lgame.event:do-event (event)
    (when (= (lgame.event:event-type event) lgame::+sdl-quit+)
      (lgame.time:clock-stop)))

  (lgame.render:set-draw-color 255 255 255)
  (lgame.render:clear)

  (lgame.render:blit txt txt-rect)

  (lgame.render:present)

  (livesupport:update-repl-link)
  (lgame.time:clock-tick 60))

(main)
```

If you are using SLIME, the use of the livesupport calls should prevent your
main REPL loop from being taken over. This is great for interactive development.

# Usage

lgame is not in Quicklisp, so you should first clone it to
~/quicklisp/local-projects/ or put a symlink there so that your local quicklisp
and ASDF can find it. Try to `(ql:quickload "lgame")` and verify all its dependencies are
loaded. In particular, you'll need to first install some libraries to your OS if
you haven't done so already:

* SDL2
* SDL2-image
* SDL2-mixer
* SDL2-ttf

The SDL libraries should be "fully loaded", i.e. if you're on gentoo, you'll need to
make sure libsdl2 includes things like haptic support. This is because by
default lgame tries to initialize SDL with "everything".

After verifying it loads, you can try running some of the examples in
[lgame-examples](https://github.com/Jach/lgame/tree/examples) to see that it
works. They're written to be runnable standalone with `sbcl --script` or except
for `taste.lisp` to be runnable by
loading the file and executing `(main)`. Some examples will take over the REPL
thread, but some make use of
[livesupport](https://github.com/cbaggers/livesupport) for interactive
development.

After that, happy hacking! `aliens.lisp` is probably the most full-featured
example at the moment for you to consider referencing, but my goal is that by
looking at a random pygame game or a using-SDL2-with-C/C++ tutorial or the
official [SDL docs](https://wiki.libsdl.org/) you should be able to gather how
to accomplish the same things using lgame and not have to hunt around too deeply
through underdocumented Lisp code. I've provided a [Library
Organization](#library-organization) section here in the readme covering the
namespaces of lgame which can help orient you further.

# License

lgame code itself is licensed under the Unlicense and as such is in the public
domain. You can use any part of this as-is or snip it into your own code
without attribution. Assets under `assets/` are also in the public domain,
**unless** they are accompanied by their own individual licenses, such as the
default included font.

lgame examples are also under the Unlicense/public domain, unless a particular
example indicates otherwise.

If you want to build and distribute binaries using lgame, it is important to
adhere to the license requirements of all dependencies, such as SDL and various
Lisp systems used by lgame. `lgame.util:display-licenses` can help find this
information.

# Contributing

If you do end up using this or wanting to use this despite the warnings and
limitations, and find some issues or want to contribute back some fixes or
features, feel free to open up an issue and/or pull request and I'll try to look
into it but no promises. For a pull request in particular, I'm happy, like many
projects, to assume implicit agreement with the 'license' choice if nothing
special is said, but it'll help if you explicitly include somewhere a statement
acknowledging that you're putting such a contribution into the public domain,
see [unlicense.org](https://unlicense.org/) if you want something to copy. If
you disagree with putting stuff into the public domain, I'm open to accepting a
contribution in the form of an additional dependency to `lgame.asd` pointing to
your extension. Also happy if you want to put yourself in a new authors.md file.

# Library Philosophy

The goal of lgame is not to duplicate pygame's API entirely, or even to wrap
SDL2 and friends as thoroughly and carefully, but to provide something close
enough which facilitates making the sorts of programs found on pygame.org about
as easily but in Common Lisp. To further that end, lgame is not against
eventually becoming a sort of "kitchen sink" of features that can be useful for
a lot of games. For instance, some A\* pathfinding code is included. Basically
as I make my own games or game concepts, if I find myself needing something in
more than one of them, it's likely to end up in lgame, or at least an example.

A lot of code so far is very optimistic and doesn't bother with all the error
checks recommended by SDL or in many cases done for you by cl-sdl's `sdl2:`
package wrapper functions. I'll try to improve such things over time but it's
not a high priority.

Similarly I haven't gotten anything to the point where I want to build and
distribute binaries beyond my own machines. I develop with gentoo linux, so
there may be platform specific bugs with Mac or Windows that I haven't run into.
I also don't plan on ever explicitly testing and supporting Mac for either
development or game binaries, so only Windows will realistically receive my
attention for faults. If you want to contribute Mac fixes though, I'll accept
them.

lgame is currently "single-threaded preferred". That is, like most pygame games,
the style of development should be that one thread is responsible for
initializing everything, loading assets (and unloading them later), and running
the game loop. This keeps things on a relatively 'happy' path with respect to
threading issues, especially around foreign memory and GL stuff, but it's
obviously not modern. I'd like to experiment sometime with a more multi-threading
aware architecture, including one that automatically and safely manages foreign
memory, but this will likely end up changing a lot of interfaces and may be
better to do without SDL under the hood in the first place.

## Why not just use plain cl-sdl2 for a game?

Despite having some useful wrappers, it's not nearly complete enough a wrapper
over SDL2 as is Pygame. Since SDL is a C library, if you were using C or C++ you
would have to worry about low level memory management yourself. Pygame entirely
eliminates that, mostly because it itself is implemented as mostly C code to
expose a nicer interface to Python.

cl-sdl2 *does not* handle the memory management for you in the general case.
What's worse, the documentation can be actively misleading!

(In 2022, https://github.com/lispgames/cl-sdl2/commit/2d761165f01f03f2a8012be683828895ee6821c9
made the following remarks obsolete; I've left it here for now as a historical note.)

--begin outdated remarks--

This commentary isn't meant to denigrate anyone who has contributed to the project, I'm very
glad that cl-sdl2 exists, and arguably I could help things more with a pull
request rather than this note, but consider
[rect.lisp](https://github.com/lispgames/cl-sdl2/blob/2b21bc2c18dc846e7a72951340014b8f504b2943/src/rect.lisp).
Its first function, `make-point`, has lying documentation that it will be garbage
collected as needed. It is lying because, if you look at
[cl-autowrap](https://github.com/rpav/cl-autowrap)'s `plus-c:c-let` function
that `make-point` is using, the let takes an optional `:free t` statement that
by default is `nil`. Of course it must be `nil` here for `make-point`, because
if it simply allocated and freed, it would have nothing valid to return. The
conclusion is that if you use `sdl2:make-point`, or other functions like
`sdl2:make-rect`, you are responsible for calling `sdl2:free-point` and
`sdl2:free-rect` later on when you are done with them. The GC will not clear up
this foreign memory for you.

Now if you look at the commit history of that file, you'll find a
[big commit](https://github.com/lispgames/cl-sdl2/commit/329e0ccefff25e87985f188ee5dc88833a8da956#diff-16795e9608b700a33f873738739c3a035eb5f4276b0f743568b7eb968fc7881f)
about disabling finalizers. What are those? They are a concept in the
[trivial-garbage](https://github.com/trivial-garbage/trivial-garbage) library.
In theory, with careful usage, they allow you to create a GC-trackable object
wrapper, and when the GC decides to garbage collect that wrapper, you can have
it call a custom hook that can clean up the underlying foreign resource unknown
to the GC. [cl-sdl2-ttf](https://github.com/Failproofshark/cl-sdl2-ttf), which
lgame currently [ed: *no longer*] relies on to provide bindings for SDL2-TTF for font rendering,
*does* still use finalizers, possibly correctly, and even on underlying SDL
objects like surfaces where my preference is to just explicitly free them.

I asked mfiano about that commit once, here's what he said:

> (09:45:21 AM) mfiano: In the past cl-sdl2 had finalizers, but they caused many bugs, like double-frees. I removed that the first chance I got when I took over the project.\
> (09:45:37 AM) mfiano: Bugs mostly because it's own codebase was using them wrong.\
> (09:46:19 AM) mfiano: Not to mention, you never know when a finalizer is going to run with the GC, and they have to be finalized on the same thread they were constructed on. It was a serious bug trap

This seems reasonable to me. So again, just a little bit of doc string cleanup
would go a long way to making it clear that people are responsible, unless they
are able to use a lexical `with-...` macro like `with-rects` which you can see
does does have a call to `free-rect`. Similarly if they just used the previously
mentioned `c-let` with `:free t`. However even that might not be desirable,
see the [Library Organization](#lgameevent) section on the lgame.event package
for some notes on how I try to have macros that enable stack allocation.

--end outdated remarks--

At the time of this writing, lgame expects the game to run in a single thread,
but I want to make it multi-threaded eventually, or at least to not fall apart
if someone else wants to incorporate it in a multi-threaded game. The elephant
in the room is that bad stuff can happen on various platforms when you don't
work with memory interacting with the GPU (think: textures, GL calls) from
within the same thread. It's a commonly discovered problem for C++ developers
too. This despite that the concept of "render thread" is legacy/technical debt
now and that modern engines use a multi-threaded job-scheduling architecture
with nothing called the render thread. One day I'll learn more about what's
actually going on but for the curious see cl-sdl2's source itself for various Mac
workarounds, or this 2012
[interview](https://fabiensanglard.net/doom3/interviews.php) with Carmack:

> on windows, OpenGL can only safely draw to a window that was created by the same thread. We created the window on the launch thread, but then did all the rendering on a separate render thread. It would be nice if doing this just failed with a clear error, but instead it works on some systems and randomly fails on others for no apparent reason.\
> The Doom 4 codebase now jumps through hoops to create the game window from the render thread and pump messages on it, but the better solution, which I have implemented in another project under development, is to leave the rendering on the launch thread, and run the game logic in the spawned thread.

*Anyway* this whole digression should make it clear that cl-sdl2 is not very
high level, and that's sufficient for why I won't just use it directly for a
game and would create wrappers if I didn't have lgame, and why even if cl-sdl2
provides a convenient wrapper for something, I might use it, but I also might
not and just prefer using the raw underlying FFI stuff. On top of that, again,
is that it doesn't fully wrap SDL in Lispy ways, so you'll need to get
underneath it to the actual FFI for some things anyway. You have to do this so
often, you almost might as well treat cl-sdl2 as just handling the bare minimum
of using cl-autowrap yourself on the header files. If it has something higher
level you can safely and intuitively use, great!

## What does/doesn't lgame wrap?

If there's a corresponding direct way of doing something pygame provides in one
or maybe two function calls to something provided by cl-sdl2 or an SDL2 FFI
call, then I don't want to wrap it with an lgame namespaced function. Though for
convenience lgame does :use all the functions and symbols in the FFI. See the
project organization section.

The core viewpoint is that "how can I do X?" should be answerable by looking up
how it's done in regular SDL2 or sometimes pygame (many examples available
online) and then just translating it directly (including manual memory
management!), but possibly searching lgame for existing usage and discovering a
convenient wrapper. (For example, a single function call to handle loading an
image with sdl2-image into a surface, converting that to a texture, and freeing
the surface.)

As I work on my own small game ideas, I may want to generalize and include
something into lgame that has no equivalent in pygame, for example A\*
pathfinding. I currently have no precommitments to minimalism. Another more
trivial example is `(lgame.loader:get-texture ...)` which 1) keeps a centralized
copy of the `SDL_Texture` making it easy to unload (and free) them all later,
and 2) caches them so that a common 'mistake' I've found (and probably made) in
pygame games of loading the images in a sprite object's constructor carries no
penalty. Basically I'm a fan of having program-wide managers or services
("Services Architecture") for various things even inside libraries, that other
objects talk to, subscribe to, or pluck stuff from, instead of having every
program having to create functionally identical management systems for so many
things on their own.

This approach can in the future maybe be used to provide safe finalizers on
wrapped objects -- e.g. the thing being wrapped keeps track of what thread
created the foreign memory in addition to the foreign memory pointer itself, and
so when the GC decides in its thread that it's time to deallocate it, the
routine actually sends that request to the responsible thread.

# Library Organization

## Caveats

lgame makes use of implicit state, I'm currently undecided if this is a good
idea and might change things later. As one example, to control a game's frame
rate you can call `(lgame.time:clock-start)` before the game loop, and
`(lgame.time:clock-tick 60)` at the end of each frame. In contrast pygame has
you make a `pygame.time.Clock()` object and tick that each frame. My theory is
that most games don't need more than one of these, so lgame can just keep the
state for you. Similar idea with `lgame:*screen*` and `lgame:*renderer*` which
are exported and intended to be passed to appropriate sdl2 functions if I didn't
bother making a wrapper for them that uses them implicitly.

**Currently foreign memory must be manually managed.** lgame does manage or
provide helpers for managing a few things (and I'll need to better document
them) but generally if you do something like making a texture or long-lived
SDL\_Rect you're responsible for freeing it. I may investigate using finalizers
so that things can be garbage collected, or perhaps make it configurable, but
that's a longer term plan.

## Packages

lgame is divided into several packages to mimic (in a non-hierarchical way)
pygame's structure and to hold to a general design principle that each service
should get its own package/namespace. I try to document things in the Lisp code
itself, so if you're comfortable jumping in consult `packages.lisp` and then
each lisp file implementing the package you're interested in. Currently I make
extensive use of [cl-annot](https://github.com/m2ym/cl-annot) and so generally
speaking you'll need to look file-by-file (or rely on your editor's symbol
auto-completion) to see what symbols are actually exported to each package.

### lgame.state

This package manages global shared state among the various lgame modules that
upstream games can also access and make use of. For example, many SDL functions
require an SDL\_Renderer argument, this is exposed with
`lgame.state:*renderer*`.

This state is use'd and re-exported into the "top-level" lgame package for convenience.

### lgame

The default top-level package fulfills two purposes. The first is to provide the
entry point function `(lgame:init)` that should be called by any game before
using other lgame features, and the exit point function `(lgame:quit)` that
should be called when a game closes. (Importantly, this quit will not kill your
Lisp image, so you can restart a game if you need to, it just cleans up any
lgame state.)

The second purpose is to use and re-export `lgame.state`'s state symbols, and to
use (but not export) *all* symbols in the `sdl2-ffi.functions` and `sdl2-ffi`
packages (that is, the underlying SDL functions and enums). The exported state
symbols are:

* `*screen*` -- aka the SDL\_Window created for you by
  `(lgame.display:create-window ...)` that wraps `SDL_CreateWindow(...)`.
* `*screen-rect*` -- a convenient SDL\_Rect with fields x=0, y=0,
  width=windowWidth, height=windowHeight. Note that if you use
  `lgame.display:set-logical-size` the `*screen-rect*` will be updated to have
  the logical width and height instead of the actual window's.
* `*renderer*` -- the SDL\_Renderer created for you by
  `(lgame.display:create-renderer)`, parented to the window.
* ...possible misc singletons -- exported for internal convenience like
  `*texture-loader*` but shouldn't be interfaced with directly (use
  `lgame.loader`)

These symbols may in the future turn into functions instead, so consider
wrapping them in inline getters over using them directly...

The benefit of having the FFI stuff available in this package is that if you
read some random C/C++ code and see some functions you'd like to call, it's
easy. e.g.:

```c
SDL_Texture texture = SDL_CreateTextureFromSurface(main_renderer, source_surf);
```

turns to:

```lisp
(setf texture (lgame::sdl-create-texture-from-surface lgame:*renderer* source-surface))
```

You can of course explicitly use `sdl2-ffi.functions:sdl-create-texture-from-surface`, the symbols refer to the same thing.
Or even use `sdl2:create-texture-from-surface`, which wraps the sdl- function
but includes a null pointer check.

Similarly using things like either `lgame::+sdl-windowpos-centered+` vs. `sdl2-ffi:+sdl-windowpos-centered+` is up to you.

### lgame.display

Exports functions for setting up your game window.

* `lgame.display:create-window` -- creates an SDL\_Window and binds it to `lgame:*screen*`,
  also returns it if you need a local copy.
* `lgame.display:create-centered-window` -- convenience version for having the window
  centered on the desktop.
* `lgame.display:create-renderer` -- creates an SDL\_Renderer and binds it to
  `lgame:*renderer*`, also returns it if you need a local copy.
* `lgame.display:set-logical-size` -- can make use of SDL2's logical screen size
  feature. If you wanted to make a retro 640x480 game, you might set the window
  size to that and go into full screen and hope your monitor has a mode for that
  (with black bars if the aspect ratio isn't the same). With SDL2, you don't
  have to do that anymore. Now you can make your window size something like
  1920x1080, but set the logical size to 640x480. Your render code stays the
  same as if you had a small window (drawing in a 640x480 zone) but SDL will
  automatically upscale things for you and add black bars (or whatever bg you want)
  to preserve the aspect ratio. If you go into fullscreen, the monitor's display mode
  also doesn't need to change.
* `lgame.display:screenshot-png` -- lets you save a screenshot of your rendered frame.

An example usage:
```lisp
(lgame:init)
(lgame.display:create-centered-window "Hello" 1920 1080)
(lgame.display:create-renderer)
; game loop here, you're setup with a 1920x1080 screen ready to render stuff on
(lgame:quit)
```

### lgame.event

Utils and wrappers around SDL2 events, particularly handling the event loop
somewhat nice and intuitively with a `lgame.event:do-event` macro and to reference such
event data with `ref` or a specialized function on top of `ref`.
Here is some more explanation in the context of code:

```lisp
;; Example do-event usage for a few event types and properties:

(lgame.event:do-event (event)
  (when (or
          (= (lgame.event:event-type event) lgame::+sdl-quit+)
          (and (= (lgame.event:event-type event) lgame::+sdl-keydown+)
               (= (lgame.event:key-scancode event) lgame::+sdl-scancode-escape+)))
    ; quit
    (setf *running?* nil))

  (when (= (lgame.event:event-type event) lgame::+sdl-mousebuttondown+)
    ; ...
    )

  (when (= (lgame.event:event-type event) lgame::+sdl-mousebuttonup+)
    ; ...
    )

  (when (and (= (lgame.event:event-type event) lgame::+sdl-keyup+)
             (= (lgame.event:key-scancode event) lgame::+sdl-scancode-f+))
    ; f to toggle full screen without switching the monitor's display mode
    (if *full-screen*
        (progn (lgame::sdl-set-window-fullscreen lgame:*screen* 0) (setf *full-screen* nil))
        (progn (lgame::sdl-set-window-fullscreen lgame:*screen* lgame::+sdl-window-fullscreen-desktop+) (setf *full-screen* t)))))
```

Support functions are `lgame.event:event-type` and `lgame.event:key-scancode`. They both build on a
general `ref` macro. Taken from `event.lisp`:

```lisp
(defmacro ref (event &rest fields)
  `(plus-c:c-ref ,event sdl2-ffi:sdl-event ,@fields))

(defun event-type (event)
  (ref event :type))

(defun key-scancode (event)
  (ref event :key :keysym :scancode))
```

`plus-c:c-ref` lets you poke at nested struct fields. So you could write
`key-scancode` as `(plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym :scancode)`
which is basically equivalent to C's `event.key.keysym.scancode`.

Knowing what fields exist requires looking at
the [SDL Documentation](https://wiki.libsdl.org/SDL_Event) and knowing your type.
For key codes, you start with the SDL\_Event page and descend to the
SDL\_KeyboardEvent then the SDL\_Keysym and lastly the SDL\_Scancode (an enum).

`do-event`'s definition just loops over `sdl-poll-event`:

```lisp
(defmacro do-event ((event) &body loop-body)
  "Helper macro to iterate through SDL's event list until it is empty,
   binding each SDL_Event to event."
  `(with-event (,event)
     (loop until (zerop (sdl2-ffi.functions:sdl-poll-event ,event))
           do
           ,@loop-body)))
```

All this I think is easier to understand than the `sdl2:with-event-loop` macro.

Of important note here is my `with-event` macro. It binds foreign SDL\_Event
structs (and similar macros bind other things like SDL\_Rect) in a more
efficient way than using cl-sdl2's macros.  This is done by using cffi directly
with a constantly known type size, enabling allocation on the stack. See the
macro doc:

```lisp
(defmacro with-event ((event) &body body)
  "Helper macro to enable sdl event allocation on the stack.
   Verify with macroexpand this:

(with-event (event)
  (print event))

  against:

(sdl2:with-sdl-event (event)
  (print event))

  or even:

(plus-c:c-let ((event sdl2-ffi:sdl-event :free t))
  (print event))
"
  (let ((size (autowrap:foreign-type-size (autowrap:find-type 'sdl2-ffi:sdl-event))))
    `(cffi:with-foreign-pointer (,event  ,size)
       ,@body)))
```

### lgame.mouse

* `lgame.mouse:get-mouse-pos` -- simple wrapper to get the mouse position as an
  (x y) list.

### lgame.time

Some functions to handle time-related tasks, notably enforcing a max FPS
framerate.

Example usage:

```lisp
(lgame.time:clock-start)
;; now start the game loop
(loop while (lgame.time:clock-running?) do
  (lgame.event:do-event (event)
    (when should-quit? (lgame.time:clock-stop))
    ...)
  ; render...
  (lgame.time:clock-tick 60))
```

Note some examples still make use of their own `*running?*` variable. Do what
you like.

`clock-tick` should be called at the end of the frame, because it takes the duration of
the frame into account for how long it should sleep for. Sleeping is done with `lgame::sdl-delay`.

If you don't pass anything to `clock-tick`, the framerate will not be capped. In
any case, you can also use the return value of `clock-tick` which is the time in
milliseconds for the frame to complete (independent of whether sleeping
occurred).

Unfortunately since the clock resolution is only in milliseconds, this isn't really a good value to
use as a "dt" variable to pass down to update functions for frame-rate-independent physics. You should
read [Gaffer On Games - Fix Your Timestep!](https://gafferongames.com/post/fix_your_timestep/) anyway.

If you intentionally delay a frame with `(sleep (/ 1 30))`, or dropped to around
30 FPS due to an intensive scene, you would at least see a useful number like 33
or 34. Keeping statistics of this number can help you notice when you don't have
consistent frame times (e.g. a garbage collection might have kicked off).

### lgame.rect

Package for working with SDL\_Rects. You can create them, update them, and do
useful things like checking for collisions or scaling. There's also a handy
`rect-coord` function to query or set things about the rect without having to
touch the underlying x,y,w,h fields for everything. e.g.:

```lisp
(lgame.rect:with-rect (r 0 0 5 5)
  ; 5x5 rect at x,y coordinate 0,0
  (lgame.rect:rect-coord r :center) ; query its center position, i.e. (2,2)
  (lgame.rect:rect-coord r :topright) ; query its top-right position, i.e. (5,0)
  (setf (lgame.rect:rect-coord r :topright) '(20 40)) ; moves top-right position to coord (20,40)
  (lgame.rect:rect-coord r :center) ; now rect center is at (17,42). Its left axis is at 15, its right at 20, i.e. it's still a 5x5 rect.
  (lgame.rect:outside-screen? r) ; sees if this rect lgame.rect:collide-rect? with lgame.state:*screen-rect*
  )
```

Note that since SDL Rects must use integers for their values, setters
may automatically `truncate` their inputs (the same as casting to int in C),
and destructive moves use `round`.

### lgame.sprite

Similar set of classes and mixins to work with game sprites compared to
[pygame.sprite](https://www.pygame.org/docs/ref/sprite.html). Check out
`chimp.lisp` for a simple example or `aliens.lisp` for a bigger example. The
main idea is that you give each sprite its own class inheriting from the
appropriate lgame.sprite base class, set the sprite's image and rect slots in a
constructor, and implement the specializing method `update`. If you need
something more complicated than "blitting" the sprite's image slot to the
location specified by its rect slot, you can also implement the method `draw`.
You should write your game loop to call update/draw for all sprites every frame.
There's also a `group` class to create sprite groups and thus only have to call
(from the main loop) update/draw on the group itself.

### lgame.font

For now, lgame just provides a way to load and store TTF files
(it also includes a default liberally licensed font if you
don't want to download one or specify a system font) and a single
`lgame.font:render-text` function that functionally does the same as `sdl2-ttf:render-text-solid`
but without using finalizers. Other ttf functions are exposed in the
`lgame-sdl2-ttf.ffi` package.

Thus you *will* need to clean up the font texture yourself after you're done with
it. See `chimp.lisp` for a static example which basically goes like this:

```lisp
  ; pre-game loop setup
  (let* ((font (lgame.font:load-font (lgame.font:get-default-font) 20))
         (banner-txt (lgame.font:render-text font "Your message here" 10 10 10))
         (banner-txt-rect (lgame.rect:get-texture-rect banner-txt)))
    ; ...
    (unwind-protect (game-loop)
      (sdl2:free-rect banner-txt-rect)
      (sdl2:destroy-texture banner-txt)
      (lgame:quit)))

  ; inside game loop:
  (lgame.render:blit banner-txt banner-txt-rect)

  ;; note: could also write:
  ;(sdl2:render-copy lgame:*renderer* banner-txt :dest-rect banner-txt-rect)
  ;; or:
  ;;(lgame::sdl-render-copy lgame:*renderer* banner-txt nil banner-txt-rect)
```

### lgame.render

Some wrappers like the just shown `blit` that let you avoid having to specify
the `*renderer*` all the time.

* `lgame.render:blit` - wraps sdl-render-copy for a texture and only dest-rect
* `lgame.render:clear` - wraps sdl-render-clear
* `lgame.render:present` - wraps sdl-render-present
* `lgame.render:set-draw-color` - wraps sdl-set-render-draw-color, allows passing
in a 3 or 4 length rgb/rgba list, or providing the rgba arguments explicitly.
* `lgame.render:with-draw-color*` - macro to temporarily set and then restore a
  render draw color
* `lgame.render:with-render-target*` - macro to temporarily set and then restore
  a render target to something besides the whole screen texture

### lgame.loader

Lets you use a singleton to load and cache textures, with them automatically
being destroyed on `lgame:quit`. Example:

```lisp
; in setup, after (lgame:init)
(lgame.loader:create-texture-loader (directory-namestring *load-truename*))
; later on in game:
(let ((image (lgame.loader:get-texture :ball)))
  (setf (lgame.sprite:.image self) image)
  (setf (lgame.sprite:.rect self) (lgame.rect:get-texture-rect image)))
```

That will try to load "ball.png" from the directory that the lisp file resides
in. If `get-texture` is called with the same data as before, it will return the
texture from cache.

`get-texture` maps keyword names to lowercase png files, but you can also give
it an explicit filename string like "my ball.jpg" instead. If you need to use a
different context directory than the one specified at creation time, you can
pass in a `:dir` keyword argument. If you need to set a color-key for alpha
transparency, you can pass in a `:color-key` argument as a list of 0-255 (r g b).

### lgame.pathfinding

Incomplete port of an old A\* implementation. Check out subdirectories of examples/maze/ for
usage until I get around with re-creating a playground and adding in all the
extra functionality the original supported. I also want to benchmark it for
speed and accuracy against some game maps before I declare it reliable
(especially because it's using an odd extra map array for data that should
probably be stored with the primary map array cells instead).

### lgame.util

Utility functions that don't fit in other packages, and may indeed be better
split off into separate libraries. Currently just a function for printing
licensing information that may be useful when shipping a game binary.

### Potential future packages...

There is no lgame.sound, but that may change. For now, using sdl2-mixer directly
as some examples do is easy enough.

The two GL examples show that using OpenGL is a lot of work, even when using SDL
to do the initial window setup. If I start experimenting with more 3D games and
using raw GL calls, some useful things may end up in something like a lgame.gl
package.

If lgame ever does become a true 'kitchen sink' it might become better to think
of it as an engine, but I'd rather at that point just split things off into an
engine.

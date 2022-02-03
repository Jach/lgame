# lgame

lgame is a [pygame](https://www.pygame.org/news)-inspired library for making
simple games in Common Lisp using
[cl-sdl2](https://github.com/lispgames/cl-sdl2).

This is very pre-alpha quality and as such I don't really recommend it to others
yet except perhaps along with
[lgame-examples](https://github.com/Jach/lgame-examples) as a reference. This is
why I haven't even asked for it to be included into quicklisp yet, but that may
change in the future.

# Usage

Because this isn't in quicklisp, you should first clone it to
~/quicklisp/local-projects/ or put a symlink there so that your local quicklisp
and ASDF can find it. Try to quickload it and verify all its dependencies are
loaded. In particular, you'll need to first install some libraries to your OS if
you haven't done so already:

* SDL2
* SDL2-image
* SDL2-mixer
* SDL2-ttf

The libraries should be "fully loaded", i.e. if you're on gentoo, you'll need to
make sure libsdl2 includes things like haptic support.

# Library Philosophy

The goal of lgame is not to duplicate pygame's API entirely, or even to wrap
SDL2 and friends as thoroughly, but to provide something close enough that
facilitates making the sorts of programs found on pygame.org about as easily in
Common Lisp.

If there's a corresponding direct way of doing something pygame provides in one
or maybe two function calls to something provided by cl-sdl2 or an sdl2 FFI
call, then I don't want to wrap it. The idea is that "how can I do X?" should be
answerable by looking up how it's done in regular SDL2 or sometimes pygame (many
examples available online) and then just translating it directly (including
manual memory management!), but possibly searching lgame for existing usage and
discovering a convenient wrapper. (For example, a single function call to handle
loading an image with sdl2-image into a surface, converting that to a texture,
and freeing the surface.)

As I work on my own small game ideas, I may want to generalize and include
something into lgame that has no equivalent in pygame, for example A\*
pathfinding. I currently have no precommitments to minimalism. Another more
trivial example is `(lgame.loader:get-texture ...)` which 1) keeps a centralized
copy of the `SDL_Texture` making it easy to unload (and free) them all later,
and 2) caches them so that a common 'mistake' I've found (and probably made) in
pygame games of loading the images in a sprite object's constructor carries no
penalty.  Basically I'm a fan of having program-wide managers for various things
that other objects subscribe themselves to or pluck copies of assets from,
instead of having every program having to manage everything on its own.

## Caveats

lgame makes use of implicit state, I'm currently undecided if this is a good
idea. As one example, to control a game's frame rate you can call
`(lgame.time:clock-start)` before the game loop, and `(lgame.time:clock-tick
60)` at the end of each frame. In contrast pygame has you make a
`pygame.time.Clock()` object and tick that each frame. My theory is that most
games don't need more than one of these, so lgame can just keep the state for
you. Similar idea with `lgame:*screen*` and `lgame:*renderer*` which are
exported and intended to be passed to appropriate sdl2 functions.

Currently memory must be manually managed. lgame manages some things (and I'll
need to document them) but generally if you make your own texture, you're
responsible for freeing it. I may investigate using finalizers so that things
can be garbage collected, or perhaps make it configurable.

# License

lgame itself is licensed under the Unlicense and as such is in the public domain.
Assets in assets/ may be covered by their own individual licenses.

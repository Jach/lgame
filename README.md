This project is a collection of projects aimed at building out my idea for an
"lgame" library, a Pygame-inspired library for Common Lisp.

The goal of lgame is not to duplicate pygame's API entirely, or even to wrap
SDL2 and friends as thoroughly, but to provide something close enough that
facilitates making the sorts of programs found on pygame.org about as easily (if
not more easily) in Common Lisp. In my own dabbling I've found the existing
options for game making in Lisp are too low-level (they provide little apart
from a FFI wrapper around using SDL directly, as lgame does, or OpenGL), too
high-level (works well enough to build simple arcade games, if you don't need
much customization), or too idiosyncratic. For whatever its flaws, PyGame has
had a long period of success, so following its approach to things seems like a
good idea.

My general philosophy (for now) is that if there's a corresponding direct way of
doing something Pygame provides in one or maybe two function calls to something
provided by cl-sdl2 or an sdl2 FFI call, then I don't want to wrap it. The idea
is that "how can I do X?" should be answerable by looking up how it's done in
regular SDL2 or sometimes Pygame (many examples available online) and then just
translating it directly (including manual memory management!), but possibly
searching lgame for existing usage and discovering a convenient wrapper. (For
example, a single function call to handle loading an image with sdl2-image into
a surface, converting that to a texture, and freeing the surface.)

On the other hand, as I work on my own small game ideas, I may want to
generalize and include something into lgame that has no equivalent in pygame,
just because it seems like such an obvious thing many games would want. An
example of this is `lgame:*texture-loader*` which 1) keeps a centralized copy of
the `SDL_Texture` making it easy to unload (and free) them all later, and 2)
caches them so that a common 'mistake' I've found (and probably made) in pygame
games of loading the images in a sprite object's constructor carries no penalty.
Basically I'm a fan of having program-wide managers for various things that
other objects subscribe themselves to or pluck copies of assets from, instead of
having them manage everything on their own.

The goal of this project is to whip lgame into something resembling working
order. At the time of writing I've barely got anything, only starting to extract
bits I'm starting to get satisfied with from what I thought was going to be a
quick arcade game clone. I realize I need to go simpler. So I'll implement some
basic pygame tutorials/examples, maybe some of my own old pygames, and then
hopefully some small stuff from other people on pygame's site.

Besides that, having more examples to learn from (even bad examples!) is what
helps build a community. PyGame has an excellent documentation site, as well as
some simple tutorials, but it's easy to learn so much more just by reading the
source of various projects. And since I'm not trying to do anything too fancy
here, even if someone doesn't want to use lgame, an example may at least provide
inspiration for how to do something with cl-sdl2. Maybe in the far future I can
work on documentation, but I'd first need to have something worthwhile to
document. You can perceive lack of doc as a reflection of my own opinion on that.

# License

All project code and lgame itself are licensed under the Unlicense, that is, are in
the public domain.

Certain assets may have their own licenses, I've tried to call them out in the
project/asset folders if so under a LICENSE file for that folder.

Pygame's own examples (https://github.com/pygame/pygame/tree/main/examples) that
many of these are based on are noted to be in the public domain.

A casual reminder that any binary build distributions should include somewhere
the license information of all dependencies.

# Projects

Each project has its own folder, here is a suggested order to look at:

* Taste
* Moveit

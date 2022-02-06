This project is a collection of projects aimed at building out my idea for an
"lgame" library, a Pygame-inspired library for Common Lisp.

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

Files are designed to be runnable with `sbcl --script`, some may be load-able as
well in your REPL to run as you please.

Each project has its own folder, here is a suggested order to look at:

* Taste
* Moveit
* Liquid
* Testsprite
* Chimp
* Vgrade
* Aliens
* Maze
* gl, gl2

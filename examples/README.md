# lgame-examples

Here are provided examples showing how to use lgame (and sdl2 under the hood) to
accomplish various things.

Most of them are based on corresponding [pygame
examples](https://github.com/pygame/pygame/tree/main/examples)

Others are based on things found elsewhere in the lisp gamedev world, or are
based entirely of my own creation.

Still others (in the future) are large and complex enough (or are too derivative
of something not public domain licensed) that I will instead provide links in
this readme to their respective projects.

If you ever use lgame yourself, feel free to ping me/make a pull request to put
your link here!

# Running

Each example here is made to be run as a standalone script with `sbcl --script
example/example.lisp`. They share the feature that their first line quickloads
lgame (so be sure to setup lgame first so that quicklisp can find it). Included
is a `run-all.sh` if you just want to run them and see what's here.

Every example except for `taste/taste.lisp` should safely be `LOAD`able into
your running Lisp session, after which you can execute
`(lgame.example.EXAMPLE-NAME:main)` to run the example. You can load `taste` as
well, but it will immediately start executing. When you execute a particular
example's `main`, some will take over your REPL thread until you close them, but
others make use of [livesupport](https://github.com/cbaggers/livesupport) to
leave your REPL functional and allow you to interactively modify the examples as
they're running.

`lgame-examples.asd` exists to load all (but `taste`) in one system, but this is
mainly used to facilitate checking for breaking changes with `sblint`.

# Examples

Under the pygame-inspired section examples are listed roughly in order of
complexity. None can be called "good ideas" in the sense of being instructive in
how to more properly structure code (that will require a bigger example in the
[External](#external) section) but, where the documentation of lgame itself lacks, should
provide at least enough clues for how to get started. Always remember that the
SDL2 way of doing something is the canonical way with lgame, so you should
become familiar with SDL2's API to be fully effective.

## pygame-inspired

## Others

## External

# Motivation

The original goal of these examples was, beyond my own WIP games, to help whip
lgame into something resembling working order. The ongoing goal is to continue
making more examples, especially if I can make narrow targeted examples based on
extracts from my games. The more things I make the more I realize what should
change or not or what really needs to be added. Hopefully after enough examples
(and pain of refactoring them if I want to make breaking changes) lgame will
eventually turn into something that someone else will want to try using.

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

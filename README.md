haskmas
==

A haskell-themed tree decoration written in haskell. Happy Haskmas!

[![Build status](https://travis-ci.org/silky/haskmas.svg)](https://travis-ci.org/silky/haskmas) [![Build status](https://ci.appveyor.com/api/projects/status/hkc8fywse26wvwkh?svg=true)](https://ci.appveyor.com/project/silky/haskmas)

![Haskmas in 2D and 3D](haskmas_all.png)

[(In online viewer in GitHub) Haskmas in 3D](https://github.com/silky/haskmas/blob/master/haskmas.stl)

Original [Haskmass](http://www.meetup.com/Melbourne-Haskell-Users-Group/events/222203592/)
logo courtesty of [Lyndon](https://github.com/sordina).


usage/installation
==

- Download directly from [Thingiverse](http://www.thingiverse.com/thing:1187442)

or

- Build/run with [stack](https://github.com/commercialhaskell/stack)

````
stack build
stack exec haskmas
````

At the moment this outputs `haskmas.scad`, which you can feed into [OpenSCAD](http://www.openscad.org/) to get a rendering; from here you can export to STL.

I'm not using [ImplicitCAD](https://github.com/colah/ImplicitCAD)s STL output because it's a bit [inefficient](https://github.com/colah/ImplicitCAD/pull/67) at the moment.

- Print on a 3D printer
- Put it somewhere

![Haskmas tree decoration](on_tree.png)

# Xmonad
These files are linked back from the repository clone directory
to `ln -s <path>/dot-files/xmonad/<file> ~/.xmonad/`

There will also need to have pipes made in the home directory for the full effect:
* `mkfifo ~/marquee.pipe`
* `mkfifo ~/sun.pipe`

These pipes recieve sunrise sunset from a python script and the next [Equinox or Solstices date](https://www.roaringpenguin.com/wiki/index.php/Earth_Seasons) as of September 16, 2020 the [US Naval Observatory](http://aa.usno.navy.mil/seasons) is broken.

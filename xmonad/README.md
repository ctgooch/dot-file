# XMonad
## Initial setup
Currently I pull my clone down and then create my `~/.xmonad` thenn I link each file like so: `ln -s <path>/dot-files/xmonad/<file> ~/.xmonad/`.
My xmonad.hs makes some spawns to get the sunrise and sunset info in addition to the weather.  To accomplish this I create the following pipes in home::
* `mkfifo ~/marquee.pipe`
	* These pipes recieve sunrise sunset from a python script and the next [Equinox or Solstices date](https://www.roaringpenguin.com/wiki/index.php/Earth_Seasons) as of September 16, 2020 the [US Naval Observatory](http://aa.usno.navy.mil/seasons) is broken.
* `mkfifo ~/sun.pipe`
	* This pipe is created calling a python script with astral library see below 
	```python
	from astral import LocationInfo 
	import datetime
    import pytz
   from astral.sun import sun
   city = LocationInfo("<CITY>", "<STATE>", "<TIMEZONE>", <LATTITUDE>, <LONGITUDE>)
   day=datetime.date.today()
   s = sun(city.observer, date=day, tzinfo=city.timezone)
   sr=s["sunrise"]
   ss=s["sunset"]
  dw=s["dawn"]
  print(( \
        #'SR:{} - SS:{}'. format (sr.strftime('%H:%M'),ss.strftime('%H:%M'))))
        'DW:{},SR:{} - NOON{} - SS:{}, Dusk:{}'. format (dw.strftime('%H:%M'),sr.strftime('%H:%M'),s["noon"].strftime('%H:%M'),ss.strftime('%H:%M'),s["dusk"].strftime('%H:%M'))))


## Layouts and Alias
|Layout|Alias|Provided by|
|--------------------|-------------:|------------|
| ResizableTall|RTall| `XMonad.Layout.ResizableTile`|
|Tabbed Simplest|tabbs | `XMonad.Layout.Tabbed`|
|IM Grid| I M | `XMonad.Layout.IM` |
|Full|Full | default |
|Grid|grids| `XMonad.Layout.Grid` |
|TwoPane| v50 | `XMonad.Layout.TwoPane` |
|Mirror TwoPane| h50 | `XMonad.Layout.TwoPane` |

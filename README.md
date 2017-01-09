rastergrid
==========

This package contains a function I used time and time again in my phd (and a bit in my post-doc work too). `genGridUTM` simply creates a custom grid that can be used to crop raster data into manageable chunks, which is necessary when working with dense time series. I know there are much better ways to do this, but this has worked for me, so I'm throwing it up here for the benefit of anyone who wants to use it.

### Install it:
```R
library(devtools)
install_github('bendv/rastergrid')
library(rastergrid)
?genGridUTM
```


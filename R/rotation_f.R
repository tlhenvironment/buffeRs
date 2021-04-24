#shamelessly taken from Edzer Pebesma sf package vignette
#https://r-spatial.github.io/sf/articles/sf3.html

rotation_f = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

# tkPLUS: A tcltk Wrapper Wrapper

## To install if you have already installed the devtools package:

library(devtools)

install_github("Jake1Egelberg/tkPLUS")

## To install devtools:

install.packages("devtools")

## A general introduction
Package functions are prefaced with "tc," as in tcLabel or tcBuild. Create individual widgets with tc<Widget> functions and save them to a list() object. Then, use tcBuild() to build UIs from the widget list. tcApp can alternatively be used to create an external app from your list that can be launched via a .bat file.

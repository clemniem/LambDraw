# ffp-project: LambDraw!

Welcome to my final project in advanced functional programming of 2014/15.

LambDraw is a tool to provide G-Code for the LambDraw Plotter a selfbuild CoreXY-Drawing Machine.

# The steps:

1. Take an Image as Input
2. Reduce the Image in Size and Colors to a 4-Color Palette. 
3. Take each Color-Plane and produce G-Code
~~4. Send G-Code via USB to Motorcontroller (Arduino)~~ TODO
4. Save G-Code to .nc file and send via G-Code-Sender.


# Example:

I am using LambDraw to produce Gcode for my 4-Color-Plotter.
![LambDraw GUI](https://raw.githubusercontent.com/clemniem/LambDraw/master/doc/screenshot_lambgui.png)

The Machine
![LambDraw Machine](https://raw.githubusercontent.com/clemniem/LambDraw/master/doc/lambdraw_machine.jpg)

The Drawing
![LambDraw Output](https://raw.githubusercontent.com/clemniem/LambDraw/master/doc/lambdraw_output.jpg)

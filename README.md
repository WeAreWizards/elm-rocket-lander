# Rocket lander in Elm

The example was updated from previous versions of elm-compiler to work with 0.18.

To run:

```sh
elm-make ./src/Ship.elm --output ship.html
```

and then open ship.html in your browser.


## Notes to updates from previous versions
* Uses Kwarrtz/render to render svg instead of Graphics.Collage
* This renderer uses a different origin of the coordinate system
* Separated code into Files/Modules 

The map builder shows the same results as the Results Tab, but gives customisation options over how the map is plotted. Note that in this map, the 1-100 scores are always used as a basis. Click "Plot" to see the map.

#### Map controls

The side panel on the right gives controls over the map. The "Indicator to plot" dropdown lets you select any indicator or aggregate to plot on the map.

The "Number of colours" selector is used to define how many bins to divide the scores into. Setting this e.g. to three will make three boxes appear below, where you can define the respective colours to plot on the map for each bin. Note that:

- Bins are calculated as equal intervals between the maximum and minimum score
- Colours can be entered as valid hex codes (e.g. `#33A02C`), or [colour names recognised by R](http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf) (e.g. `red`).

Of the remaining controls:

- The "Opacity" input controls how opaque the overlay colours are.
- Line colour, weight and style control the lines separating regions. Colours are input in the same way as tile colours.
- The map base tiles can be changed to a number of available alternatives

#### Saving the map

The map can be saved to an image file using the "Download map" button. However, this feature may not work on all deployments and browsers.

The easiest and most flexible way to save the map as an image is to use a screen capture. On Windows hold the Windows key and press `SHIFT` + `S` to capture a selected area on the screen. Alternatively press the "print screen" button on your keyboard.

You should also be able to download the HTML file and save it for use in an interactive document. More details on this tab are in the [online documentation](https://unhcr-guatemala.github.io/A2SIT/book/map_builder.html)

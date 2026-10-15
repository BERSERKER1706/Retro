# RETRO LENS

A dark, editorial-style **photo filter studio** built as a single-file R Shiny app. Upload a photo, pick one of 21 film-inspired looks, dial the strength anywhere from an inverted effect to an exaggerated one, compare against the original, and download the result as a PNG.

Everything lives in one `app.R`. The flower imagery and the filter-thumbnail sample photos are embedded in the file, so there is no `www/` folder or other asset to carry around.

## Features

- **21 filters:** Original, Sepia, Vignette, Grain, Faded, Noir, Duotone, Vintage, Warm, Cool, Dramatic, Bright, Moody, Negative, Poster, Blur, Cyanotype, Sunset, Emboss, Sketch, Pixelate.
- **Density slider (-100% to +100%):** 0% is the filter's normal look, positive values push it further, negative values move toward the opposite effect. Typing a value in the number box works too.
- **Three view modes:** Filtered, Split (draggable divider) and Original. Press and hold the photo to peek at the original.
- **Photo tools:** rotate 90°, flip, "Surprise me" (random filter), reset, fullscreen.
- **Flexible loading:** drag and drop, paste from the clipboard, or click to browse (PNG or JPG).
- **Filter archive:** a bento-style grid of thumbnails, each rendered from a sample image with that filter, plus a small live preview that follows you down the page.
- **Landing page:** hero, numbered process steps, filter archive, statement and footer, in a dark Floria-inspired style (Inter + Playfair Display italic).
- **Download:** exports the filtered photo at full resolution as `retro_lens_output.png`. The in-browser preview is downscaled to 1400 px, but the download is not.

## Requirements

- R 4.1 or newer (the code uses the native pipe `|>`)
- R packages: `shiny`, `magick`, `base64enc`
- ImageMagick is bundled with the `magick` package on Windows and macOS. On Linux you may need the system library, e.g. `libmagick++-dev` (Debian/Ubuntu) or `ImageMagick-c++-devel` (Fedora).
- An internet connection in the browser to load the Google Fonts (Inter, Playfair Display and the others the page references). The app still works without it, with fallback fonts.

## Installation and running

```r
install.packages(c("shiny", "magick", "base64enc"))
```

Then, from the folder containing `app.R`:

```r
shiny::runApp("app.R")
```

or from a terminal:

```bash
Rscript -e 'shiny::runApp("app.R", launch.browser = TRUE)'
```

## How to use

1. Scroll to **The Studio** (or press the hero's Edit button) and upload a photo.
2. Choose a filter from the archive.
3. Set the density with the slider or the number box.
4. Compare with Filtered / Split / Original, rotate or flip if needed.
5. Click **Download PNG**.

Choosing a filter always resets density to 0%. Uploading a new photo resets the filter, density, rotation and flip.

## Keyboard shortcuts

Active once a photo is loaded and you are not typing in a field.

| Key | Action |
|---|---|
| ← / → | Previous / next filter (also moves the Split divider when it has focus) |
| ↑ / ↓ | Density ±5 (hold Shift for ±1) |
| C (hold) | Show the original |
| M | Cycle view mode |
| S | Surprise me |
| R | Reset everything |
| F | Fullscreen |

## How it works

- **`apply_*()` functions:** one per filter, built on `magick` (`image_modulate`, `image_colorize`, `image_contrast`, `image_noise`, `image_quantize`, and so on).
- **`apply_filter_by_name()`:** a single dispatcher used by both the main preview and the thumbnails, so they always match.
- **Density scaling:** `density_to_t()` maps the slider range to an internal strength `t`. Slider -50 lands exactly on the unfiltered photo, 0 on the filter's standard look, and +100 on double strength. Helpers `scale_mod`, `scale_amt` and `scale_contrast` scale each filter's parameters by `t`.
- **Vignette:** `magick` has no `image_vignette()`, so the effect is built from a radial mask composited with `multiply`.
- **Server state:** reactive values hold the current filter, density, rotation and flip. `base_img()` applies rotation and flip, then `filtered_img()` applies the filter. The same `filtered_img()` feeds the preview and the download.
- **Preview:** original and filtered images are sent to the browser as data URIs and layered, so switching modes and the Split divider are instant and need no server round trip.
- **Start-up:** the embedded images are decoded and written to a temporary folder served under `/art/`, and the 21 archive thumbnails are rendered once from the embedded sample photos.

## Project structure

```
app.R    # the whole app: helpers, filters, UI (HTML/CSS/JS), server, embedded images
```

The file is large (roughly 1 MB, ~9,000 lines) because the images are base64-encoded inside it. The filter logic and server are near the top and bottom; the bulk in between is UI markup, CSS and JavaScript.

## Customising

- **Add a filter:** write an `apply_yourfilter(img, t)` function, add its name to `filter_names`, add a case in `apply_filter_by_name()`, and give it a sample photo in the `sample_for` list so its thumbnail renders.
- **Change the look:** colours, fonts and spacing are set in the CSS inside the `ui` definition.

## Troubleshooting

- **`there is no package called 'magick'`:** run the install command above; on Linux install the ImageMagick dev libraries first.
- **Photo won't load:** only PNG and JPG are accepted.
- **Slow on huge photos:** filters run on the full-size image so the download is full quality. Very large photos take longer per change.

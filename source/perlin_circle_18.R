
# load packages -----------------------------------------------------------

library(tidyverse)
library(scales)
library(ambient)
library(here)
library(ggfx)

# typical helper functions ------------------------------------------------

sample_shades <- function(n) {
  sample(colours(distinct = FALSE), n)
}

blend_shades <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255, green = z[2, ]/255, blue = z[3, ]/255)
  return(z)
}

save_path <- function(sys_id, sys_version, seed, fmt = ".png") {
  sys_version <- sys_version %>% str_pad(width = 2, pad = "0")
  seed <- seed %>% str_pad(width = 3, pad = "0")
  base <- paste(sys_id, sys_version, seed, sep = "_")
  file <- paste0(base, fmt)
  path <- here("image", file)
  return(path)
}


# the thing I want to play with -------------------------------------------

# concept: Will Chase
# source:  https://twitter.com/W_R_Chase/status/1359251137111744526
# gist:    https://gist.github.com/djnavarro/a90265b0eed8dae9bad7052e7e3183d9

perlin_circle <- function(cx = 0, cy = 0, n = 100, noise_max = 0.5,
                          octaves = 2, r_min = 0.5, r_max = 1,
                          frequency = 1) {
  circ <- tibble(
    angle = seq(0, 2*pi, length.out = n),
    xoff = cos(angle) %>% rescale(from = c(-1, 1), to = c(0, noise_max)),
    yoff = sin(angle) %>% rescale(from = c(-1, 1), to = c(0, noise_max)),
    r = gen_simplex%>%
      fracture(
        fractal = fbm,
        x = xoff,
        y = yoff,
        octaves = octaves,
        frequency = frequency
      ) %>%
      rescale(from = c(-0.5, 0.5), to = c(r_min, r_max)),
    x = r * cos(angle) + cx,
    y = r * sin(angle) + cy
  )

  keep <- with(circ, chull(x, y))

  return(circ[keep,])
}




# generator function ------------------------------------------------------

generate_scene <- function(seed) {

  # plot parameters
  set.seed(seed)

  sys_id <- "perlincircle"
  sys_version <- 18

  xlim <- c(1, 10)
  ylim <- c(1, 10)

  prop_keep <- runif(1, min = .5, max = .6)
  n_colours <- 3 #sample(2:6, 1)

  # generate image data

  shades <- sample_shades(n_colours)
  bg <- "black"

  perlin_circle_l <- lift_dl(perlin_circle)

  n_grid <- 20
  dat <- expand_grid(
    cx = seq(1, 10, length.out = n_grid),
    cy = seq(1, 10, length.out = n_grid),
    r_min = .05,
    r_max = .15,
    n = 1000,
    frequency = 1,
    noise_max = .3,
    octaves = 6
  ) %>%
    mutate(r_max = r_max + octaves/30) %>%
    sample_frac(prop_keep) %>%
    transpose() %>%
    imap_dfr(~ perlin_circle_l(.x) %>% mutate(id = .y))

  threshold <- function(x, level = .1) {
    x * (x > level * max(x))
  }

  # specify plot
  np <- 20
  pic <- dat %>%
    ggplot(aes(x, y, group = id, fill = sample(id))) +
    as_reference(
      stat_density_2d(
        geom = "raster",
        data = tibble(x = rnorm(np, 5, 2), y = rnorm(np, 5, 2)),
        mapping = aes(x = x, y = y, alpha = threshold(after_stat(density))),
        contour = FALSE,
        show.legend = FALSE,
        inherit.aes = FALSE,
        n = 1000
      ),
      id = "density"
    ) +
    with_blend(
      geom_polygon(show.legend = FALSE),
      bg_layer = "density",
      blend_type = "copy_opacity"
    ) +
    theme_void() +
    theme(plot.background = element_rect(fill = bg)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_alpha_continuous(range = c(0, 1)) +
    scale_fill_gradientn(colours = shades) +
    scale_colour_gradientn(colours = shades) +
    coord_fixed(xlim = xlim, ylim = ylim) +
    NULL

  # write image

  ggsave(
    filename = save_path(sys_id, sys_version, seed),
    plot = pic,
    width = 10,
    height = 10,
    dpi = 300
  )

}


# generate images ---------------------------------------------------------

seeds <- 800:805
for(s in seeds) {
  cat("seed", s, "\n")
  generate_scene(s)
}


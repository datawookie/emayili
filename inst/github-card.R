# https://www.ddrive.no/post/making-hex-and-twittercard-with-bunny-and-magick/

library(magick)
# remotes::install_github("dmi3kno/bunny")
library(bunny)

hex <- image_read(here::here("man/figures/emayili-hex.png")) %>%
  image_scale("400x400")

gh_logo <- bunny::github %>% image_scale("50x50")

gh_card <- image_canvas_ghcard("#ffffff") %>%
  image_compose(hex, gravity = "East", offset = "+100+0") %>%
  image_annotate(
    "emayili: Email from R",
    gravity = "West",
    location = "+100-30",
    color = "#0d4448",
    size = 50,
    font = "Roboto Slab"
  ) %>%
  image_compose(gh_logo, gravity = "West", offset = "+100+45") %>%
  image_annotate(
    "datawookie/emayili",
    gravity = "West",
    location = "+160+45",
    size = 50,
    font = "Ubuntu Mono"
  ) %>%
  image_border_ghcard("#8b9196")

gh_card

gh_card %>%
  image_write(here::here("man/figures/emayili-github-card.png"))

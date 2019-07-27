library(hexSticker)
library(showtext)

# Load Google font.
#
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()

sticker(here::here("inst/hex/envelope-regular.png"),
        # Image
        s_x = 1,
        s_y = 1.10,
        s_width = 0.6,
        s_height = 0.6,
        # Package name
        package = "{emayili}",
        p_size = 16,
        p_y = 0.45,
        p_color = "#ffffff",
        p_family = "roboto_slab",
        # Hex
        h_fill = "#3498db",
        h_color = "#000000",
        # Spotlight
        spotlight = TRUE,
        l_y = 0.45,
        l_alpha = 0.4,
        # Output
        filename = here::here("man/figures/emayili-hex.png"),
        dpi = 300
)

library(tidyverse)
library(magick)


img_test <- magick::image_read("https://www.fnordware.com/superpng/pnggrad16rgb.png")

graph_ggplot <- ggplot(data = mtcars, mapping = aes(
    x = wt, y = mpg)) +
    annotation_custom(img, ymin = 30, ymax = 40, xmin = 4, xmax = 5) +
    geom_point() +
    labs(
        title = "This is the title",
        subtitle = "This is the subtitle"
    )
    

graph_ggplot

https://live.staticflickr.com/65535/51609533648_2d4e6b4c23_q.jpg

img <- grid::rasterGrob(img_test, interpolate = TRUE)


graph_ggplot

?image_colorize

?colorRampPalette




prot_colors <- c(
    `light_blue` = "#1aa2bc",
    `dark_blue` = "#175578",
    `darker_blue` = "#0f3e63",
    `violet` = "#591778",
    `light_red` = "#d74d29",
    `orange` = "#f68720",
    `dark_green` = "#557817",
    `light_green` = "#04a77e",
    `dark_grey` = "#454444",
    `light_grey` = "#7d7d7d",
    `yellow` = "#f2c241"
    )



prot_cols <- function(...) {
    cols <- c(...)
    if (is.null(cols))
        return (prot_colors)
    prot_colors[cols]
}


par(mfrow=c(1,2))
prot_cols() %>% scales::show_col()
prot_cols("light_red", "yellow") %>% scales::show_col()



ggplot(mtcars, aes(hp, mpg)) +
    geom_point(color = prot_cols("light_blue"),
               size = 4, alpha = .8)


prot_palettes <- list(
    `primary`  = prot_cols("light_blue", "dark_blue", "darker_blue", "dark_grey", "light_grey"),
    `secondary`  = prot_cols("dark_green", "light_green", "light_red", "orange", "violet"),
    `all`   = prot_cols("light_blue", "dark_blue", "darker_blue", "dark_grey", "light_grey",
                        "dark_green", "light_green", "light_red", "orange", "violet")
    )

prot_pal <- function(palette = "main", reverse = FALSE, ...) {
    pal <- prot_palettes[[palette]]
    if (reverse) pal <- rev(pal)
    grDevices::colorRampPalette(pal, ...)
    }


par(mfrow=c(1,2))

prot_pal("primary")(12) %>% scales::show_col()
prot_pal("secondary")(4) %>% scales::show_col()


scale_color_prot <- function(palette = "main",
                                   discrete = TRUE,
                                   reverse = FALSE,
                                   ...) {
    pal <- prot_pal(palette = palette, reverse = reverse)
    
    if (discrete) {
        discrete_scale("colour", paste0("prot_", palette), palette = pal, ...)
    } else {
        scale_color_gradientn(colours = pal(256), ...)
    }
}

scale_fill_prot <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
    pal <- prot_pal(palette = palette, reverse = reverse)
    
    if (discrete) {
        discrete_scale("fill", paste0("prot_", palette), palette = pal, ...)
    } else {
        scale_fill_gradientn(colours = pal(256), ...)
    }
}



ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
    geom_point(size = 4, alpha = .6) +
    scale_color_prot(discrete = FALSE, palette = "secondary")

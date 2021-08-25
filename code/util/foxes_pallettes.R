# extrafont::font_import(pattern="Lato")

foxes_palettes <- list(
  main = c("#2F9989", "#EE9C19", "#D1A7BF", "#BF2D44", "#0E6894"),
  light = c("#6EC2B6", "#F9AF2C", "#F8CCE1", "#E73249", "#1A8EBC"),
  dark = c("#247366", "#CB8216", "#9B7C8C", "#9A2236", "#083855"),
  extra = c("#149355", "#BA6C7F", "#DE6F29"),
  extra_light = c("#4EB167","#E4677D","#EE7E48"),
  extra_dark = c( "#0A5931","#9A485A","#8C441C"),
  extra_2_light = c("#4EB167","#F8CCE1","#EE7E48", "#E73249"),
  extra_3_light = c("#4EB167","#F8CCE1","#EE7E48"),
  extra_3 = c("#149355","#F8CCE1", "#D1A7BF", "#DE6F29")
)

foxes_strategy_gradient <- function(n){
  pal = foxes_palettes$light[c(2,3,5,1)]
  pal[2] <- foxes_palettes$extra_light[2]
  # pal <- rev(pal)
  out <- grDevices::colorRampPalette(pal)(n)
  structure(out, class = "palette")
}

foxes_strategy_gradient_2 <- function(n){
  pal = foxes_palettes$light[c(2,3,1)]
  pal[2] <- foxes_palettes$extra_light[2]
  # pal <- rev(pal)
  out <- grDevices::colorRampPalette(pal)(n)
  structure(out, class = "palette")
}

foxes_strategy_gradient_2 <- function(n){
  pal = c(foxes_palettes$dark[1], foxes_palettes$main[2], foxes_palettes$light[3])
  # pal <- rev(pal)
  out <- grDevices::colorRampPalette(pal)(n)
  structure(out, class = "palette")
}

#' A Wes Anderson palette generator
#'
#' These are a handful of color palettes from Wes Anderson movies.
#'
#' @param n Number of colors desired. Unfortunately most palettes now only
#'   have 4 or 5 colors. But hopefully we'll add more palettes soon. All color
#'   schemes are derived from the most excellent Tumblr blog:
#'   \href{http://wesandersonpalettes.tumblr.com/}{Wes Anderson Palettes}.
#'   If omitted, uses all colours.
#' @param name Name of desired palette. Choices are:
#'   \code{BottleRocket1}, \code{BottleRocket2},  \code{Rushmore1},
#'   \code{Royal1}, \code{Royal2},  \code{Zissou1}, \code{Darjeeling1},
#'   \code{Darjeeling2},  \code{Chevalier1} , \code{FantasticFox1} ,
#'   \code{Moonrise1}, \code{Moonrise2}, \code{Moonrise3}, \code{Cavalcanti1},
#'   \code{GrandBudapest1}, \code{GrandBudapest2}, \code{IsleofDogs1}, \code{IsleofDogs2}
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#' wes_palette("Royal1")
#' wes_palette("GrandBudapest1")
#' wes_palette("Cavalcanti1")
#' wes_palette("Cavalcanti1", 3)
#'
#' # If you need more colours than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colours
#' pal <- wes_palette(21, name = "Zissou1", type = "continuous")
#' image(volcano, col = pal)
foxes_palette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  
  pal <- foxes_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")
  
  if (missing(n)) {
    n <- length(pal)
  }
  
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}






### GGPLOT THEMES

scm_ribbon <- scale_color_manual("Allocation \nStrategy:",
                                 values=foxes_palettes$main[c(2, 1, 4, 3)],
                                 labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe"),
                                 guide = guide_legend(override.aes = list(fill=foxes_palettes$main[c(2, 1, 4, 3)],
                                                                          alpha = 0.5)))
scm <- scale_color_manual("Allocation \nStrategy:",
                          values=foxes_palettes$main[c(2, 1, 4, 3)],
                          labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe"))

sfm <- scale_fill_manual("Allocation \nStrategy:",
                          values=foxes_palettes$main[c(2, 1, 4, 3)],
                          labels=c("Slow-Risky","Slow-Safe","Fast-Risky","Fast-Safe"))

gr_ribbon <- geom_ribbon(alpha = 0.35, linetype = "blank")

foxes_theme <-       theme_bw() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white")) 

foxes_theme_min <-       theme_minimal() +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"),
        strip.background = element_rect(fill = "white")) 

species_wrap <- facet_wrap(.~species_id, nrow = 2, ncol = 2, labeller = labeller(species_id = c("1" = "Slow-Risky", 
                                                                                                "2" = "Slow-Safe",
                                                                                                "3" = "Fast-Risky", 
                                                                                                "4" = "Fast-Safe")))

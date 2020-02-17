###Colors
ab_colors <- c(
  `light blue 1`  = "#B1C2CF",
  `light blue 2`  = "#7CBBC7",
  `light blue 3`   = "#43B9CC",
  `light blue 4` = "#2096BA",
  `light blue 5`   = "#327591",
  `medium blue 1` = "#93A1AC",
  `medium blue 2`  = "#60919A",
  `medium blue 3`  = "#2C8F9F",
  `medium blue 4`  = "#21718A",
  `medium blue 5`   = "#305C6E",
  `light orange 1` = "#F7B58B",
  `light orange 2`   = "#FBA950",
  `light orange 3` = "#F28232",
  `light orange 4`  = "#DF6E21",
  `medium orange 1`  = "#CF8B61",
  `medium orange 2`  = "#DA8C37",
  `medium orange 3`   = "#CD671E",
  `medium orange 4` = "#B04F0D",
  `light purple 1`   = "#CE83A3",
  `light purple 2` = "#AC517F",
  `light purple 3`  = "#837FA7",
  `light purple 4`   = "#796391",
  `dark purple 1` = "#903F68",
  `dark purple 2`  = "#7F375B",
  `dark purple 3`   = "#5C5883",
  `dark purple 4` = "#4B3861"
)


ab_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (ab_colors)
  
  ab_colors[cols]
}

color_list = list( c("#2096BA","#DF6E21" ,"#FBA950", "#CE83A3","#7CBBC7", "#796391","#F7B58B","#B1C2CF"),c("#DA8C37","#2C8F9F","#AC517F","#43B9CC","#60919A","#4B3861","#B04F0D","#93A1AC"),c("#CF8B61","#7F375B","#327591","#837FA7","#CD671E")   )


ab_palettes <- list(
  `mix` = ab_cols("light blue 4","light orange 4","light orange 2","light purple 1" ),
  `light blue`  = ab_cols("light blue 1", "light blue 2", "light blue 3", "light blue 4", "light blue 5"),
  `Gender`  = ab_cols("light purple 1", "light purple 2"),
  `Income`  = ab_cols("medium blue 1", "medium blue 2"),
  `Settlement`  = ab_cols("dark purple 1", "dark purple 2"),
  `Education`  = ab_cols("medium orange 4","light orange 2", "light orange 3" ),

  `Age`  = ab_cols("light blue 4", "light blue 5"),
  
  `medium blue`  = ab_cols("medium blue 1", "medium blue 2", "medium blue 3", "medium blue 4", "medium blue 5"),
  
  `light orange`  = ab_cols("light orange 4", "light orange 3", "light orange 2", "light orange 1"),
  
  `medium orange`  = ab_cols("medium orange 1", "medium orange 2", "medium orange 3", "medium orange 4"),
  
  `light purple`  = ab_cols("light purple 1", "light purple 2", "light purple 3", "light purple 4"),
  
  `dark purple`  = ab_cols("dark purple 1", "dark purple 2", "dark purple 3", "dark purple 4"),
  
  `pie` = ab_cols("light blue 1", "dark purple 4","medium orange 2", "light purple 1","light blue 4","light orange 1"," dark purple 1",
                  
                  "medium orange 4","light purple 4","dark purple 2","light blue 1","medium blue 4","light orange 1","medium orange 3","light purple 2"),
  
  `test1` = color_list[[1]],
  `test2` = color_list[[2]],
  `test3` = color_list[[3]]
   
 
)




ab_pal <- function(palette = "light_orange", reverse = FALSE, ...) {
  pal <- ab_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_color_ab <- function(palette = "light orange", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ab_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("ab_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_ab <- function(palette = "light orange", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ab_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("ab_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

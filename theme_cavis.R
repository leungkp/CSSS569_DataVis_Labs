### ggplot2 theme written by Chris Adolph and Kai Ping (Brian) Leung
require(ggplot2)

## There are three themes in this file:
# 1. `theme_cavis`` (basic version with no gridlines at all)
# 2. `theme_cavis_hgrid`` (with horizontal/y-axis gridlines; for scatterplots, lineplots, etc.)
# 3. `theme_cavis_vgrid` (with vertical/x-axis gridlines; for ropeladder)

# Version 1: basic version (with no gridlines at all)
theme_cavis <- theme(
  ## Removes main plot gray background
  panel.background = element_rect(fill = "white"), 
  
  ## Golden rectangle plotting area (leave out for square)
  aspect.ratio = ((1 + sqrt(5))/2)^(-1), 
  
  ## All axes changes
  axis.ticks.length = unit(0.5, "char"),  # longer ticks
  
  ## Horizontal axis changes
  axis.line.x.top = element_line(size = 0.2),    # thinner axis lines
  axis.line.x.bottom = element_line(size = 0.2), # thinner axis lines
  axis.ticks.x = element_line(size = 0.2),       # thinner ticks
  axis.text.x = element_text(color = "black", size = 12),
  
  ## match type of axis labels and titles
  axis.title.x.top = element_text(size = 12,
                                  margin = margin(t = 0, r = 0, b = 7.5, l = 0)),
  
  ## match type; pad space between title and labels
  axis.title.x.bottom = element_text(size = 12,
                                     margin = margin(t = 7.5, r = 0, b = 0, l = 0)),
  
  ## Vertical axis changes
  axis.ticks.y = element_blank(), # no y axis ticks (gridlines suffice)
  axis.text.y = element_text(color = "black", size = 12,
                             margin = margin(t = 0, r = -4, b = 0, l = 0)),
  
  ## match type of axis labels and titles, pad
  axis.title.y = element_text(size = 12,
                              margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
  
  ## Legend
  legend.key = element_rect(fill = NA, color = NA),
  legend.background = element_blank(),
  legend.title = element_blank(),
  ## Remove unhelpful gray background
  
  ## Gridlines 
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(), 
  
  ## Faceting (small multiples)
  strip.background = element_blank(),
  
  ## Remove unhelpful trellis-like shading of titles
  strip.text.x = element_text(size=12),  # Larger facet titles
  strip.text.y = element_text(size=12, angle = 0),  
  strip.placement = "outside",           # Place titles outside plot
  panel.spacing.x = unit(1.25, "lines"), # Horizontal space b/w plots
  panel.spacing.y = unit(1, "lines")     # Vertical space b/w plots
)

# Version 2: with major horizontal gridlines 
theme_cavis_hgrid <- 
  theme_cavis + 
  theme(panel.grid.major.y = element_line(color = "gray75", size = 0.1))

# Version 3: with major vertical gridlines (for ropeladder)
theme_cavis_vgrid <- 
  theme_cavis + 
  theme(panel.grid.major.x = element_line(color = "gray75", size = 0.1))




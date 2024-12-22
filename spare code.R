# set chord chart parameters

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# set custom color palette

mycolor <- viridis(6, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:6)]

#set variable to order the sectors in the most visually pleasing way

sector_order2 <- c("Pop", "Hip-Hop", "Rock/Metal", "Soul/R&B", "Country","Electronic")

#generate chord diagram

collab_chords <- chordDiagram(
  x = adj_matrix_long,
  grid.col = mycolor,
  order = included_genres,
  transparency = 0.25,
  directional = 1,  
  direction.type = c("arrows", "diffHeight"),
  diffHeight  = -0.02,
  annotationTrack = "grid",
  link.target.prop = TRUE,
  annotationTrackHeight = c(0.03, 0.02),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  self.link = 1,
  symmetric = TRUE)

#customise labels and axes

circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    
    circos.axis(
      h = "top",  # Place axis at the top of the track
      major.at = seq(xlim[1], xlim[2], length.out = 5),  # Create 5 evenly spaced ticks
      labels = round(seq(0, 1, length.out = 5), 2),  # Labels as proportions (0-1)
      sector.index = sector.index,
      labels.cex = 0.6,  # Reduce label size
      minor.ticks = 1
    )
  }
)

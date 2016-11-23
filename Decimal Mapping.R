library(ggplot2)
library(RColorBrewer)

setwd()

digitplot <- function(sourcefile, light, dark) {
  # Select number of digits to plot
  digits <- 10000
  # Read the digits from the filename given
  a <- read.csv(sourcefile, header=F)
  # The csv is parsed as a dataframe, so we select the first records only as a character string
  # strsplit() splits it into individual characters
  b <- strsplit(as.character(a[1,1]),NULL)
  # We select all characters except the whitespaces
  c <- b[[1]][b[[1]] != " "]
  # Do not select the first two characters, those come before the decimal dot
  d <- as.numeric(c[3:(digits+2)])
  d <- d[!is.na(d)]
  # Transformation constant by which the digit is multiplied
  m <- pi/180*36*-1
  # Transformation constant which rotates the graph
  n <- 0.5*pi
  # Initialize empty dataframe
  coord <- data.frame(id = c(1), x = c(0), y = c(0))
  # Iteratively fill the dataframe with coordinates
  for(i in 1:min(length(d),digits)) {
    coord[i+1,] <- c(i, coord[i,2] + cos(d[i]*m+n), coord[i,3] + sin(d[i]*m+n))
  }
  # Determine drawing sizes
  coord$x <- coord$x - min(coord$x)
  coord$y <- coord$y - min(coord$y)
  # Initialize colours
  colfunc <- colorRampPalette(c(light, dark))
  # Create the plot
  plot <- ggplot() + 
    geom_path(data = coord, aes(x=x, y=y, colour=id), size=0.15) + #Path
    geom_point(data = coord, aes(x=x, y=y, colour=id), shape=18, size=0.5) + #Dots
    geom_point(data = coord[1,], aes(x=x, y=y), colour=dark, shape=16, size=4) + #Coloured ring of starting circle
    geom_point(data = coord[1,], aes(x=x, y=y), colour="white", shape=16, size=2.25) + #White center of starting circle
    scale_colour_gradientn(colours=colfunc(nrow(coord))) + #Map the gradient colours
    xlim(c(0,185)) + ylim(c(0,185)) + #Set graph area
    coord_fixed(ratio=1) + #Equal scale of x and y axes
    theme(legend.position="none", #Disable every part of the graph window
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="black"), #Set the background to black
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
  #Return the plot
  return(plot)
}

phiplot <- digitplot("Phi 10000.csv","#FFEEEE","firebrick4") #FBF3C6
piplot <- digitplot("Pi 10000.csv","#D2F0FF","#005F8C")
eplot <- digitplot("e 10000.csv","white","plum2","purple4")
iiplot <- digitplot("ii 10000.csv","#D5FBFF","#0094A8")
sqrtplot <- digitplot("sqrt2 10000.csv","#FFFFDD","orange2")
lnplot <- digitplot("ln2 10000.csv","#F0FFD2","#5F8C00")

ggplot(as.data.frame(table(d)), aes(x = factor(d), y = Freq, fill=Freq)) +
    geom_bar(width = 1,stat="identity", alpha=.7) + coord_polar(start=-pi/10) +
    geom_hline(yintercept=1000, colour="white", size=1) +
    theme_bw() + 
    theme(panel.background=element_rect(fill="#333333"), panel.grid.major.x=element_blank(), axis.text.x=element_blank()) + 
    geom_text(aes(label=d, y = 830), alpha = 1, size=6, colour="white")

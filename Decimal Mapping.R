library(ggplot2)

# Select number of digits to plot
digits <- 10000

# Choose one of the source files containing the digits
a <- read.csv("Phi 10000.csv", header=F)
a <- read.csv("Pi 10000.csv", header=F)
a <- read.csv("e 10000.csv", header=F)
a <- read.csv("ii 10000.csv", header=F)

# It reads the csv as a dataframe, so we select the first records only as a character string
# strsplit() splits it into individual characters
b <- strsplit(as.character(a[1,1]),NULL)
# We select all characters except the whitespaces
c <- b[[1]][b[[1]] != " "]
# Do not select the first two characters, those come before the decimal dot
d <- as.numeric(c[3:(digits+2)])

# Transformation constant by which the digit is multiplied
m <- pi/180*36*-1
# Transformation constant which rotates the graph
n <- 0.5*pi

# Initialize empty dataframe
coord <- data.frame(id = c(1), x = c(0), y = c(0))
# Iteratively fill the dataframe with coordinates
for(i in 2:digits) {
  coord[i,] <- c(i, coord[i-1,2] + cos(d[i]*m+n), coord[i-1,3] + sin(d[i]*m+n))
}

# Draw plot
plot <- ggplot() + 
  geom_path(data = coord, aes(x=x, y=y, colour=id), size=0.5) +
  geom_point(data = coord, aes(x=x, y=y, colour=id), shape=18, size=1) +
  geom_point(data = coord[1,], aes(x=x, y=y), colour="#0000FF", shape=16, size=5) +
  geom_point(data = coord[1,], aes(x=x, y=y), colour="#FFFFFF", shape=16, size=3) +
  scale_colour_continuous(low="#FFFFFF", high="#00FFFF") +
  theme(legend.position="none",
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

print(plot)

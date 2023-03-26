# Write a program that draws a starry sky. In the picture, display stars up to 5th magnitude in the 
# Kavrayskiy projection. The sizes of the images of stars should be chosen approximately proportional 
# to their brightness.
# Add the ecliptic and the boundaries of the constellation Aquarius to the drawing. In the title, 
# indicate the projection and constellation. Optional: color the stars according to their spectral type.

library(plotly)
library(colorspace)

stars <- read.table("stars.dat", header = T, sep = "|", comment.char = "%")  # header = T         -- first line of 'stars.dat' contains the names of the variables
                                                                             # sep = "|"          -- the field separator character
                                                                             # comment.char = "%" -- symbol of a comment
stars <- stars[stars[,"Mag.V"] < 5,]   # Leave in 'stars' only those stars whose apparent magnitude is less than 5.

# Create a variable to store some data.
# The first 3 places will be reserved for Right Ascension (hours, minutes, seconds).
# The last 3 places will be assigned to Declination (degrees, arc minutes, arc seconds).
coordinates_of_stars <- unlist(strsplit(stars$coord1..ICRS.J2000.2000., " "))   # Fill this variable with coordinates from the column 'coord1..ICRS.J2000.2000.' of the 'stars'.
                                                                                # strsplit(...) -- split the elements of the vector i, separated by a space (" "), into a list
                                                                                # unlist(...)   -- transform the list back into a vector
coordinates_of_stars <- coordinates_of_stars[coordinates_of_stars != ""]        # Remove from the vector elements consisting of empty strings.
coordinates_of_stars <- matrix(coordinates_of_stars, ncol = 6, byrow = T)    # Transform the vector into a matrix of coordinates.

RA <- ((as.numeric(coordinates_of_stars[,1]) + (as.numeric(coordinates_of_stars[,2]))/60 + (as.numeric(coordinates_of_stars[,3]))/3600) * pi / 12) - pi   # Right Ascension (RA) in radians (it's equivalent to earth longitude (lambda))
Dec <- (as.numeric(coordinates_of_stars[,4]) + (as.numeric(coordinates_of_stars[,5]))/60 + (as.numeric(coordinates_of_stars[,6]))/3600) * pi / 180        # Declination (Dec) in radians (it's equivalent to earth latitude (phi))
new_coordinates <- matrix(c(3 / 2 * RA * sqrt((1 / 3) - (Dec / pi) ^ 2), Dec), ncol = 2)                                                                  # Create the matrix with new coordinates (x, y) for the Kavrayskiy projection.

colour <- substr(stars[,"spec..type"], 1, 1)   # Indicate the colors of the stars in accordance with their spectral type.
otherwise <- colour != "O" & colour != "B" & colour != "A" & colour != "F" & colour != "G" & colour != "K" & colour != "M" # FIXXX
colour[colour == "O"] <- 'rgb(111, 158, 240)'  # bluish
colour[colour == "B"] <- 'rgb(161, 197, 240)'  # blue-white
colour[colour == "A"] <- 'rgb(255, 255, 255)'  # white
colour[colour == "F"] <- 'rgb(245, 234, 135)'  # yellowish white
colour[colour == "G"] <- 'rgb(250, 239, 43)'   # yellowish
colour[colour == "K"] <- 'rgb(245, 139, 0)'    # orange
colour[colour == "M"] <- 'rgb(245, 41, 33)'    # reddish
colour[otherwise] <- 'rgb(238, 119, 250)'      # pinkish

# The coordinates of the constellation boundaries in the Kavrayskiy projection:
AQR <- read.table("AQR.dat", sep = " ")

alpha <- (as.numeric(AQR[,1]) * pi / 12) - pi   # Right Ascension (alpha) in radians (for constellation boundaries)
delta <- as.numeric(AQR[,2]) * pi / 180         # Declination (delta) in radians (for constellation boundaries)
AQR <- matrix(c(3 / 2 * alpha * sqrt((1 / 3) - (delta / pi) ^ 2), delta), ncol = 2)

# Ecliptic coordinates:
delta_ecl <- asin(sin(23.5 * pi / 180) * sin(c(1:180) * pi / 180))
alpha_ecl <- (acos(cos(c(1:180) * pi / 180) / cos(delta_ecl))) - pi 

delta_ecl <- c(delta_ecl, asin(sin(23.5 * pi / 180) * sin(c(360:181) * pi / 180)))
alpha_ecl <- c(alpha_ecl, acos(cos(c(360:181) * pi / 180) / cos(delta_ecl[181:360])))

ecliptic <- matrix(c(3 / 2 * alpha_ecl * sqrt((1 / 3) - (delta_ecl / pi) ^ 2), delta_ecl), ncol = 2)

star_size <- as.numeric(stars[,"Mag.V"])       # Indicate the brightness of the stars for further size selection.
star_size <- exp(-star_size/6)*2.5

coord_df <- as.data.frame(new_coordinates)
bound_df <- as.data.frame(AQR)
eclip_df <- as.data.frame(ecliptic)

fig <- plot_ly(data = coord_df, x = -coord_df$V1, y = coord_df$V2,type = 'scatter',
               mode = 'markers', marker = list(size = star_size,color = colour,line = list(width = 0)),
               name = 'stars')
fig <- fig %>% layout(title = 'Kavrayskiy projection, Aquarius. Kobozeva V.A.',
                      yaxis = list(zeroline = F, showgrid = F),
                      xaxis = list(zeroline = F, showgrid = F),
                      showlegend = F,
                      plot_bgcolor = 'rgb(0, 0, 51)')

fig <- fig %>% add_trace(data = bound_df, x = -bound_df$V1, y = bound_df$V2,
                         mode = 'lines', color = I('white'),line = list(width = 0.5),
                         marker = list(size = 0.5), name = 'AQR boundaries')
fig <- fig %>% add_trace(data = eclip_df, x = -eclip_df$V1, y = eclip_df$V2, 
                         mode = 'lines', color = I('red'),line = list(width = 0.5),
                         marker = list(size = 0.5), name = 'ecliptic')

fig

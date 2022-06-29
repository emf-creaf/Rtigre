ifn <- read.csv("Peus_majors3.csv", sep=";",dec=".",header=T)

# Pinus uncinata = 22.
ifn <- subset(ifn, IDESPECIE == 22)

# Maximum DBH for Pinus sylvestris (source: wikipedia English).
max_y <- 170

# Select variables.
x <- ifn$DN_IFN2
y <- ifn$DN_IFN3
prec <- ifn$P_ANUAL
temp <- ifn$TM_MAMJJA

# Filter out bad rows.
i <- which(!is.na(x) & !is.na(y) & !is.na(temp) & !is.na(prec))
x <- x[i]
y <- y[i]
prec <- prec[i]
temp <- temp[i]

# Only trees that do grow.
i <- which(x>0 & y>0 & y>x)
x <- x[i]
y <- y[i]
prec <- prec[i]
temp <- temp[i]

# Select sample.
set.seed(42)
i <- sample(length(x), 5000)
x <- x[i]
y <- y[i]
prec <- prec[i]
temp <- temp[i]

# Stand radii: 5, 10, 15 and 25 m.
# Trees larger than: 7.5, 12.5, 22.5 and 42.5
# Trees at certain DBH show biases. Eliminate them.
i <- which(abs(x-12.5)>.2 & abs(x-22.5)>.2 & abs(x-42.5)>.2)
x <- x[i]
y <- y[i]
prec <- prec[i]
temp <- temp[i]

plot(x,y-x,xlim=c(0,max(c(x,y))))
plot(prec,y-x,xlim=c(0,max(prec)),log="y")
plot(temp,y-x,xlim=c(0,max(temp)),log="y")

# Make data.frame.
Punci_IFN <- data.frame(y1 = x, y2 = y, temp = temp, prec = prec, max_y = max_dbh)

# Save in the correct format.
usethis::use_data(Punci_IFN, overwrite = T)



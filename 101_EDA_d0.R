library(tidyverse)


setwd("C:/Users/Ocanamat/Desktop/MercedesKaggle/MercedesKaggle")

train_data <- read_csv('../train.csv')

train_data <- train_data[!(train_data$y == 265.32),]

train_data$ID_v2 <- 1:nrow(train_data)
train_data$featSum <- rowSums(train_data[, 11:378])
train_data$y_normalized <- scale(train_data$y)
train_data$y_binned <- cut(train_data$y, breaks = seq(min(train_data$y), max(train_data$y), length.out = 50) , labels = FALSE)
#train_data$y_normColor <-

cor(rowSums(train_data[, 11:378]), train_data$y)
cor(train_data$ID_v2, train_data$y)

image(as.matrix(train_data[, 11:378]))

length(unique(train_data$y))

hist(train_data$y)
plot(density(train_data$y))
plot(density(train_data$featSum))

modValues <- c(2,3,4,5,7,9,10,13,15,20,24,25,30,31,45,60,90,365)

for (i in modValues) {
    train_data[, ncol(train_data) + 1] <- factor(train_data$ID %% i)
    names(train_data)[ncol(train_data)] <- paste0("modulo_ID.", i)
}

for (i in modValues) {
    train_data[, ncol(train_data) + 1] <- factor(train_data$ID_v2 %% i)
    names(train_data)[ncol(train_data)] <- paste0("modulo_IDv2.", i)
}

for (i in modValues) {
    train_data[, ncol(train_data) + 1] <- factor(train_data$featSum %% i)
    names(train_data)[ncol(train_data)] <- paste0("modulo_featSum.", i)
}


qplot(train_data$ID, train_data$featSum)
i<-5
for (i in modValues) {
    png(paste0("../featSum_moduloID_", i, ".png"), width = 1200, height = 900, res = 100)
    colorVar <- paste0("modulo_ID.", i)
    print(ggplot(train_data, aes_string(x = "ID", y = "featSum", size = 'y', colour = colorVar)) +
    geom_point(alpha = .5)) #+
    #scale_colour_brewer(palette = "Set1"))
    dev.off()
}

for (i in modValues) {
    png(paste0("../featSum_moduloIDv2_", i, ".png"), width = 1200, height = 900, res = 100)
    colorVar <- paste0("modulo_IDv2.", i)
    print(ggplot(train_data, aes_string(x = "ID", y = "featSum", size = 'y', colour = colorVar)) +
    geom_point(alpha = .5)) #+
    #scale_colour_brewer(palette = "Set1"))
    dev.off()
}

for (i in modValues) {
    png(paste0("../densityPlot_y_moduloIDv2_", i, ".png"), width = 1200, height = 900, res = 100)
    colorVar <- paste0("modulo_IDv2.", i)

    print(ggplot(data = train_data, aes_string(x = 'y', color = colorVar)) + geom_density())
    
    dev.off()
}
i <- 30
j <- 6
for (i in modValues) {
    moduloVar <- paste0("modulo_ID.", i)

    setwd("C:/Users/Ocanamat/Desktop/MercedesKaggle/MercedesKaggle")
    pathDir <- dir.create(paste0("../pixelImage_moduloID_", i), showWarnings = FALSE)
    setwd(paste0("../pixelImage_moduloID_", i))

    for (j in as.integer(unlist(unique(train_data[moduloVar])))) {
        #cat(j)
        png(paste0("pixelPlot_y_moduloID_", i,"_res_", j,".png"), width = 1200, height = 900, res = 100)

        image_data <- train_data[train_data[moduloVar] == j,]

        if (nrow(image_data) != 0) {
        imageMatrix <- sweep(as.matrix(image_data[, 11:378]), MARGIN = 1, image_data$y_binned, `*`)
        image(t(imageMatrix), col = customColorPal)
        }

        dev.off()


    }
 
}
setwd("C:/Users/Ocanamat/Desktop/MercedesKaggle/MercedesKaggle")

image_data <- subset(train_data, modulo_ID.10 == 3)
imageMatrix <- sweep(as.matrix(image_data[, 11:378]), MARGIN = 1, image_data$y_binned, `*`)
image(t(imageMatrix), col = customColorPal)
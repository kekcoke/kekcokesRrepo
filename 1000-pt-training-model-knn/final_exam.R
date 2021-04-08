#1. Generate 1000 X-Y coordinates
x <- runif(1000, 0, 1)
y <- runif(1000, 0, 1)


library(reshape2)


#Initialize dataframe
coords <- data.frame(x=x, y=y, quadrant = 0)


#2. Function to assign quadrant per coordinate
assign_Quad = function(x,y, n) {
  len <- length(x)
  quadrants <- c(1:n)
  for (i in 1:n) {
    ifelse (x[i] <= 0.5 & y[i] <= 0.5, quadrants[i] <- 'C',
            ifelse (x[i] <= 0.5 & y[i] > 0.5, quadrants[i] <- 'A',
                    ifelse(x[i] > 0.5 & y[i]> 0.5, quadrants[i] <- 'B',
                           quadrants[i] <- 'D' )
            )
    )
  }
  coords <- data.frame(x=x, y=y, quadrant = quadrants)
  return (coords)
}


#1000 coordinates with PROPER QUADRANTS
cwithq1000 <- assign_Quad(x, y, 1000)

#Set up function to distort data. FOr a specific quadrant, assign any but 'that'
fake_quad = function(x,y, n) {
  len <- length(x)
  quadrants <- c(1:n)
  for (i in 1:n) {
    ifelse (x[i] <= 0.5 & y[i] <= 0.5, quadrants[i] <- sample(c('A','B','D'), 1, replace = TRUE, prob=NULL),
            ifelse (x[i] <= 0.5 & y[i] > 0.5, quadrants[i] <- sample(c('B','C','D'), 1, replace = TRUE, prob=NULL),
                    ifelse(x[i] > 0.5 & y[i]> 0.5, quadrants[i] <- sample(c('A','C','D'), 1, replace = TRUE, prob=NULL),
                           quadrants[i] <- sample(c('A','B','C'), 1, replace = TRUE, prob=NULL) )
            )
    )
  }
  coords <- data.frame(x=x, y=y, quadrant = quadrants)
  return (coords)
}


#Create 50 NOISE coordinates 
x1 <- runif(200, 0, 1)
y1 <- runif(200, 0, 1)
coords1_3 <- fake_quad(x1, y1, 200)


#Combine 1000 pairs with 200 noise data
coords1200 <- rbind(cwithq1000, coords1_3)


#5. Create 100 random distributed test data points 
x2 <- runif(100, 0, 1)
y2 <- runif(100, 0, 1)

testData100 <- assign_Quad(x2, y2, 100)

#combine 1200+100 (testData) rows as dMod1300
dMod1300 <- rbind(coords1200, testData100)

#View full dataset
str(dMod1300)


#Scale full 1300 rows of dataset
normalize = function(x) {
  return ((x - min(x))/ (max(x) - min(x)))
}

#Check it normalizes
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

ndMod1300 <- as.data.frame(lapply(dMod1300[,c(1,2)], normalize))

# confirm that normalization worked
summary(ndMod1300$x)
summary(ndMod1300$y)


#Create test (1:1200,) and train (1201:1300,) data
ktrain <-ndMod1300[1:1200,]
ktest <- ndMod1300[1201:1300,]

#and labels for training and test data 
ktrainl <-dMod1300[1:1200, 3]
ktestl <- dMod1300[1201:1300, 3]


#load knn-containing package
library(class)
#6 first attempt                                 
kpred1 <- knn(train = ktrain, test = ktest, cl= ktrainl, k=35)
#Evaluate model performance
library(gmodels)
#Create cross-table on pred. vs actual
CrossTable(x = ktestl, y = kpred1, prop.chisq = FALSE)



#Second Predictive Model                                 
kpred2 <- knn(train = ktrain, test = ktest, cl= ktrainl, k=15)
CrossTable(x = ktestl, y = kpred2, prop.chisq = FALSE)

#Third Predictive Model
kpred3 <- knn(train = ktrain, test = ktest, cl= ktrainl, k=25)
CrossTable(x = ktestl, y = kpred3, prop.chisq = FALSE)

#Fourth Predictive Model
kpred4 <- knn(train = ktrain, test = ktest, cl= ktrainl, k=30)
CrossTable(x = ktestl, y = kpred4, prop.chisq = FALSE)

#Fifth Predictive Model
kpred5 <- knn(train = ktrain, test = ktest, cl= ktrainl, k=50)
CrossTable(x = ktestl, y = kpred5, prop.chisq = FALSE)



#Fifth Predictive Model
kpred6 <- knn(train = ktrain, test = ktest, cl= ktrainl, k=70)
CrossTable(x = ktestl, y = kpred6, prop.chisq = FALSE)


#Plot pre- and post-normalized coordinate datasets
library(ggplot2)
ggplot(data=coords, aes_string(x=x, y=y), main="1000 Coordinate Plot") + geom_point(size=0.5, shape=5)
ggplot(data=coords1200, aes_string(x=coords1200$x, y=coords1200$y), main="1200 Coordinates Plot") + geom_point(size=0.5, shape=5)

ggplot(data=testData100, aes_string(x=testData100$x, y=testData100$y), main="Test Data 100-coordinate Plot") + geom_point(size=0.5, shape=5)
ggplot(data=dMod1300, aes_string(x=dMod1300$x, y=dMod1300$y), main="Non-Normalized Coordinate Plot") + geom_point(size=0.5, shape=5)
ggplot(data=ndMod1300, aes_string(x=ndMod1300$x, y=ndMod1300$y), main="Normalized 1300 Coordinate Plot") + geom_point(size=0.5, shape=5)


#Using as scale to improve perforance (altnernative)
zk <- as.data.frame(scale(dMod1300[-3]))

ztrain <- zk[1:1200,]
ztest <- zk[1201:1300,]


zkpred <- knn(train = ztrain, test = ztest, cl = ktrainl, k = 35)
CrossTable(x = ktestl, y = zkpred, prop.chisq = FALSE)

zkpred2 <- knn(train = ztrain, test = ztest, cl = ktrainl, k = 10)
CrossTable(x = ktestl, y = zkpred2, prop.chisq = FALSE)

zkpred3 <- knn(train = ztrain, test = ztest, cl = ktrainl, k = 70)
CrossTable(x = ktestl, y = zkpred3, prop.chisq = FALSE)



summary(zk$x)
summary(zk$y)



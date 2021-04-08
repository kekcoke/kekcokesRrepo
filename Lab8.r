#Load .csv and set sAF to false.
  #educ <- read.csv('C:\\Users\\Kevin Oane\\Documents\\DB2Lab\\students-academic-performance-dataset\\xAPI-Edu-Data.csv', header=TRUE, stringsAsFactors = FALSE)

educ <- read.csv('C:\\Users\\Kevin Oane\\Documents\\DB2Lab\\students-academic-performance-dataset\\xAPI-Edu-Data.csv', header=TRUE, stringsAsFactors = TRUE)

#Check Datasets
str(educ)

#convert all columns to numeric
numeric_educ <- sapply(educ, as.numeric)
    #stringsAsFactors = FALSE causes NA coerciion 
    #Nope

numeric_educ <- data.frame (lapply(educ, as.numeric))

str(numeric_educ)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
educ_norm <- as.data.frame(lapply(numeric_educ, normalize))


summary(numeric_educ$Class)

summary(educ_norm$Class)

# create training and test data
educ_train <- educ_norm[1:336, ]
educ_test <- educ_norm[337:480, ]

library(neuralnet)

#first model, hidden = 0
educ_model <- neuralnet(formula = 
                        Class ~ gender + NationalITy +
                          PlaceofBirth + StageID + GradeID + 
                          SectionID + Topic + Semester +
                          Relation + raisedhands + VisITedResources +
                          AnnouncementsView + Discussion + 
                          ParentAnsweringSurvey +
                          ParentschoolSatisfaction + StudentAbsenceDays,
                          data = educ_train, hidden = 0
                        )
#plot
plot(educ_model)

#check performance
model1_result <- compute(educ_model, educ_test[1:16])

#get predicted education performance
pred_edPef1 <- model1_result$net.result

#check predicted-actual correlation
cor(pred_edPef1, educ_test$Class)





#second model, hidden = 5
educ_model2 <- neuralnet(formula = 
                          Class ~ gender + NationalITy +
                          PlaceofBirth + StageID + GradeID + 
                          SectionID + Topic + Semester +
                          Relation + raisedhands + VisITedResources +
                          AnnouncementsView + Discussion + 
                          ParentAnsweringSurvey +
                          ParentschoolSatisfaction + StudentAbsenceDays,
                        data = educ_train, hidden = c(5,4,3,2,1)
)

plot(educ_model2)

#check performance
model2_result <- compute(educ_model2, educ_test[1:16])

#get predicted education performance
pred_edPef2 <- model2_result$net.result

#check predicted-actual correlation
cor(pred_edPef2, educ_test$Class)



#third model, hidden = 10
educ_model3 <- neuralnet(formula = 
                           Class ~ gender + NationalITy +
                           PlaceofBirth + StageID + GradeID + 
                           SectionID + Topic + Semester +
                           Relation + raisedhands + VisITedResources +
                           AnnouncementsView + Discussion + 
                           ParentAnsweringSurvey +
                           ParentschoolSatisfaction + StudentAbsenceDays,
                         data = educ_train, hidden = c(10,9,8,7,6,5,4,3,2,1)
)

plot(educ_model3)
#check performance
model3_result <- compute(educ_model3, educ_test[1:16])

#get predicted education performance
pred_edPef3 <- model3_result$net.result

#check predicted-actual correlation
cor(pred_edPef3, educ_test$Class)

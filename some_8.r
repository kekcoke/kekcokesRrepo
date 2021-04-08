

#import EXCEL file 'Import Dataset' in the Environment tab to your right, convert gender to numeric
str(educSVMNorm)
educSVMNorm$Class <- as.factor(educSVMNorm$Class)
str(educSVMNorm)
ksvm_educ_train <- educSVMNorm[1:336,]
ksvm_educ_test <- educSVMNorm[336:480,]


#LINEAR
linear_classifier <- ksvm(Class ~., data= ksvm_educ_train ,
                          kernel = "vanilladot")
#linear_classifier

#check model performance
linear_predictions <-predict(linear_classifier, ksvm_educ_test)
linear_predictions
table(linear_predictions, ksvm_educ_test$Class)

head(linear_predictions)

agreement <- linear_predictions  == ksvm_educ_test$Class
table(agreement)
prop.table(table(agreement))

#RBF
rbf_classifier <- ksvm(Class ~., data= ksvm_educ_train,
                       kernel = "rbfdot")
rbf_classifier
#check rbf model performance
rbf_predictions <- predict(rbf_classifier, ksvm_educ_test)
table(rbf_predictions,ksvm_educ_test$Class)
#look vs agreement vs non-agreement
#product boolean vector for predicting class
agreement_rbf <- rbf_predictions == ksvm_educ_test$Class
View(table(agreement_rbf))
prop.table(table(agreement_rbf))


#POLY
poly_classifier <- ksvm(Class ~., data= ksvm_educ_train,
                        kernel = "polydot")
poly_classifier
#check poly model performance
poly_predictions <- predict(poly_classifier, ksvm_educ_test)
table(poly_predictions, ksvm_educ_test$Class)
#look vs agreement vs non-agreement
#product boolean vector for predicting class
agreement_poly <- poly_predictions == ksvm_educ_test$Class
View(table(agreement_poly))
prop.table(table(agreement_poly))

#ANOVA
anovadot_classifier <- ksvm(Class ~., data= ksvm_educ_train,
                            kernel = "anovadot")
anovadot_classifier
#check anovadot model performance
anovadot_predictions <- predict(anovadot_classifier, ksvm_educ_test)
#look vs agreement vs non-agreement
#product boolean vector for predicting class
agreement_anovadot <- anovadot_predictions == ksvm_educ_test$Class
View(table(agreement_anovadot))
prop.table(table(agreement_anovadot))

#TANHDOT
tanhdot_classifier <- ksvm(Class ~., data= ksvm_educ_train,
                           kernel = "tanhdot")
tanhdot_classifier
#check tanhdot model performance
tanhdot_predictions <- predict(tanhdot_classifier, ksvm_educ_test)
#look vs agreement vs non-agreement
#product boolean vector for predicting class
agreement_tanhdot <- tanhdot_predictions == ksvm_educ_test$Class
View(table(agreement_tanhdot))
prop.table(table(agreement_tanhdot))


#LAPLACE

laplacedot_classifier <- ksvm(Class ~., data= ksvm_educ_train,
                               kernel = "laplacedot")
laplacedot_classifier
#check laplacedot  model performance
laplacedot_predictions <- predict(laplacedot_classifier, ksvm_educ_test)
#look vs agreement vs non-agreement
#product boolean vector for predicting class
agreement_laplacedot  <- laplacedot_predictions == ksvm_educ_test$Class
View(table(agreement_laplacedot ))
prop.table(table(agreement_laplacedot ))

#BESSEL
besseldot_classifier <- ksvm(Class ~., data= ksvm_educ_train,
                             kernel = "besseldot")
besseldot_classifier
#check besseldot model performance
besseldot_predictions <- predict(besseldot_classifier, ksvm_educ_test)
#look vs agreement vs non-agreement
#product boolean vector for predicting class
agreement_besseldot <- besseldot_predictions == ksvm_educ_test$Class
View(table(agreement_besseldot))
prop.table(table(agreement_besseldot))

library(ElemStatLearn)
data(prostate)
str(prostate)

#regularized regreesion
#in caret: ridge, lasso, relaxo

small=prostate[1:5,]
lm(lpsa~.,data=small)

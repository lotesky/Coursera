library(quantmod)
library(forecast)

#**********************************************************

#forecasting

#Google data
from.dat<-as.Date("01/01/08",format="%m/%d/%y")
to.dat<-as.Date("01/01/15",format="%m/%d/%y")
getSymbols("GOOG",src="google",from=from.dat,to=to.dat)
GOOG<-GOOG[,-5]

#summarize monthly and store as time series
mGoog<-to.monthly(GOOG)
googOpen<-Op(mGoog)
ts1<-ts(googOpen,frequency=12)
plot(ts1,xlab="Years+1",ylab="GOOG")
#decompose
plot(decompose(ts1),xlab="Years+1")

#training and test
ts1Train<-window(ts1,start=1,end=5)
ts1Test<-window(ts1,start=5,end=(7-0.01))

#simple moving average
plot(ts1Train)
lines(ma(ts1Train,order=3),col="red")

#exponential smoothing
ets1<-ets(ts1Train,model="MMM")
fcast<-forecast(ets1)
plot(fcast)
lines(ts1Test,col="red")
#accuracy
accuracy(fcast,ts1Test)
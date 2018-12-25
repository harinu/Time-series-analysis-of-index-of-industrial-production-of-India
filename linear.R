x<-data<-read.csv("C:/Users/Hari Priya/Desktop/ISIDATA.csv")
attach(data)
print("linear trend")
l<-nrow(data)
x<-c(1:l)
for(i in 2:length(data))
{
sx<-0
sy<-0
sxx<-0
sxy<-0
y<-data[,i]
sx<-sum(x)
sy<-sum(y)
sxx<-sum(x*x)
sxy<-sum(x*y)
#print(paste(sx,sy,sxx,sxy))
#print(paste('the first equation for ',i,'is:',sy,'=',l,'a+',sx,'b'))
#print(paste('the second equation for',i,' is :',sxy,'=a',sx,'+b',sxx))
mat<-matrix(c(l,sx,sx,sxx),nrow=2,byrow=T)
aa<-solve(mat)
bb<-matrix(c(sy,sxy),ncol=1,byrow=F)
xx<-aa%*%bb
#print(xx)
print(paste('the trend equation for',i,' is y=',xx[1],'+',xx[2],'x'))
i=i+1
}


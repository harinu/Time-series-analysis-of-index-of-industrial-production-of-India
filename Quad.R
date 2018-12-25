data<-read.csv("C:/Users/Hari Priya/Desktop/ISIDATA.csv")
attach(data)
l<-nrow(data)
x<-c(1:l)
print("quadratic equation")
for(i in 2:length(data))
{
sx<-0
sy<-0
sxx<-0
sxy<-0
sxxx<-0
sxxxx<-0
y<-data[,i]
sx<-sum(x)
sy<-sum(y)
sxx<-sum(x*x)
sxy<-sum(x*y)
sxxx<-sum(x*x*x)
sxxxx<-sum(x*x*x*x)
sxxy<-sum(x*x*y)
print(paste('the first equation for ',i,'is:',sy,'=',l,'a+',sx,'b+c',sxx))
print(paste('the second equation for',i,' is :',sxy,'=a',sx,'+b',sxx,'+c',sxxx))
print(paste('the third equation for ',i,'is :',sxxy,'=a',sxx,'+b',sxxx,'+c',sxxxx))
mat<-matrix(c(l,sx,sxx,sx,sxx,sxxx,sxx,sxxx,sxxxx),nrow=3,byrow=T)
aa<-solve(mat)
bb<-matrix(c(sy,sxy,sxxy),ncol=1,byrow=F)
xx<-aa%*%bb
print(xx)
print(paste('the trend equation for',i,' is y=',xx[1],'+',xx[2],'x+',xx[3],'x^2'))
i=i+1
}
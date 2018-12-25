data<-read.csv("C:/Users/Hari Priya/Desktop/statdatafile.csv")
attach(data)
l<-nrow(data)
x<-c(1:l)
print("cubic equation")
for(i in 2:length(data))
{
sx<-0
sy<-0
sxx<-0
sxy<-0
sxxx<-0
sxxxx<-0
sxxxy<-0
sxxxxx<-0
sxxxxxx<-0
y<-data[,i]
sx<-sum(x)
sy<-sum(y)
sxx<-sum(x*x)
sxy<-sum(x*y)
sxxx<-sum(x*x*x)
sxxxx<-sum(x*x*x*x)
sxxy<-sum(x*x*y)
sxxxy<-sum(x*x*x*y)
sxxxxx<-sum(x*x*x*x*x)
sxxxxxx<-sum(x*x*x*x*x*x)
print(paste('the first equation for ',i,'is:',sy,'=',l,'a+',sx,'b+c',sxx,'+d',sxxx))
print(paste('the second equation for',i,' is :',sxy,'=a',sx,'+b',sxx,'+c',sxxx,'+d',sxxxx))
print(paste('the third equation for ',i,'is :',sxxy,'=a',sxx,'+b',sxxx,'+c',sxxxx,'+d',sxxxxx))
print(paste('the fourth equation for ',i,'is:',sxxxy,'=a',sxxx,'+b',sxxxx,'+c',sxxxxx,'+d',sxxxxxx))
mat<-matrix(c(l,sx,sxx,sxxx,sx,sxx,sxxx,sxxxx,sxx,sxxx,sxxxx,sxxxxx,sxxx,sxxxx,sxxxxx,sxxxxxx),nrow=4,byrow=T)
aa<-solve(mat)
bb<-matrix(c(sy,sxy,sxxy,sxxxy),ncol=1,byrow=F)
xx<-aa%*%bb
print(xx)
print(paste('the trend equation for',i,' is y=',xx[1],'+',xx[2],'x+',xx[3],'x^2',xx[4],'x^4'))
i=i+1
}
data<-read.csv("statdatafile.csv")
attach(data)
l<-nrow(data)
x<-c(1:l)
print("fourth degree equation")
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
sxxxxy<-sum(x*x*x*x*y)
sxs<-sum(x*x*x*x*x*x*x)
sxe<-sum(x^7)
print(paste('the first equation for ',i,'is:',sy,'=',l,'a+',sx,'b+c',sxx,'+d',sxxx,'+e',sxxxx))
print(paste('the second equation for',i,' is :',sxy,'=a',sx,'+b',sxx,'+c',sxxx,'+d',sxxxx,'+e',sxxxxx))
print(paste('the third equation for ',i,'is :',sxxy,'=a',sxx,'+b',sxxx,'+c',sxxxx,'+d',sxxxxx,'+e',sxxxxxx))
print(paste('the fourth equation for ',i,'is:',sxxxy,'=a',sxxx,'+b',sxxxx,'+c',sxxxxx,'+d',sxxxxxx,'+e',sxs))
print(paste('the fifth equation for ',i,'is :',sxxxxy,'=a',sxxxx,'+b',sxxxxx,'+c',sxxxxxx,'+d',sxs,'+e',sxe))
mat<-matrix(c(l,sx,sxx,sxxx,sxxxx,sx,sxx,sxxx,sxxxx,sxxxxx,sxx,sxxx,sxxxx,sxxxxx,sxxxxxx,sxxx,sxxxx,sxxxxx,sxxxxxx,sxs,sxxxx,sxxxxx,sxxxxxx,sxs,sxe),nrow=5,byrow=T)
aa<-solve(mat)
bb<-matrix(c(sy,sxy,sxxy,sxxxy,sxxxxy),ncol=1,byrow=F)
xx<-aa%*%bb
print(xx)
print(paste('the trend equation for',i,' is y=',xx[1],'+',xx[2],'x+',xx[3],'x^2',xx[4],'x^4',xx[5],'x^5'))
i=i+1

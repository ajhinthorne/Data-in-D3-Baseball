#Jump statistic practice
s<-.1
n<-50
data<-rbind(cbind(rnorm(n, 0, s),rnorm(n, 0, s)),
            cbind(rnorm(n,1,s),rnorm(n,1,s)),
            cbind(rnorm(n,0,s),rnorm(n,1,s)),
            cbind(rnorm(n,1,s),rnorm(n,0,s)))
plot(data)

install.packages("cstab")
library(cstab)

kseq<-c(2:5)
example<-cDistance(data,kseq,method="kmeans",kmIter=10)
plot(example$Jumps)

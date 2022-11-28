

options(digits = 22)
 x1<-1/3
 x2<-1/4

 if(round(x1-x2,7)==round(1/12,7)){
   print("Subtraction is correct")

} else
 {
   print("Subtraction is wrong")
 }
 
 
 options(digits = 22)
 x1<-1
 x2<-1/2
 print(x1-x2)
 print(1/2)
 if(x1-x2==1/2){
   print("Subtraction is correct")
   
 } else
 {
   print("Subtraction is wrong")
 }
 
 
 options(digits = 22)
 derivative<-function(x)
 {
   d<-((x+10^-15)-x)/10^-15
  
      
   return(d)
 }
 derivative(100000)
 
 library(ggplot2)
 #3
 set.seed(12345)
 
 myvar<-function(x)
 { sum1<-0
   n<-(1/(length(x)-1))
   sum1<- sum(x^2)
   ex2<-(sum(x))^2
   n2<-1/length(x)
   varience<-n*(sum1-(n2*ex2))
   print(varience)
 }
 
 
 #myvar(1:10)
 std<-1
 varience<-std^2
rnd<- rnorm(10000, mean=10^8, 1)
y<-c()
for (i in 1:length(rnd)) {
  y[i]<-myvar(x=rnd[1:i]) - var(rnd[1:i])
}
j<-1:10000

 ggplot2::ggplot(data_F) + geom_point(aes(x = 1:10000, y = y)) + ggplot2::theme_classic() +
 labs(title = "Dependencies of Y on i") + xlab("Index i") + ylab("Y")

 #4 
 options(digits = 2)
 improvement<-function(new_x)
 {
   d<-length(new_x)-1
   improve<-sum((new_x-mean(new_x))^2)/d
   return(improve)
 }
 new_y<-c()
 for (j in 1:length(rnd)) {
   new_y[j]<-round(improvement(new_x=rnd[1:j])-var(rnd[1:j]),digits=7)
 }
 ggplot2::ggplot() + geom_point(aes(x = 1:10000, y = new_y)) + ggplot2::theme_classic() +
   labs(title = "Improved Dependencies of Y on i") + xlab("Index i") + ylab("Y")
 
 #4
 if(result_matrix[1]==result_matrix[4] && result_matrix[2]==result_matrix[4] && result_matrix[3]==result_matrix[4])
 {
   print("A,B,C are working correctly")
 }else if(result_matrix[1]==result_matrix[4] && result_matrix[2]==result_matrix[4] && result_matrix[3]!=result_matrix[4])
 {
   print("A,B are working correctly")
 }
 else if(result_matrix[1]==result_matrix[4] && result_matrix[2]!=result_matrix[4] && result_matrix[3]==result_matrix[4])
 {
   print("A,C are working correctly")
 }
 else if(result_matrix[1]!=result_matrix[4] && result_matrix[2]==result_matrix[4] && result_matrix[3]==result_matrix[4])
 {
   print("B,C are working correctly")
 }
 else if(result_matrix[1]==result_matrix[4] && result_matrix[2]!=result_matrix[4] && result_matrix[3]!=result_matrix[4])
 {
   print("A is working correctly")
 }
 else if(result_matrix[1]!=result_matrix[4] && result_matrix[2]==result_matrix[4] && result_matrix[3]!=result_matrix[4])
 {
   print("B is working correctly")
 }
 else if(result_matrix[1]!=result_matrix[4] && result_matrix[2]!=result_matrix[4] && result_matrix[3]==result_matrix[4])
 {
   print("C is working correctly")
 }
 else if(result_matrix[1]!=result_matrix[4] && result_matrix[2]!=result_matrix[4] && result_matrix[3]!=result_matrix[4])
 {
   print("A,B,C are not working correctly")
 }
 else
 {
   
 }
 
 
 
 
 
 
 
 binomial_coefficient <- function(n, k) {
   A <- prod(1:n)/(prod(1:k) * prod(1:(n-k)))
   B <- prod((k + 1):n)/prod(1:(n - k))
   C <- prod(((k + 1):n)/(1:(n - k)))
   choose_function <- choose(n,k)
   result_vector <- c(A, B, C, choose_function)
   names(result_vector) <- c("A", "B", "C", "choose_function")
   #result_vector[which(is.nan(result_vector))] <- -1
   #result_vector[which(is.infinite(result_vector))] <- -2
  
   
     return(result_vector)
 }
 binomial_coefficient(2000,1000)

 
 
 result_plot<- matrix(list(),ncol = 4)
 for(k in 1:1000){
   n<-k*2
  result_plot <- rbind(result_plot, as.vector(binomial_coefficient(n,k)))
 }
 finitesA <- sum(is.finite(unlist(result_plot[,1])))
 finitesB <- sum(is.finite(unlist(result_plot[,2])))
 finitesC <- sum(is.finite(unlist(result_plot[,3])))
 
 naA <- sum(is.nan(unlist(result_plot[,1])))
 naB <- sum(is.nan(unlist(result_plot[,2])))
 naC <- sum(is.nan(unlist(result_plot[,3])))
 
 infinitesA <- sum(is.infinite(unlist(result_plot[,1])))
 infinitesB <- sum(is.infinite(unlist(result_plot[,2])))
 infinitesC <- sum(is.infinite(unlist(result_plot[,3])))
 

dfA <- data.frame(name = c("finite","infinite","NA") ,count = c(finitesA, infinitesA, naA))
dfB <- data.frame(name = c("finite","infinite","NA") ,count = c(finitesB, infinitesB, naB))
dfC <- data.frame(name = c("finite","infinite","NA") ,count = c(finitesC, infinitesC, naC))

p1 <- ggplot2::ggplot(dfA, aes(x = name, y = count)) + geom_bar(stat='identity')
p2 <- ggplot2::ggplot(dfB, aes(x = name, y = count)) + geom_bar(stat='identity')
p3 <- ggplot2::ggplot(dfC, aes(x = name, y = count)) + geom_bar(stat='identity')

cowplot::plot_grid(p1,p2,p3, labels = c("A","B","C"))
 print(result_plot)

 
 
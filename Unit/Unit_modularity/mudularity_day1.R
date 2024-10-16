setwd("~/Desktop/KU/Classes/machine_learning/CorimanyaMLbiol/Unit/Unit_modularity")

# Simulating probability of salamander extinction (PVA)
numextinct<-0
for (scounter in 1:100) {
  pop <-52
  for (tcounter in 1:25){
    pop<-pop*exp(1.01*(1-pop/60)+rnorm(1,0,2))
    if (pop<1){pop<-0}
  }
  if (pop==0){numextinct<-numextinct+1}
}
numextinct/100

# 1000 years instead
numextinct<-0
for (scounter in 1:1000) {
  pop <-52
  for (tcounter in 1:25){
    pop<-pop*exp(1.01*(1-pop/60)+rnorm(1,0,2))
    if (pop<1){pop<-0}
  }
  if (pop==0){numextinct<-numextinct+1}
}
numextinct/100

# fixing the error the bad way (no modularity!)
numextinct<-0
for (scounter in 1:1000) {
  pop <-52
  for (tcounter in 1:25){
    pop<-pop*exp(1.01*(1-pop/60)+rnorm(1,0,2))
    if (pop<1){pop<-0}
  }
  if (pop==0){numextinct<-numextinct+1}
}
numextinct/1000

# with plotting (still not modular)

plot(c(0,25),c(0,300),type='n',
     xlab="time step (yr)",ylab="populations")
for (scounter in 1:1000) { 
  pops<-c(52,rep(NA,25))
  for (tcounter in 1:25) #time steps
    {
    pops[tcounter+1]<-
      pops[tcounter]*exp(1.01*(1-pops[tcounter]/60)+rnorm(1,0,2))
    if (pops[tcounter+1]<1){pops[tcounter+1]<-0}
  }
    #judge extinctions
    if (pops[26]==0) {numextinct<-numextinct+1}
    #plot this sim
    lines(0:25, pops, type ='l')
}
numextinct/10000  
numextinct<-0
plot(c(0,25),c(0,300),type='n',
     xlab="time step (yr)",ylab="populations")
for (scounter in 1:10000) {   
  pops<-c(52,rep(NA,25))
  for (tcounter in 1:25){
    pops[tcounter+1]<-
      pops[tcounter]*
      exp(1.01*(1-pops[tcounter]/60)+
            rnorm(1,0,2))
  if (pops[tcounter+1]<1){pops[tcounter+1]<-0}
  }
#judge extinctions
if (pops[26]==0){numextinct<-numextinct+1}
#plot this sim
lines(0:25,pops,type='l')
}
numextinct/10000 

# log scale only first 100 sims for plot
numextinct<-0
plot(c(0,25),c(0,300),type='n',
     xlab="time step (yr)",ylab="populations")
for (scounter in 1:10000){
  pops<-c(52,rep(NA,25))
  for (tcounter in 1:25){
    pops[tcounter+1]<-
      pops[tcounter]*
      exp(1.01*(1-pops[tcounter]/60)+
            rnorm(1,0,2))
  if (pops[tcounter+1]<1){pops[tcounter+1]<-0}
  }
#judge extinctions
if (pops[26]==0){numextinct<-numextinct+1}
#plot this sim
if (scounter<=100) {
  lines(0:25,pops,type='l')}
}
numextinct/10000


##### DOING THIS WITH MODULARITY!! #####

# FIRST, what tasks do we need to accomplish??
## 1. simulate population through many time steps, keeping each time step for all populations
## 2. make a plot showing some or all populations fluctuating through time
## 3. Calculate extinction risk at any point in future or any several points.

#   second decide on interface, or inputs and outputs
## 1. inputs: p0 (starting population), r, k, and rsd (mean rate, carrying capacity, standard dev of rate),
##            numsims (number of simulations to do), numsteps (number of timesteps to do)
## 1. outputs: matrix of current and future population. 1 column is p0 and one column is p0+1
## matrix will have dimensions of numsims by numsteps+1
popsim <- function(p0, r, K, nsd, numsteps)
{
  res<- matrix(NA, length(p0), numsteps +1)
  res[,1] <-p0
    for (tcount in 1:numsteps)
    {
      res[,tcount+1]<-res[,tcount]*exp(r*(1-res[,tcount]/K)+rnorm(length(p0),0,nsd))
      res[res[,tcount+1]<1,tcount+1]<-0
    }
  return(res)
}
##2. m: matrix of numsims by numsteps+1 with each row a population time series from a different simulation
##    pts: a vector of which time series to plot. Default NA means plot them all.
##    logxp1: a T/F variable indicating whether or not to print on log(x+1) scale
## output: none...technically "side effect" of this code is the plot. plot is not an output.

plotter<-function(m,pts=1:dim(m)[1],logxp1=T)
{
  #keep only the rows of m indicated with pts
  m<-m[pts,,drop=F]
  #log(x+1) transform if desired
  if (logxp1)
  {
    m<-log10(m+1)
  }
  #plot
  plot(0:(dim(m)[2]-1),m[1,],type='n',xlab="Time step",ylab="Population",
       ylim=c(0,max(m)))
  for (rcounter in 2:(dim(m)[1]))
  {
    lines(0:(dim(m)[2]-1),m[rcounter,],type='l')
  }
}
## 3. m: matrix of numsims by numsteps+1 with each row a population time series from a different simulation
##    riskcols: a vector of indices with the range dim(m)[2] the corresponding columns of m
##    at which extinction risks are going to be computed.
## output: a vector of the same length as riskcols containing the extinction risks
extrisks<-function(m,risktimes)
{
  #keep only the columns of m that correspond to the times at which risk is needed
  m<-m[,risktimes+1, drop = F]
  #calculate risks
  risks<-apply(FUN=sum, X=(m==0), MARGIN=2)/(dim(m)[1])
  return(risks)
}

## Chances the salamander will go extinct in 25 years
sims<-popsim(p0 = rep(52,10000),r=1.01,K=60,nsd = 2,numsteps = 25)
extrisks(m=sims,risktimes = 25)
plotter(m=sims,pts=1:100,logxp1=T)

##10,15,20,25 years extinction risk
extrisks(m=sims,risktimes=c(10,15,20,25))
allrisks<-extrisks(m=sims,risktimes=0:25)
plot(0:25,allrisks,type='l',xlab='Time (years)',ylab='Extinction risk')

# What would happen if, 5 years from now, I can get the land owners to agree to stop letting
#fertilizer spill into the pond, I think that will make r go to 1.03 from that point forward.”
sims1<-popsim(p0=rep(52,10000),r=1.01,K=60,nsd=2,numsteps=5)
sims2<-popsim(p0=sims1[,dim(sims1)[2]],r=1.03,K=60,nsd=2,numsteps=20)
sims<-cbind(sims1,sims2[,2:dim(sims2)[2]])
extrisks(m=sims,risktimes=25)
allrisks<-extrisks(m=sims,risktimes=0:25)
plot(0:25,allrisks,type='l',xlab='Time (years)',ylab='Extinction risk')

## Follow-up question: What if r goes to 1.05?
sims1<-popsim(p0=rep(52,10000),r=1.01,K=60,nsd=2,numsteps=5)
sims2<-popsim(p0=sims1[,dim(sims1)[2]],r=1.05,K=60,nsd=2,numsteps=20)
sims<-cbind(sims1,sims2[,2:dim(sims2)[2]])
extrisks(m=sims,risktimes=25)
allrisks<-extrisks(m=sims,risktimes=0:25)
plot(0:25,allrisks,type='l',xlab='Time (years)',ylab='Extinction risk')


## Follow-up question: What if r goes to 1.1
sims1<-popsim(p0=rep(52,10000),r=1.01,K=60,nsd=2,numsteps=5)
sims2<-popsim(p0=sims1[,dim(sims1)[2]],r=1.1,K=60,nsd=2,numsteps=20)
sims<-cbind(sims1,sims2[,2:dim(sims2)[2]])
extrisks(m=sims,risktimes=25)
allrisks<-extrisks(m=sims,risktimes=0:25)
plot(0:25,allrisks,type='l',xlab='Time (years)',ylab='Extinction risk')

## Q6) What if I can gradually get them to reduce their fertilizer use 
#every 5 years so the r value improves each 5 years by a fixed amount?
#How would you do this given the functions we have developed?
# Starting parameters
p0 <- rep(52, 10000)  # initial population for all simulations
r_values <- c(1.01, 1.03, 1.05, 1.07, 1.09)  # improving r every 5 years
K <- 60  # carrying capacity
nsd <- 2  # standard deviation of growth rate
numsteps <- 5  # number of steps in each phase (5 years per r value)

# First 5 years with r = 1.01
sims <- popsim(p0 = p0, r = r_values[1], K = K, nsd = nsd, numsteps = numsteps)

# Simulate the remaining years in phases
for (i in 2:length(r_values)) {
  # Take the population at the end of the previous simulation as the starting point
  p0_next <- sims[, dim(sims)[2]]
  
  # Simulate the next 5 years with a higher r value
  sims_next <- popsim(p0 = p0_next, r = r_values[i], K = K, nsd = nsd, numsteps = numsteps)
  
  # Bind the results (skipping the first column to avoid repeating the initial population)
  sims <- cbind(sims, sims_next[, 2:dim(sims_next)[2]])
}

# Calculate extinction risk at the end of the 25 years
extrisks(m = sims, risktimes = 25)

# Plot the extinction risks across the 25 years
allrisks <- extrisks(m = sims, risktimes = 0:25)
plot(0:25, allrisks, type = 'l', xlab = 'Time (years)', ylab = 'Extinction risk')

## increasing k
sims<-popsim(p0=rep(52,10000),r=1.01,K=80,nsd=2,numsteps=25)
extrisks(m=sims,risktimes=25)
sims<-popsim(p0=rep(52,10000),r=1.01,K=150,nsd=2,numsteps=25)
extrisks(m=sims,risktimes=25)
sims<-popsim(p0=rep(52,10000),r=1.01,K=250,nsd=2,numsteps=25)
extrisks(m=sims,risktimes=25)

##reduce effects of winter?
sims<-popsim(p0=rep(52,10000),r=1.01,K=60,nsd=1.5,numsteps=25)
extrisks(m=sims,risktimes=25)
sims<-popsim(p0=rep(52,10000),r=1.01,K=60,nsd=1,numsteps=25)
extrisks(m=sims,risktimes=25)
sims<-popsim(p0=rep(52,10000),r=1.01,K=60,nsd=0.5,numsteps=25)
extrisks(m=sims,risktimes=25)


#A function for doing the population simulations for our endangered salamander
#using a stochastic version of the Ricker model.
#
#Args
#p0: a vector of starting populations length equals the number of simulations you
#want to do
#r, K, nsd: model parameters, nsd the standard deviation of the noise
#numsteps: the number of time steps to do
#
#Output
#A numsims by numsteps+1 matrix with each row a population time series from a
#different simulation
popsim1<-function(p0,r,K,nsd,numsteps)
{
  res<-matrix(NA,length(p0),numsteps+1)
  res[,1]<-p0
  for (tcount in 1:numsteps)
  {
    res[,tcount+1]<-res[,tcount]*exp(r*(1-res[,tcount]/K)+rnorm(length(p0),0,nsd))
    res[res[,tcount+1]<1,tcount+1]<-0
  }
  return(res)
}

#A function for doing the population simulations for our endangered salamander
#using a stochastic version of the Beverton-Holt model.
#
#Args
#p0: a vector of starting populations length equals the number of simulations you
#want to do
#r, K, nsd: model parameters, nsd the standard deviation of the noise
#numsteps: the number of time steps to do
#
#Output
#A numsims by numsteps+1 matrix with each row a population time series from a
#different simulation
#
popsim2<-function(p0,r,K,nsd,numsteps)
{
  res<-matrix(NA,length(p0),numsteps+1)
  res[,1]<-p0
  for (tcount in 1:numsteps)
  {
    res[,tcount+1]<-(r+rnorm(length(p0),0,nsd))*res[,tcount]/(1+res[,tcount]/K)
    res[res[,tcount+1]<1,tcount+1]<-0
  }
  return(res)
}
#using any specified model (specified by a "one-step function", see the
#osf argument below).
#
#Args
#p0: a vector of starting populations length equals the numer of simulations you
#want to do
#params: a named vector of parameters for the model you will run, names are
#the parameter names, values are their values
#osf: A "one-step function", i.e., a function with two arguments. First, a vector
#of populations, and second a named vector of parameters (same format as
#params above). Gives the populations at the next time step.
#numsteps: the number of time steps to do
#
#Output
#A numsims by numsteps+1 matrix with each row a population time series from a
#different simulation
#
popsim<-function(p0,osf,params,numsteps)
{
  res<-matrix(NA,length(p0),numsteps+1)
  res[,1]<-p0
  for (tcount in 1:numsteps)
  {
    res[,tcount+1]<-osf(pt=res[,tcount],params=params)
    res[res[,tcount+1]<1,tcount+1]<-0
  }
  return(res)
}
#Ricker one-step function.
#
#Args
#pt: A vector of populations at time t
#params: A named vector with entries named r, K, nsd for the growth rate,
#carrying capacity, and noise standard deviation under a stochastic version of
#the Ricker model. If you do not use the right names, this won't work.
#
#Output
#A vector of populations of the same length as pt, #for the next time step
#
RickerOSF<-function(pt,params)
{
  r<-params['r']
  K<-params['K']
  nsd<-params['nsd']
  return(pt*exp(r*(1-pt/K)+rnorm(length(pt),0,nsd)))
}
#Beverton-Holt one-step function.
#
#Args
#pt: A vector of populations at time t
#params: A named vector with entries named r, K, nsd for the growth rate,
#carrying capacity, and noise standard deviation under a stochastic version
#of the Beverton-Holt model. You HAVE to use these names.
#
#Output
#A vector of populations of the same length as pt, #for the next time step
#
BevHoltOSF<-function(pt,params)
{
  r<-params['r']
  K<-params['K']
  nsd<-params['nsd']
  return((r+rnorm(length(pt),0,nsd))*pt/(1+pt/K))
}

Rsims<-popsim(p0=rep(52,10000),osf=RickerOSF,params=c(r=1.01,K=60,nsd=2),
              numsteps=25)
BHsims<-popsim(p0=rep(52,10000),osf=BevHoltOSF,params=c(r=1.01,K=60,nsd=2),
               numsteps=25)
extrisks(m=Rsims,risktimes=25)
extrisks(m=BHsims,risktimes=25)

defendant_race=c("BLACK","WHITE")
death_penalty=c("yes","no")
victim_race=c("WHITE","BLACK")
A=expand.grid(defendant_race=defendant_race,death_penalty=death_penalty,victim_race=victim_race)
data=c(53,11,414,37,0,4,16,139)
table=cbind(A,obs=data)
B=xtabs(data~defendant_race+death_penalty+victim_race,data=table)
#####keep the same order in ecpand.grid and xtabs...now partial tables will be basod on last factor in the command xtabs....fill the data according to 1st component of last factor(i.e. victim race)..the other components of last factor...
#condional_odds
conditional_odds=function(k){
i=j=array(NA,length(victim_race))
p=q=matrix(NA,length(defendant_race),length(death_penalty))
  for(i in 1:length(defendant_race)){
    for(j in 1:length(death_penalty)){
      if(i==j){
        p[i,j]=B[i,j,k]
        q[i,j]=1.0
      }
      else{
        q[i,j]=B[i,j,k]
        p[i,j]=1.0
      }
  } 
  }
prod(p)/prod(q)
}
k=c(1:2)
sapply(k,conditional_odds)
#marginal odds
marginal_table=B[,,1]+B[,,2]
marginal_odds=function(B){
p=q=array(NA,dim=nrow(B))
for(i in 1:dim(B)[1]){
  for(j in 1:dim(B)[2]){
    if(i == j){
      p[i]=B[i,j]
    }
    else{
      q[i]=B[i,j]
    }
  }
}
prod(p)/prod(q)
}
marginal_odds(marginal_table)


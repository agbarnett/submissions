# 99_reconstruct_probabilities.R
# reconstructing the probabilities for the ordinal model
# August 2022

# mean estimates from table in results_editorial
intercept = -0.66538380
cosine = -0.24097109
sine = -0.03683322 
week = -0.13931038 
vers1 = 3.32085785
vers2 = 3.87691387  
vers3 = 4.64843030
C = c(0, 1.95960530)
# data
cos = 1
sin = 1
weekend = 0
version = c(0,0,0) # assume first submission
k = 2 # first cut = 1, second cut = 2
# regression equation
logit1 = intercept + (cosine*cos) + (sine*sin) +(week*weekend) + (vers1*version[1]) + (vers2*version[2]) + (vers3*version[3] ) - C[k]
# regression equation with weekend
weekend = 1
logit2 = intercept + (cosine*cos) + (sine*sin) +(week*weekend) + (vers1*version[1]) + (vers2*version[2]) + (vers3*version[3] ) - C[k]

# odds ratio based on parameter estimate
exp(week)

p1 = inv.logit(logit1)
p2 = inv.logit(logit2)
o1 = p1 / (1-p1)
o2 = p2 / (1-p2)
o2 / o1 # same as exp(week); result is the same using the first and second cut-point

library(RUnit)
library(rvle)

f <- rvle.open('petrinet-meteo.vpz')
rvle.setOutputPlugin(f, "view", "storage")
rvle.setLinearCombination(f, 123, 2)
rvle.clearConditionPort(f,"petri","day")
rvle.addIntegerCondition(f, "petri", "day", 3)
rvle.addIntegerCondition(f, "petri", "day", 3)


resTrue = rvle.runManager(f, commonSeed = TRUE)
checkEquals(nrow(resTrue), 2)
checkEquals(ncol(resTrue), 2)
checkEquals(length(resTrue[[1,1]]), 1)
#compute matrix of comparisons for each cell
matTrue = resTrue[[2,1]][[1]]  == resTrue[[1,1]][[1]]
#all comparisons should be true
checkEquals((sum(matTrue)==ncol(matTrue)*nrow(matTrue)),TRUE)


resFalse = rvle.runManager(f, commonSeed = FALSE)
#check nrow(res) = 2
checkEquals(nrow(resFalse), 2)
#check ncol(res) = 2
checkEquals(ncol(resFalse), 2)
#check length(resTrue[[1,1]]) = 1
checkEquals(length(resFalse[[1,1]]), 1)

matFalse = resFalse[[2,1]][[1]]  == resFalse[[1,1]][[1]]
#compute matrix of comparisons for each cell
sumFalse = sum(matFalse)
#some comparisons should be false
checkEquals((sum(matFalse)<ncol(matFalse)*nrow(matFalse)),TRUE)


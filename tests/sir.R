library(RUnit)
library(rvle)
f <- rvle.open('sir.vpz')

cnd <- rvle.listConditions(f)
checkEquals(cnd[1], "cond_sir")

result <- rvle.run(f)

# check result of the view
checkEqualsNumeric(dim(result[[1]])[1], 2000)
checkEqualsNumeric(dim(result[[1]])[2], 4)

# get the first view
view1 <- result[[1]]
checkEqualsNumeric(view1[[1]][[2000]], 19.99, tolerance=1e-5)
checkEqualsNumeric(view1[[2]][[2000]], 0.1542165, tolerance=1e-5)
checkEqualsNumeric(view1[[3]][[2000]], 4.973883e+02, tolerance=1e-5)
checkEqualsNumeric(view1[[4]][[2000]], 3.457524, tolerance=1e-5)

# show port list
ports <- rvle.listConditionPorts(f, cnd)
checkEquals(ports[1], "a")
checkEquals(ports[2], "active")
checkEquals(ports[3], "dependance")
checkEquals(ports[4], "r")
checkEquals(ports[5], "threshold")
checkEquals(ports[6], "variables")

# change condition
checkEqualsNumeric(rvle.getConditionPortValues(f, cnd, "a"), 0.5, tolerance=1e-5)
rvle.setRealCondition(f, cnd, "a", 0.6)

# get the first view
result <- rvle.run(f)
checkEqualsNumeric(dim(result[[1]])[1], 2000)
checkEqualsNumeric(dim(result[[1]])[2], 4)

view1 <- result[[1]]
checkEqualsNumeric(view1[[1]][[2000]], 19.99, tolerance=1e-5)
checkEqualsNumeric(view1[[2]][[2000]], 0.04369215, tolerance=1e-5)
checkEqualsNumeric(view1[[3]][[2000]], 4.927206e+02, tolerance=1e-5)
checkEqualsNumeric(view1[[4]][[2000]], 8.235717, tolerance=1e-5)

# check the seed
seed = rvle.getSeed(f)
checkEquals(seed, 12379843)
rvle.setSeed(f, 12345678)
seed = rvle.getSeed(f)
checkEquals(seed, 12345678)

# check the beginning time
rvle.setBegin(f, 123.321)
begin = rvle.getBegin(f)
checkEqualsNumeric(begin, 123.321, tolerance=1e-5)
rvle.setBegin(f, 0.0)

# check the duration
duration = rvle.getDuration(f)
checkEqualsNumeric(duration, 20, tolerance=1e-5)
rvle.setDuration(f, 123.321)
duration = rvle.getDuration(f)
checkEqualsNumeric(duration, 123.321, tolerance=1e-5)

# check the experimental frames
rvle.setDuration(f, 0.25)
rvle.setLinearCombination(f, 1, 5)
result = rvle.runManager(f)
result
checkEqualsNumeric(dim(result)[1], 5)
checkEqualsNumeric(dim(result)[2], 1)

rvle.addRealCondition(f, "cond_sir", "a", 0.6)
rvle.addRealCondition(f, "cond_sir", "r", 0.006)
result = rvle.runManager(f)
result
checkEqualsNumeric(dim(result)[1], 5)
checkEqualsNumeric(dim(result)[2], 2)

rvle.setTotalCombination(f, 1, 5)
result = rvle.runManager(f)
result
checkEqualsNumeric(dim(result)[1], 5)
checkEqualsNumeric(dim(result)[2], 4)

#check the output plugin setting
checkEquals(rvle.getOutputPlugin(f,"sirview"), "storage")
rvle.setOutputPlugin(f,"sirview","text")
checkEquals(rvle.getOutputPlugin(f,"sirview"), "text")
rvle.setOutputPlugin(f,"sirview","storage")

#check the list of views
cnd <- rvle.listViews(f)
checkEquals(cnd[1], "sirview")

library(RUnit)
library(rvle)

sir <- new("Rvle", file = "sir.vpz")


# simple run and direct access to the result
result <- run(sir)@outlist[[1]]

# check result of the view
checkEqualsNumeric(dim(result)[1], 2000)
checkEqualsNumeric(dim(result)[2], 4)

# simple run and keep
sir <- run(sir)
result <- results(sir)[[1]]

# check result of the view
checkEqualsNumeric(dim(result)[1], 2000)
checkEqualsNumeric(dim(result)[2], 4)

# get the first view
view1 <- result
checkEqualsNumeric(view1$time[[2000]], 19.99, tolerance=1e-5)
checkEqualsNumeric(view1$"Top model:sir.I"[[2000]], 0.1542165, tolerance=1e-5)
checkEqualsNumeric(view1$"Top model:sir.R"[[2000]], 4.973883e+02, tolerance=1e-5)
checkEqualsNumeric(view1$"Top model:sir.S"[[2000]], 3.457524, tolerance=1e-5)

# change condition
result <- run(sir, cond_sir.a = 0.6)@outlist[[1]]

checkEqualsNumeric(dim(result)[1], 2000)
checkEqualsNumeric(dim(result)[2], 4)

view1 <- result
checkEqualsNumeric(view1$time[[2000]], 19.99, tolerance=1e-5)
checkEqualsNumeric(view1$"Top model:sir.I"[[2000]], 0.04369215, tolerance=1e-5)
checkEqualsNumeric(view1$"Top model:sir.R"[[2000]], 4.927206e+02, tolerance=1e-5)
checkEqualsNumeric(view1$"Top model:sir.S"[[2000]], 8.235717, tolerance=1e-5)

# the new list of args
sir <- run(sir, cond_sir.a = 0.6)

# check the storage of the result
config(sir)["restype"] <- "matrix"
config(sir)$restype  <- "matrix"
checkTrue(is.matrix(run(sir, duration = 0.25)@outlist[[1]]))

config(sir)["restype"] <- "dataframe"
config(sir)$restype  <- "dataframe"
checkTrue(is.data.frame(run(sir, duration = 0.25)@outlist[[1]]))


# check many experimental plans with small duration
config(sir)["plan"] <- "linear"
sir <- run(sir, cond_sir.a = c(0.6, 0.5), cond_sir.r = c(0.005, 0.004), duration = 0.25)

result <- results(sir)
checkEquals(dim(result), c(1,2))

config(sir)["replicas"] <- 2
sir <- run(sir, cond_sir.a = c(0.6, 0.5), cond_sir.r = c(0.005, 0.004), duration = 0.25)

result <- results(sir)
checkEquals(dim(result), c(2,2))

config(sir)["plan"] <- "total"
sir <- run(sir, cond_sir.a = c(0.6, 0.5), cond_sir.r = c(0.005, 0.004), duration = 0.25)

result <- results(sir)
checkEquals(dim(result), c(2,4))

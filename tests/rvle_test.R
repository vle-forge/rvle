#!/usr/bin/Rscript

#tmpdir = "/tmp"
tmpdir = tempdir()
file.copy("vlehome",tmpdir,recursive = TRUE)
vlehome = normalizePath(paste(tmpdir,"/vlehome",sep=""))
Sys.setenv(VLE_HOME=vlehome)

library(rvle)

currentdir = getwd();
unlink("./test_port/buildvle", recursive=TRUE, force = TRUE)
.rvle.compile_test_port()
setwd(currentdir)
unlink("./test_port/buildvle", recursive=TRUE, force = TRUE)


##########
# Test packages function
##########

pkgs = rvle.listPackages(justprint=FALSE)
checkEquals(length(pkgs),3)
checkEquals(pkgs[1],"test_port")
checkEquals(pkgs[2],"vle.adaptative-qss")
checkEquals(pkgs[3],"vle.output")
pkgContent = rvle.packageContent("test_port", justprint=FALSE)
checkEquals(length(pkgContent),12)

##########
# Test conditions
##########
f <- rvle.open(file="test_conditions.vpz",pkg="test_port")

cnd <- rvle.listConditions(f)

checkEquals(length(cnd), 2)
checkTrue("test" %in% cnd)
checkTrue("simulation_engine" %in% cnd)

# show port list
ports <- rvle.listConditionPorts(f, cnd[which(cnd == "test")])

checkTrue("bool" %in% ports)
checkTrue("double" %in% ports)
checkTrue("int" %in% ports)
checkTrue("multi_bool" %in% ports)
checkTrue("multi_double" %in% ports)
checkTrue("multi_int" %in% ports)
checkTrue("multi_string" %in% ports)
checkTrue("multi_tuple" %in% ports)
checkTrue("notmanaged" %in% ports)
checkTrue("string" %in% ports)
checkTrue("tuple" %in% ports)

# check the type of the port values vector
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "string")),
        "character")
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "double")),
        "double")
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "int")),
        "integer")
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "bool")),
        "logical")

### Change of behavior, this is too ambiguous 
## checkEquals(storage.mode(rvle.getConditionPortValues(f, "test",
##                         "multi_string")), "character")
## checkEquals(storage.mode(rvle.getConditionPortValues(f, "test",
##                         "multi_double")), "double")
## checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "multi_int")),
##         "integer")
## checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "multi_bool")),
##         "logical")
res = rvle.getConditionPortValues(f, "test","multi_string")
checkEquals(storage.mode(res),"list")
checkEquals(storage.mode(res[[1]]),"character")
checkEquals(storage.mode(res[[2]]),"character")
res = rvle.getConditionPortValues(f, "test","multi_double")
checkEquals(storage.mode(res[[1]]),"double")
checkEquals(storage.mode(res[[2]]),"double")
res = rvle.getConditionPortValues(f, "test","multi_int")
checkEquals(storage.mode(res[[1]]),"integer")
checkEquals(storage.mode(res[[2]]),"integer")
res = rvle.getConditionPortValues(f, "test","multi_bool")
checkEquals(storage.mode(res[[1]]),"logical")
checkEquals(storage.mode(res[[2]]),"logical")
##End new behavior

tuple <- rvle.getConditionPortValues(f, "test", "tuple")

##Change of behavior : directly cast tuple into doubles 
#checkEquals(storage.mode(tuple), "list")
#checkEquals(tuple[[1]][1], 1.0)
#checkEquals(tuple[[1]][2], 2.0)
#checkEquals(tuple[[1]][3], 3.0)
#checkEquals(tuple[[1]][4], 4.0)
#checkEquals(tuple[[1]][5], 5.0)
checkEquals(storage.mode(tuple), "double")
checkEquals(tuple[1], 1.0)
checkEquals(tuple[2], 2.0)
checkEquals(tuple[3], 3.0)
checkEquals(tuple[4], 4.0)
checkEquals(tuple[5], 5.0)
##End new behavior

newtuple <- seq(length=14, from=6, to=19)
rvle.setTupleCondition(f, "test", "tuple", newtuple)
tuple <- rvle.getConditionPortValues(f, "test", "tuple")

##Change of behavior : directly cast tuple into doubles
#checkEquals(storage.mode(tuple), "list")
#checkEquals(tuple[[1]][1], 6.0)
#checkEquals(tuple[[1]][2], 7.0)
#checkEquals(tuple[[1]][3], 8.0)
#checkEquals(tuple[[1]][4], 9.0)
checkEquals(storage.mode(tuple), "double")
checkEquals(tuple[1], 6.0)
checkEquals(tuple[2], 7.0)
checkEquals(tuple[3], 8.0)
checkEquals(tuple[4], 9.0)
##End new behavior

tuple <- rvle.getConditionPortValues(f, "test", "multi_tuple")
checkEquals(storage.mode(tuple), "list")
checkEquals(tuple[[1]][1], 0.0)
checkEquals(tuple[[1]][2], 1.0)
checkEquals(tuple[[1]][3], 2.0)
checkEquals(tuple[[2]][1], 3.0)
checkEquals(tuple[[2]][2], 4.0)
checkEquals(tuple[[2]][3], 5.0)


# check the case of unmanaged type

##Change of behavior : all types are managed
#checkException(rvle.getConditionPortValues(f,"test", ",notmanaged"))
notmanaged = rvle.getConditionPortValues(f,"test", "notmanaged")
checkEquals(storage.mode(notmanaged$a), "double")
checkEquals(storage.mode(notmanaged$b), "double")
checkException(rvle.getConditionPortValues(f,"test", "notexisting"))
##End new behavior

##########
# Test Vle values handling on conditions
##########

f <- rvle.open(file="test_conditions.vpz",pkg="test_port")

c = rvle.getConditionPortValues(f,"test","bool")
checkEquals(class(c), "logical")##note: 'bool' are not typed 'VleBOOLEAN'
rvle.addValueCondition(f,"test","bool",c)
c = rvle.getConditionPortValues(f,"test","bool")
checkEquals(class(c),"VleMULTIPLE_VALUES")
checkEquals(class(c[[1]]),"logical")
checkEquals(class(c[[2]]),"logical")

c = rvle.getConditionPortValues(f,"test","double")
checkEquals(class(c), "numeric")##note: 'double' are not typed 'VleDOUBLE'
rvle.addValueCondition(f,"test","double",c)
c = rvle.getConditionPortValues(f,"test","double")
checkEquals(class(c),"VleMULTIPLE_VALUES")
checkEquals(class(c[[1]]),"numeric")
checkEquals(class(c[[2]]),"numeric")

c = rvle.getConditionPortValues(f,"test","int")
checkEquals(class(c), "integer")
rvle.addValueCondition(f,"test","int",c)
c = rvle.getConditionPortValues(f,"test","int")
checkEquals(class(c),"VleMULTIPLE_VALUES")
checkEquals(class(c[[1]]),"integer")
checkEquals(class(c[[2]]),"integer")

c = rvle.getConditionPortValues(f,"test","tuple")
c1 = rvle.getConditionPortValues(f,"test","double")
checkEquals(class(c), "VleTUPLE")
checkEquals(class(c1), "VleMULTIPLE_VALUES")
rvle.addValueCondition(f,"test","tuple",c1)
c = rvle.getConditionPortValues(f,"test","tuple")
checkEquals(class(c),"VleMULTIPLE_VALUES")
checkEqualsNumeric(length(c),3)
checkEquals(class(c[[1]]),"VleTUPLE")
checkEquals(class(c[[2]]),"numeric")
checkEquals(class(c[[3]]),"numeric")

c <- rvle.getConditionPortValues(f, "test", "multi_tuple")
checkEquals(class(c),"VleMULTIPLE_VALUES")
checkEqualsNumeric(length(c),2)
checkEquals(class(c[[1]]),"VleTUPLE")
checkEquals(class(c[[2]]),"VleTUPLE")
class(c) <- "VleSET"
rvle.setValueCondition(f, "test", "multi_tuple",c)
c <- rvle.getConditionPortValues(f, "test", "multi_tuple")
checkEquals(class(c),"VleSET")
checkEqualsNumeric(length(c),2)
checkEquals(class(c[[1]]),"VleTUPLE")
checkEquals(class(c[[2]]),"VleTUPLE")

##implicit conversion
f <- rvle.open(file="test_conditions.vpz",pkg="test_port")

c = list(1,2,3)
rvle.setValueCondition(f, "test", "bool",c)
c = rvle.getConditionPortValues(f, "test", "bool")
checkEquals(class(c),"VleSET")
checkEquals(storage.mode(c),"list")
checkEquals(length(c),3)
checkEquals(class(c[[1]]),"numeric")
checkEqualsNumeric(c[[1]],1)
checkEqualsNumeric(c[[2]],2)
checkEqualsNumeric(c[[3]],3)

c = c(4.2,5,6)
rvle.setValueCondition(f, "test", "bool",c)
c = rvle.getConditionPortValues(f, "test", "bool")
checkEquals(class(c),"VleTUPLE")
checkEquals(storage.mode(c),"double")
checkEquals(length(c),3)
checkEqualsNumeric(c[1],4.2)
checkEqualsNumeric(c[2],5)
checkEqualsNumeric(c[3],6)

c = list(id1=4.2,id2=5,id3=6)
rvle.setValueCondition(f, "test", "bool",c)
c = rvle.getConditionPortValues(f, "test", "bool")
checkEquals(class(c),"VleMAP")
checkEquals(storage.mode(c),"list")
checkEqualsNumeric(length(c),3)
checkEqualsNumeric(c$id1,4.2)
checkEqualsNumeric(c$id2,5)
checkEqualsNumeric(c$id3,6)

c = list(id1=1,3.6,3)#TODO to improve if only a part of names is given
rvle.setValueCondition(f, "test", "bool",c)
c = rvle.getConditionPortValues(f, "test", "bool")
checkEquals(class(c),"VleMAP")
checkEquals(storage.mode(c),"list")
checkEquals(length(c),2)
checkEquals(class(c[[1]]),"numeric")
checkEqualsNumeric(c[[1]],1)
checkEqualsNumeric(c[[2]],3)

c = list(id1=list("hello","hello2"),id2=c(1.2,5),id3=c(6), id4=9,
        id5=c("hello",1), id6=TRUE)
rvle.setValueCondition(f, "test", "bool",c)
cbis = rvle.getConditionPortValues(f, "test", "bool")
checkEquals(class(cbis),"VleMAP")
checkEqualsNumeric(length(cbis),6)
checkEquals(class(cbis$id1),"VleSET")
checkEquals(class(cbis$id1[[1]]),"character")
checkEquals(class(cbis$id1[[2]]),"character")
checkEquals(cbis$id1[[2]],"hello2")
checkEquals(class(cbis$id2),"VleTUPLE")
checkEqualsNumeric(cbis$id2[1],1.2)
checkEqualsNumeric(cbis$id2[2],5)
checkEqualsNumeric(cbis$id3,6)
checkEqualsNumeric(cbis$id4,9)
checkEquals(class(cbis$id5),"VleSET")
checkEquals(cbis$id5[[1]],"hello")
checkEquals(cbis$id5[[2]],"1")
checkEquals(cbis$id6,TRUE)
rvle.setValueCondition(f, "test", "bool",cbis)
cter = rvle.getConditionPortValues(f, "test", "bool")
checkEquals(class(cter),"VleMAP")
checkEquals(length(cter),6)
checkEquals(class(cter$id1),"VleSET")
checkEquals(class(cter$id1[[1]]),"character")
checkEquals(class(cter$id1[[2]]),"character")
checkEquals(cter$id1[[2]],"hello2")
checkEquals(class(cter$id2),"VleTUPLE")
checkEqualsNumeric(cter$id2[1],1.2)
checkEqualsNumeric(cter$id2[2],5)
checkEqualsNumeric(cter$id3,6)
checkEqualsNumeric(cter$id4,9)
checkEquals(class(cter$id5),"VleSET")
checkEquals(cter$id5[[1]],"hello")
checkEquals(cter$id5[[2]],"1")
checkEquals(cter$id6,TRUE)


c = matrix(c(1,2,3,4,5,6),nrow=3)
rvle.setValueCondition(f, "test", "bool",c)
c = rvle.getConditionPortValues(f, "test", "bool")
checkEqualsNumeric(class(c),"VleTABLE")
checkEqualsNumeric(dim(c)[[1]],3)
checkEqualsNumeric(dim(c)[[2]],2)
checkEqualsNumeric(c[1,2],4)
rvle.setValueCondition(f, "test", "bool",c)
c = rvle.getConditionPortValues(f, "test", "bool")
checkEquals(class(c),"VleTABLE")
checkEqualsNumeric(dim(c)[[1]],3)
checkEqualsNumeric(dim(c)[[2]],2)
checkEquals(c[1,2],4)

c = data.frame(matrix(c(1,2,3,4,5,6),nrow=3))
c[1,1] = "hello"
rvle.setValueCondition(f, "test", "bool",c)
c = rvle.getConditionPortValues(f, "test", "bool")
checkEquals(class(c),"VleMATRIX")
checkEqualsNumeric(dim(c)[[1]],4, tolerance=1e-5)
checkEqualsNumeric(dim(c)[[2]],2, tolerance=1e-5)
checkEquals(c[[1,2]],"X2")
checkEquals(c[[4,1]],"3")
rvle.setValueCondition(f, "test", "bool",c)
c = rvle.getConditionPortValues(f, "test", "bool")
checkEquals(class(c),"VleMATRIX")
checkEqualsNumeric(dim(c)[[1]],4, tolerance=1e-5)
checkEquals(dim(c)[[2]],2, tolerance=1e-5)
checkEquals(c[[1,2]],"X2")
checkEquals(c[[4,1]],"3", tolerance=1e-5)
f <- rvle.open(file="test_conditions.vpz",pkg="test_port")

##########
# Test experiment data
##########

f <- rvle.open(file="test_simulation.vpz", pkg="test_port")

# check the seed (TODO)
seed = rvle.getSeed(f)
#checkEquals(seed, 12379843)
rvle.setSeed(f, 12345678)
seed = rvle.getSeed(f)
#checkEquals(seed, 12345678)

# check the beginning time
rvle.setBegin(f, 123.321)
begin = rvle.getBegin(f)
checkEqualsNumeric(begin, 123.321, tolerance=1e-5)

# check the duration
duration = rvle.getDuration(f)
checkEqualsNumeric(duration, 20, tolerance=1e-5)
rvle.setDuration(f, 123.321)
duration = rvle.getDuration(f)
checkEqualsNumeric(duration, 123.321, tolerance=1e-5)

#check the list of views
cnd <- rvle.listViews(f)
checkEquals(cnd[1], "view")
checkEquals(cnd[2], "view2")

#check the output plugin setting
checkEquals(rvle.getOutputPlugin(f,"view"), "vle.output/storage")
rvle.setOutputPlugin(f,"view","mypackage/myplugin")
checkEquals(rvle.getOutputPlugin(f,"view"), "mypackage/myplugin")

#check save
rvle.save(f,"__test_rvle.vpz")
f = rvle.open("__test_rvle.vpz")
checkEquals(rvle.getOutputPlugin(f,"view"), "mypackage/myplugin")

#manage views
obss = rvle.listObservables(f);
checkEquals(obss[1], "obs")
obsPorts = rvle.listObservablePorts(f, "obs")
checkEquals(obsPorts[1], "obsPort")
attachedViews = rvle.listAttachedViews(f, "obs", "obsPort")
checkEquals(length(attachedViews), 2)
checkEquals(attachedViews[2], "view2")

rvle.addObservablePort(f, "obs","newPort")
obsPorts = rvle.listObservablePorts(f, "obs")
checkEquals(obsPorts[1], "newPort")
checkEquals(obsPorts[2], "obsPort")
rvle.removeObservablePort(f, "obs","obsPort")
obsPorts = rvle.listObservablePorts(f, "obs")
checkEquals(rvle.getObservablePortsSize(f, "obs"), 1)
checkEquals(obsPorts[1], "newPort")
checkEquals(length(rvle.listAttachedViews(f, "obs", "newPort")), 0)
rvle.attachView(f, "view", "obs", "newPort")
attachedViews = rvle.listAttachedViews(f, "obs", "newPort")
checkEquals(attachedViews[1], "view")

##########
# Test simulation
##########
f <- rvle.open(file="test_simulation.vpz", pkg="test_port")

### normal run 
result <- rvle.run(f)
checkEquals(class(result$view), "data.frame")

# check result of the view
checkEqualsNumeric(dim(result$view)[1], 21, tolerance=1e-5)
checkEqualsNumeric(dim(result$view)[2], 2, tolerance=1e-5)
checkEqualsNumeric(dim(result$view2)[1], 1, tolerance=1e-5)
checkEqualsNumeric(dim(result$view2)[2], 2, tolerance=1e-5)

view1 <- result$view
checkEquals(names(view1)[1], "time")
checkEquals(names(view1)[2], "Top model:Perturb.obsPort")

### matrix run
result <- rvle.runMatrix(f)
checkEquals(class(result$view), "matrix")
checkEquals(class(result$view[1,1]),"numeric")

# check result of the view
checkEqualsNumeric(dim(result$view)[1], 21, tolerance=1e-5)
checkEqualsNumeric(dim(result$view)[2], 2, tolerance=1e-5)
checkEqualsNumeric(dim(result$view2)[1], 1, tolerance=1e-5)
checkEqualsNumeric(dim(result$view2)[2], 2, tolerance=1e-5)

view1 <- result$view
checkEqualsNumeric(view1[3,2], 0, tolerance=1e-5)
checkEqualsNumeric(view1[4,2], 1.5, tolerance=1e-5)

### complete run
result <- rvle.runPoly(f)
checkEquals(class(result$view), "matrix")
checkEquals(class(result$view[1,1]),"list")

# check result of the view
checkEqualsNumeric(dim(result$view)[1], 22)
checkEqualsNumeric(dim(result$view)[2], 2)
checkEqualsNumeric(dim(result$view2)[1], 2)
checkEqualsNumeric(dim(result$view2)[2], 2)

view1 <- result$view
checkEqualsNumeric(view1[[4,2]], 0, tolerance=1e-5)
checkEqualsNumeric(view1[[5,2]], 1.5, tolerance=1e-5)

##########
# Test manager
##########
f <- rvle.open(file="test_simulation.vpz", pkg="test_port")

### manager  
result <- rvle.runManager(f)

##Change of behavior : was an error
#checkEqualsNumeric(dim(result)[1], 2)
#checkEqualsNumeric(dim(result)[2], 1)
#simres = result[[2,1]]
checkEqualsNumeric(dim(result)[1], 1, tolerance=1e-5)
checkEqualsNumeric(dim(result)[2], 2, tolerance=1e-5)
simres = result[[1,2]]
##End new behavior

checkEqualsNumeric(length(simres), 2, tolerance=1e-5)

simview = simres$view2
checkEqualsNumeric(dim(simview)[1], 1, tolerance=1e-5)
checkEqualsNumeric(dim(simview)[2], 2, tolerance=1e-5)

### manager matrix
result <- rvle.runManagerMatrix(f)

##Change of behavior : was an error
#valres = result[[2,1]][[1]][3,2]
#checkEqualsNumeric(valres, 0, tolerance=1e-5)
#valres = result[[2,1]][[1]][4,2]
#checkEqualsNumeric(valres, 2.5, tolerance=1e-5)
valres = result[[1,2]]$view[3,2]
checkEqualsNumeric(valres, 0, tolerance=1e-5)
valres = result[[1,2]]$view[4,2]
checkEqualsNumeric(valres, 2.5, tolerance=1e-5)
##End new behavior

##########
# Test error
##########

f <- rvle.open(file="test_error.vpz", pkg="test_port")

##experiment data 
checkEqualsNumeric(rvle.getBegin(f), 0, tolerance=1e-5)
rvle.setBegin(f,1)
checkEqualsNumeric(rvle.getBegin(f), 1, tolerance=1e-5)

##simulation error
r = rvle.run(f)
checkEquals(names(warnings())[[1]], "RVLE: error during simulation or empty results (check VLE_HOME/rvle.log)")


########
# Test Rvle class
########

f <- new("Rvle",file="test_simulation.vpz", pkg="test_port")

op = getDefault(f,"outputplugin")
checkEquals(op[["view"]],"vle.output/storage")
checkEquals(op[["view2"]],"vle.output/storage")
f = setDefault(f, outputplugin = c(view = "dummy"))
f = run(f, outputplugin = c(view = "storage"), cond.message = 6.2)
checkEqualsNumeric(results(f)$view$"Top model:Perturb.obsPort"[3],0,tolerance=1e-5)
op = getDefault(f,"outputplugin")
checkEquals(op[["view"]],"vle.output/dummy")
checkEquals(op[["view2"]],"vle.output/storage")
condval = getDefault(f,"cond.message")

##Change of behavior : these are lists
#checkEqualsNumeric(condval[1], 1.5, tolerance=1e-5)
#checkEqualsNumeric(condval[2], 2.5, tolerance=1e-5)
checkEqualsNumeric(condval[[1]], 1.5, tolerance=1e-5)
checkEqualsNumeric(condval[[2]], 2.5, tolerance=1e-5)
##End new behavior

c1 = getDefault(f,"cond.sendTime")
checkEqualsNumeric(c1,2.6, tolerance=1e-5)
f = setDefault(f, cond.sendTime = 2.8)
c1 = getDefault(f,"cond.sendTime")
checkEqualsNumeric(c1,2.8, tolerance=1e-5)

########
# Test Rvle class + Generic value handling + Simulations sequence 
########
f <- new("Rvle",file="test_simulation.vpz", pkg="test_port")

f = run(f, outputplugin = c(view = "storage"), cond.message.as_single = list(1,"hello"))
checkEquals(length(f@backup), 0)
checkEquals(class(results(f)$view[[5,2]]),"numeric")
checkEquals(is.na(results(f)$view[[5,2]][2]),TRUE)

f = run(f, outputplugin = c(view = "storage"), cond.message.as_single = list(1,"hello"),
        restype = "poly")
checkEquals(length(f@backup), 0)
checkEquals(class(results(f)$view[[5,2]]),"list")
checkEquals(results(f)$view[[5,2]][[2]],"hello")

f = run(f, outputplugin = c(view = "storage"), cond.message.as_single = matrix(c(1,2,5,4),nrow=2),
        restype = "poly")
checkEquals(class(results(f)$view[[5,2]]),"matrix")
checkEqualsNumeric(results(f)$view[[5,2]][1,2],5, tolerance=1e-5)

condval = getDefault(f,"cond.message")
checkEqualsNumeric(condval[[1]], 1.5, tolerance=1e-5)
checkEqualsNumeric(condval[[2]], 2.5, tolerance=1e-5)
res = getDefault(f,"restype")
checkEquals(res, "dataframe")


f <- new("Rvle",file="test_simulation.vpz", pkg="test_port")
t = matrix(1:20,nrow=10)
setDefault(f,cond.message.as_single=t)
checkEquals(class(getDefault(f,"cond.message")), "VleTABLE")
setDefault(f,cond.message=t)
checkEquals(class(getDefault(f,"cond.message")), "VleMULTIPLE_VALUES")

t = list(c1=rep(TRUE,3),c2=1:10)
setDefault(f,cond.message.as_single=t)
checkEquals(class(getDefault(f,"cond.message")), "VleMAP")
setDefault(f,cond.message=t)
checkEquals(class(getDefault(f,"cond.message")), "VleMULTIPLE_VALUES")

setDefault(f,cond.message.as_single="my message")
t = data.frame(matrix(c(1:20,20:1), ncol=2))
names(t) = c("cond.message","cond.sendTime")
t$cond.sendTime = as.double(t$cond.sendTime)#otherwise error
res = results(run(f,inputs=t, plan = 'linear'))
checkEquals(class(res), "matrix")
checkEquals(class(res[[1,20]]), "list")
checkEquals(length(res[[1,20]]), 2)
checkEquals(class(getDefault(f,"cond.message")), "character")




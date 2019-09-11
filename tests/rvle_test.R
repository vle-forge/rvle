#!/usr/bin/Rscript

library(rvle)

#tmpdir = "/tmp"
tmpdir = tempdir()
print(tmpdir)
dir.create(paste(tmpdir,"/vlehome",sep=""))
vlehome = normalizePath(paste(tmpdir,"/vlehome",sep=""))
Sys.setenv(VLE_HOME=vlehome)

currentdir = getwd();
unlink("./test_port/buildvle", recursive=TRUE, force = TRUE)
.rvle.compile_test_port()

setwd(currentdir)
unlink("./test_port/buildvle", recursive=TRUE, force = TRUE)


##########
# Test packages function
##########
pkgs = rvle.packages_list()
print(unlist(pkgs))
checkEquals(length(pkgs),5)
checkTrue("test_port" %in% pkgs)
checkTrue("vle.generic.builder" %in% pkgs)
checkTrue("vle.adaptative-qss" %in% pkgs)
checkTrue("vle.output" %in% pkgs)
checkTrue("gvle.default" %in% pkgs)
pkgContent = rvle.package_content("test_port")
print(pkgContent)
checkEquals(length(pkgContent),13)

##########
# Test conditions
##########

f <- rvle.open(file="test_vle.vpz",pkg="test_port")

cnd <- rvle.get_conditions(f)

checkEquals(length(cnd), 3)
checkTrue("test" %in% cnd)
checkTrue("simulation_engine" %in% cnd)
checkTrue("cond" %in% cnd)

# show port list
ports <- rvle.get_condition_ports(f, "test")

checkTrue("bool" %in% ports)
checkTrue("double" %in% ports)
checkTrue("int" %in% ports)
checkTrue("map" %in% ports)
checkTrue("string" %in% ports)
checkTrue("tuple" %in% ports)

# check the type of the port values vector
checkEquals(storage.mode(rvle.get_condition_port_value(f, "test", "string")),
        "character")
checkEquals(storage.mode(rvle.get_condition_port_value(f, "test", "double")),
        "double")
checkEquals(storage.mode(rvle.get_condition_port_value(f, "test", "int")),
        "integer")
checkEquals(storage.mode(rvle.get_condition_port_value(f, "test", "bool")),
        "logical")

# test tuples

tuple <- rvle.get_condition_port_value(f, "test", "tuple")
checkEquals(storage.mode(tuple), "double")
checkEquals(tuple[1], 1.0)
checkEquals(tuple[2], 2.0)
checkEquals(tuple[3], 3.0)
checkEquals(tuple[4], 4.0)
checkEquals(tuple[5], 5.0)
newtuple <- seq(length=14, from=6, to=19)
rvle.set_condition_port_value(f, "test", "tuple", newtuple)


tuple <- rvle.get_condition_port_value(f, "test", "tuple")
checkEquals(storage.mode(tuple), "double")
checkEquals(tuple[1], 6.0)
checkEquals(tuple[2], 7.0)
checkEquals(tuple[3], 8.0)
checkEquals(tuple[4], 9.0)

###### test map

mapval = rvle.get_condition_port_value(f,"test", "map")
checkEquals(storage.mode(mapval$a), "double")
checkEquals(storage.mode(mapval$b), "double")
notexistingval = rvle.get_condition_port_value(f,"test", "notexisting") 
checkEquals(class(notexistingval), "VleNIL")

### test simple types

c = rvle.get_condition_port_value(f,"test","bool")
checkEquals(class(c), "logical")##note: 'bool' are not typed 'VleBOOLEAN'
rvle.set_condition_port_value(f,"test","bool",c)
c = rvle.get_condition_port_value(f,"test","bool")
checkEquals(class(c), "logical")

c = rvle.get_condition_port_value(f,"test","double")
checkEquals(class(c), "numeric")##note: 'double' are not typed 'VleDOUBLE'
rvle.set_condition_port_value(f,"test","double",c)
c = rvle.get_condition_port_value(f,"test","double")
checkEquals(class(c), "numeric")

c = rvle.get_condition_port_value(f,"test","int")
checkEquals(class(c), "integer")
rvle.set_condition_port_value(f,"test","int",c)
c = rvle.get_condition_port_value(f,"test","int")
checkEquals(class(c), "integer")

##implicit conversion

c = list(1,2,3)
rvle.set_condition_port_value(f, "test", "bool",c)
c = rvle.get_condition_port_value(f, "test", "bool")
checkEquals(class(c),"VleSET")
checkEquals(storage.mode(c),"list")
checkEquals(length(c),3)
checkEquals(class(c[[1]]),"numeric")
checkEqualsNumeric(c[[1]],1)
checkEqualsNumeric(c[[2]],2)
checkEqualsNumeric(c[[3]],3)

c = c(4.2,5,6)
rvle.set_condition_port_value(f, "test", "bool",c)
c = rvle.get_condition_port_value(f, "test", "bool")
checkEquals(class(c),"VleTUPLE")
checkEquals(storage.mode(c),"double")
checkEquals(length(c),3)
checkEqualsNumeric(c[1],4.2)
checkEqualsNumeric(c[2],5)
checkEqualsNumeric(c[3],6)

c = list(id1=4.2,id2=5,id3=6)
rvle.set_condition_port_value(f, "test", "bool",c)
c = rvle.get_condition_port_value(f, "test", "bool")
checkEquals(class(c),"VleMAP")
checkEquals(storage.mode(c),"list")
checkEqualsNumeric(length(c),3)
checkEqualsNumeric(c$id1,4.2)
checkEqualsNumeric(c$id2,5)
checkEqualsNumeric(c$id3,6)

c = list(id1=1,3.6,3)#TODO to improve if only a part of names is given
rvle.set_condition_port_value(f, "test", "bool",c)
c = rvle.get_condition_port_value(f, "test", "bool")
checkEquals(class(c),"VleMAP")
checkEquals(storage.mode(c),"list")
checkEquals(length(c),2)
checkEquals(class(c[[1]]),"numeric")
checkEqualsNumeric(c[[1]],1)
checkEqualsNumeric(c[[2]],3)

c = list(id1=list("hello","hello2"),id2=c(1.2,5),id3=c(6), id4=9,
        id5=c("hello",1), id6=TRUE)
rvle.set_condition_port_value(f, "test", "bool",c)
cbis = rvle.get_condition_port_value(f, "test", "bool")
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
rvle.set_condition_port_value(f, "test", "bool",cbis)
cter = rvle.get_condition_port_value(f, "test", "bool")
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
rvle.set_condition_port_value(f, "test", "bool",c)
c = rvle.get_condition_port_value(f, "test", "bool")
checkEqualsNumeric(class(c),"VleTABLE")
checkEqualsNumeric(dim(c)[[1]],3)
checkEqualsNumeric(dim(c)[[2]],2)
checkEqualsNumeric(c[1,2],4)
rvle.set_condition_port_value(f, "test", "bool",c)
c = rvle.get_condition_port_value(f, "test", "bool")
checkEquals(class(c),"VleTABLE")
checkEqualsNumeric(dim(c)[[1]],3)
checkEqualsNumeric(dim(c)[[2]],2)
checkEquals(c[1,2],4)

c = data.frame(matrix(c(1,2,3,4,5,6),nrow=3))
c[1,1] = "hello"
rvle.set_condition_port_value(f, "test", "bool",c)
c = rvle.get_condition_port_value(f, "test", "bool")
checkEquals(class(c),"VleMATRIX")
checkEqualsNumeric(dim(c)[[1]],4, tolerance=1e-5)
checkEqualsNumeric(dim(c)[[2]],2, tolerance=1e-5)
checkEquals(c[[1,2]],"X2")
checkEquals(c[[4,1]],"3")
rvle.set_condition_port_value(f, "test", "bool",c)
c = rvle.get_condition_port_value(f, "test", "bool")
checkEquals(class(c),"VleMATRIX")
checkEqualsNumeric(dim(c)[[1]],4, tolerance=1e-5)
checkEquals(dim(c)[[2]],2, tolerance=1e-5)
checkEquals(c[[1,2]],"X2")
checkEquals(c[[4,1]],"3", tolerance=1e-5)

##########
# Test experiment data
##########

f <- rvle.open(file="test_vle.vpz",pkg="test_port")

#check the list of views
views <- rvle.get_views(f)
checkEquals(views[[1]], "view")
checkEquals(views[[2]], "view2")

#check the output plugin setting
checkEquals(rvle.get_view_plugin(f,"view"), "vle.output/storage")
rvle.set_view_plugin(f,"view","myplugin", "mypackage")
checkEquals(rvle.get_view_plugin(f,"view"), "mypackage/myplugin")


#check save
rvle.save(f,"__test_vle.vpz")
f = rvle.open("__test_vle.vpz")
checkEquals(rvle.get_view_plugin(f,"view"), "mypackage/myplugin")
rvle.set_view_plugin(f,"view","storage")

#manage views
obss = rvle.get_observables(f);
checkEquals(obss[[1]], "obs")
obsPorts = rvle.get_observable_ports(f, "obs")
checkEquals(obsPorts[[1]], "obsPort")
attachedViews = rvle.get_attached_views(f, "obs", "obsPort")
checkEquals(length(attachedViews), 2)
checkEquals(attachedViews[[2]], "view2")

rvle.add_observable_port(f, "obs","newPort")
obsPorts = rvle.get_observable_ports(f, "obs")
checkEquals(obsPorts[[1]], "newPort")
checkEquals(obsPorts[[2]], "obsPort")
rvle.del_observable_port(f, "obs","obsPort")
obsPorts = rvle.get_observable_ports(f, "obs")
checkEquals(obsPorts[[1]], "newPort")
checkEquals(length(rvle.get_attached_views(f, "obs", "newPort")), 0)
rvle.attach_view(f, "view", "obs", "newPort")
attachedViews = rvle.get_attached_views(f, "obs", "newPort")
checkEquals(attachedViews[[1]], "view")

##########
# Test simulation
##########

f <- rvle.open(file="test_vle.vpz", pkg="test_port")

### normal run 
result <- rvle.run(f)
checkEquals(class(result$view), "data.frame")

# check result of the view
checkEqualsNumeric(dim(result$view)[1], 11, tolerance=1e-5)
checkEqualsNumeric(dim(result$view)[2], 2, tolerance=1e-5)
checkEqualsNumeric(dim(result$view2)[1], 1, tolerance=1e-5)
checkEqualsNumeric(dim(result$view2)[2], 2, tolerance=1e-5)

view1 <- result$view
checkEquals(names(view1)[1], "time")
checkEquals(names(view1)[2], "Top model:Perturb.obsPort")

##########
# Test plan
##########

f <- rvle.open(file="test_vle.vpz",pkg="test_port")

## set inputs
rvle.plan_input(f, "cond", "sendTime", c(1.5,2.5,3.5))
rvle.plan_input(f, "cond", "message", c(9,8,7))
rvle.plan_output(f, id="out", path="view/Top model:Perturb.obsPort")
res = rvle.plan_run(f)

checkEqualsNumeric(res$out[2,1], 0, tolerance=1e-5)
checkEqualsNumeric(res$out[3,1], 9, tolerance=1e-5)
checkEqualsNumeric(res$out[3,2], 0, tolerance=1e-5)
checkEqualsNumeric(res$out[4,2], 8, tolerance=1e-5)
checkEqualsNumeric(res$out[4,3], 0, tolerance=1e-5)
checkEqualsNumeric(res$out[5,3], 7, tolerance=1e-5)

#rvleExp.plot(res)

##########
# Test error
##########


f <- rvle.open(file="test_vle.vpz", pkg="test_port")
res = NULL
rvle.plan_input(f, "cond", "sendTime", c(1.5,2.5,3.5))
rvle.plan_propagate(f, "cond", "message", "errorDuringAggregation")
rvle.plan_output(f, id="out", path="view/Top model:Perturb.obsPort")
res = rvle.plan_run(f)
checkEquals(is.na(res), TRUE)






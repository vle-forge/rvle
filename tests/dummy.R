library(RUnit)
library(rvle)
f <- rvle.open('dummy.vpz')

cnd <- rvle.listConditions(f)

checkEquals(cnd[1], "test")

# check the type of the port values vector
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "string")),
                "character")
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "double")),
                "double")
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "int")),
                "integer")
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "bool")),
                "logical")
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test",
                                "multi_string")), "character")
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test",
                                "multi_double")), "double")
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "multi_int")),
                "integer")
checkEquals(storage.mode(rvle.getConditionPortValues(f, "test", "multi_bool")),
                "logical")

tuple <- rvle.getConditionPortValues(f, "test", "tuple")
checkEquals(storage.mode(tuple), "list")
checkEquals(tuple[[1]][1], 1.0)
checkEquals(tuple[[1]][2], 2.0)
checkEquals(tuple[[1]][3], 3.0)
checkEquals(tuple[[1]][4], 4.0)
checkEquals(tuple[[1]][5], 5.0)

newtuple <- seq(length=14, from=6, to=19)
rvle.setTupleCondition(f, "test", "tuple", newtuple)
tuple <- rvle.getConditionPortValues(f, "test", "tuple")
checkEquals(storage.mode(tuple), "list")
checkEquals(tuple[[1]][1], 6.0)
checkEquals(tuple[[1]][2], 7.0)
checkEquals(tuple[[1]][3], 8.0)
checkEquals(tuple[[1]][4], 9.0)

tuple <- rvle.getConditionPortValues(f, "test", "multi_tuple")
checkEquals(storage.mode(tuple), "list")
checkEquals(tuple[[1]][1], 0.0)
checkEquals(tuple[[1]][2], 1.0)
checkEquals(tuple[[1]][3], 2.0)
checkEquals(tuple[[2]][1], 3.0)
checkEquals(tuple[[2]][2], 4.0)
checkEquals(tuple[[2]][3], 5.0)

# check the case of unmanaged type
checkException(rvle.getConditionPortValues(f,"test", ",notmanaged"))

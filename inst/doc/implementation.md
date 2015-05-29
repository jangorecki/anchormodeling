# Implementation notes

Anchor Modeling `AM` instance is an R6 reference class objects, it stores some metadata and `AM$data` data.table storing anchor modeling entities with their attributes. Those are called here `AMobj`, which is simply a superclass for classes `anchor`, `attribute`, `tie`, `knot`, all those are stored in `obj` column: `AM$data$obj`. The data of each entity is stored in `AM$data$obj[[i]]$data`.  
*AM* class is a anchor modeling metadata manager, while *AMobj* objects are the entities in model.  
There is one more class called *IM* Identity Management which serves surrogate key generation and natural key storage. By default it is automatically used on `AM$load` method based on *natural keys* provided in `mapping` argument.  

For the *difference* views the sqlserver *cross apply* or postgres *cross join lateral* has been substituted by data.table *rolling join*. Distinct union of all historized attributes (fields: ID and timestamp) which presents all combinations of PK is joined to each attribute using *rolling join*. The code responsible for that you can find in `AM$joinv` method.  

Additionally, *rolling join* has been used to handle *restatement*/*idempotency* while loading data into AM instance. There are two rolling joins, one vs past `roll = +Inf` and one vs future `roll = -Inf`. Can be found in `AMobj$load` method.  

Joining in data.table is performed on clustered keys which makes rolling joins very efficient.  

Saving AM instance to off-memory storage, for later loading and operating on it, is working, but was not deeply tested.  
User may in fact use the flexibility which gives Anchor Modeling. Besides of keeping the latest AM instance, it is simple to keep full scripts which builds the current state of AM, including data from their mirrors. An optional unit test and *continuous integration* for auto deployment is in place.  

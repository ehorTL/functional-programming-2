module Demo where 

import Integration


-- one-thread (x)
integrateTest2 = integrateWithEps (\x-> x) 0 5 5 0.000001

-- one-thread calc (sin)
integrateTest3 = integrateWithEps sin 0 pi 100000 0.000001

-- parallel calculations (sin)
integrateTest3P = integrateWithEpsPar sin 0 pi 100000 0.000001

-- logging calculations (sin)
integrateTest3M = integrateWithEpsM sin 0 pi 20 0.000001 100

-- one-thread x^2
integrateTest4 = integrateWithEps (\x -> x^2) 1 5 100000 0.000001
 
-- integrateTest5PP = integrateWithEpsParAll sin 0 pi 100000 0.000001 2
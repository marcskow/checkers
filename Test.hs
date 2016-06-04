task a = a + 3
genTree t = t + 2

tree =
    let test = genTree t where t = task a where a = 5 in test + 2
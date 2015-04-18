# Changelog

## 0.3.0
- upgraded Choco from 3.1.0 to 3.3.0; resolved breaking changes but no new features
- now depends on Maven Central artifact, which should be more reliable
- now relies on JDK 8
- `solution` no longer needs or supports `:feasible` argument
- added `$knapsack` global constraint

## 0.2.1
- fixed bug in which ($comparison number var) would switch the order of the args

## 0.2.0
- changed `$circuit?` to `$circuit`
- changed `$all-different?` to `$distinct`

## 0.1.1
- fixed a problem with `$scalar` that made the outputted variable's domain too constricted

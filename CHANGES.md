# Change Log

## 0.4.4 *2023-1-28*
* addressing warnings found by cargo-audit
* adjusted the benchmark documentation in light of changes
## 0.4.3 *2021-7-3*
* some modernization
## 0.4.2 *2021-3-30*
* reverting ill-advised removal of a `mut`
## 0.4.1 *2021-3-28*
* switch to MIT license
## 0.3.0
* fixed bug whereby specific `(?-b)` was not overriding general `(?b)`
## 0.3.1
* fixed bug where grammar-wide flags weren't interacting correctly with rule-specific flags
## 0.4.0
* added serde and associated dependencies to make matchers serializable
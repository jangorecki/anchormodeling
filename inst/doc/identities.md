# Identity Management design notes

- [ ] entities for which produce identities provided as character vector
  - [ ] mne of anchors
  - [ ] mne of knots
- [ ] their natural key / source columns provided as list of character vectors
  - [ ] both anchors and knots can have multiple src columns on input
    - [ ] composite natural key of anchors
    - [ ] shared knots
  - [ ] output columns in IM storage
    - [ ] anchors keep their composite key as multiple columns
    - [ ] knots union multiple columns into single column

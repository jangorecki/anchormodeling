# Identity Management design notes

- [x] auto create new definition for new mnemonics
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
      - [ ] union of knots outside of IM, in AM?
- [x] natural key / source column stored with ID data will be named after first definition of mnemonic
  - [x] dynamic name remapping available when `nk` provided

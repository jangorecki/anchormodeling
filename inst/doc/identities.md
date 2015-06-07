# Identity Management design notes

- [x] auto create new definition for new mnemonics
- [x] entities for which produce identities provided as character vector
  - [x] mne of anchors
  - [x] mne of knots
  - [x] detect based on naming convention
- [x] their natural key / source columns provided as list of character vectors
  - [x] both anchors and knots can have multiple src columns on input
    - [x] composite natural key of anchors
    - [x] shared knots
  - [x] output columns in IM storage
    - [x] anchors keep their composite key as multiple columns
    - [x] knots union multiple columns into single column
      - [x] union of shared knots in IM and ID returned for each of the source cols
- [x] natural key / source column stored with ID data will be named after first definition of mnemonic
  - [x] dynamic name remapping available when `nk` provided

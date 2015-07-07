# Querying data from DWH

- [x] for each anchor find all attributes
- [ ] for each child attribute
  - [x] query it in appropriate view  *current*, *last*, *point-in-time*, *difference*
  - [x] lookup knot if knotted
  - [x] rename knot to prefix attribute/tie code
- [ ] join anchor with all attributes
  - [x] *current*, *last*, *point-in-time* as simple tail by key with filter on time
  - [x] allow.cartesian only on *difference* views
- [ ] joining *difference* view using
  - [x] build *ID-TIME* union distinct of each attribute *ID* and *changedAt* fields
  - [ ] rolling join of ID-TIME table to all attributes to get latest attribute value for each combination of time
    - [ ] handle `POSIXct` to `Date` types rolling join?
- [x] tie query
  - [x] lookup knot if knotted
  - [x] rename knot to prefix anchor/knot role
- [ ] temporal filter rows which did not exists in DWH at the point-in-time, future anchors

# lissyrtools (beta version)

# lissyrtools 0.1.5

## Bug fixes

* `transform_equivalise_oecd()` and respectively `implement_equivalise_oecd()` were now corrected for the way they classify children in the argument `value_children`.  Previously this argument was dependent on LIS variable `nhhmem17`, now it uses `nhhmem13`.

# Ops
- [x] FitOp
- [x] TimesOp
- [x] TimesOp*
- [x] SpliceOp
- [x] RepOp
- [x] RepOp*
- [x] RevOp
- [x] Cyclop
- [x] MaybeOp
- [x] MaybeOp*
- [x] EuclidOp
- [x] PickOp
- [x] ElongateOp
- [x] ElongateOp*
- [ ] StackOp


# Sqeezers
Fit into single segment per unit of weight

## Naked

[Sequence]
FitOp

## Weighted Squeeze

Elongate
Splice = Elongate where x = 1

## xform Sqeeze

Times
FitOp = TimesOp where n = 1 | RepOp where n = 1

## xform weighted squeeze

Rep

# Stretchers
Stretch across segments and cycles

## Naked
SlowOp
CyclOp = SlowOp where x = sum-weights children

# Post operate xf
RevOp

# Event generator
Euclid
Sine?
Rand?

# Fn Wrapper
Pick
Maybe

# Heigher Order
Stack op
PatOps

# (cyc|ops)

## Patterns + Ops
Op fns return declarative data structures with `operate` implementations rather than performing their transformation immediately, because it seems much simpler to perform Uzu-style timing operations top down, passing down context from the parent (though getting weights from the children bottom up).

## Cycls

Cycls are collections of timed `Events`.

## Realization

### Timing

### Deferral implications

## `Ops` namespace

This namespace is designed to be consumed whole-hog by the user, and optimizes for fast typing and DevEx for live coding. This, good architectural design like descriptive names and simplicity over convenience are secondary to optimizing for live-coding.

Hopefully, making these compromises here, prevents them from leaking into lower-level code like `pattern`, `merge`, `events`.

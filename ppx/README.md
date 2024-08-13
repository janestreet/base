# ppx_base_internal

This ppx is intended only for use inside Base itself, to help write certain repeated code
patterns. It is not designed for use in other contexts. For example, it may refer to
internal names like `Bool0`, or be specialized to use cases in `Base` that are not
applicable to other code.

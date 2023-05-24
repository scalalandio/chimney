package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Definitions
    extends Types
    with Exprs
    with Stmts
    with Fresh
    with ChimneyTypes
    with ChimneyExprs
    with Configurations
    with Contexts
    with Results

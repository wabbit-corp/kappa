module ResourceModelTests

open Kappa.Compiler
open Kappa.Compiler.ResourceModel
open Xunit

[<Fact>]
let ``resource quantities keep borrow mode distinct from intervals`` () =
    let one = ResourceQuantity.ofSurface QuantityOne
    let borrow = ResourceQuantity.ofSurface (QuantityBorrow None)
    let omega = ResourceQuantity.ofSurface QuantityOmega

    Assert.True(ResourceQuantity.isExactOne one)
    Assert.True(ResourceQuantity.isBorrow borrow)
    Assert.False(ResourceQuantity.isBorrow one)
    Assert.False(ResourceQuantity.isInterval borrow)
    Assert.Equal("1", ResourceQuantity.toSurfaceText one)
    Assert.Equal("&", ResourceQuantity.toSurfaceText borrow)
    Assert.Equal("\u03c9", ResourceQuantity.toSurfaceText omega)

[<Fact>]
let ``resource quantity satisfaction follows interval and borrow rules`` () =
    let zero = ResourceQuantity.ofSurface QuantityZero
    let one = ResourceQuantity.ofSurface QuantityOne
    let omega = ResourceQuantity.ofSurface QuantityOmega
    let borrow = ResourceQuantity.ofSurface (QuantityBorrow None)

    Assert.True(ResourceQuantity.satisfies omega one)
    Assert.True(ResourceQuantity.satisfies omega (ResourceQuantity.exact 2))
    Assert.True(ResourceQuantity.satisfies borrow borrow)
    Assert.True(ResourceQuantity.satisfies borrow omega)
    Assert.False(ResourceQuantity.satisfies one borrow)
    Assert.False(ResourceQuantity.satisfies one (ResourceQuantity.exact 2))
    Assert.False(ResourceQuantity.satisfies borrow one)
    Assert.False(ResourceQuantity.satisfies zero one)

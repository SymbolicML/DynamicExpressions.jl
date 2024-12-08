module OperatorDerivativeModule

using DifferentiationInterface: derivative, AutoZygote

struct OperatorDerivative{F,degree,arg,backend} <: Function
    op::F
end

function Base.show(io::IO, g::OperatorDerivative{F,degree,arg}) where {F,degree,arg}
    print(io, "∂")
    if degree == 2
        if arg == 1
            print(io, "₁")
        elseif arg == 2
            print(io, "₂")
        end
    end
    print(io, g.op)
    return nothing
end
Base.show(io::IO, ::MIME"text/plain", g::OperatorDerivative) = show(io, g)

# function _zygote_gradient(op::F, ::Val{2}, ::Val{side}=Val(nothing)) where {F,side}

function make_derivative(
    op::F, ::Val{degree}, ::Val{arg}=Val(nothing); v_backend::Val{backend}=Val(AutoZygote())
) where {F,degree,arg,backend}
    return OperatorDerivative{F,degree,arg,backend}(op)
end

function (g::OperatorDerivative{F,1,1,backend})(x) where {F,backend}
    # out = only(gradient(g.op, x))
    # return out === nothing ? zero(x) : out
    return derivative(g.op, x, backend)
end
function (g::OperatorDerivative{F,2,nothing})(x, y) where {F}
    (∂x, ∂y) = gradient(g.op, x, y)
    return (∂x === nothing ? zero(x) : ∂x, ∂y === nothing ? zero(y) : ∂y)
end
function (g::OperatorDerivative{F,2,1})(x, y) where {F}
    ∂x = only(gradient(Base.Fix2(g.op, y), x))
    return ∂x === nothing ? zero(x) : ∂x
end
function (g::OperatorDerivative{F,2,2})(x, y) where {F}
    ∂y = only(gradient(Base.Fix1(g.op, x), y))
    return ∂y === nothing ? zero(y) : ∂y
end

end

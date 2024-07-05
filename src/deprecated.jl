import Base: @deprecate
import .NodeModule: Node, GraphNode

@deprecate simplify_tree(tree, operators) simplify_tree!(tree, operators)

for N in (:Node, :GraphNode)
    @eval begin
        function $N(d::Integer, c::Bool, v::T) where {T}
            Base.depwarn(
                string($N) *
                "(d, c, v) is deprecated. Use " *
                string($(N)) *
                "{T}(val=v) instead.",
                $(Meta.quot(N)),
            )
            @assert d == 1
            @assert c == true
            return $N{T}(; val=v)
        end
        function $N(::Type{T}, d::Integer, c::Bool, v::_T) where {T,_T}
            Base.depwarn(
                string($N) *
                "(T, d, c, v) is deprecated. Use " *
                string($(N)) *
                "{T}(val=v) instead.",
                $(Meta.quot(N)),
            )
            @assert d == 1
            @assert c == true
            return $N{T}(; val=v)
        end
        function $N(::Type{T}, d::Integer, c::Bool, ::Nothing, f::Integer) where {T}
            Base.depwarn(
                string($N) *
                "(T, d, c, v, f) is deprecated. Use " *
                string($(N)) *
                "{T}(feature=f) instead.",
                $(Meta.quot(N)),
            )

            @assert d == 1
            @assert c == false
            return $N{T}(; feature=f)
        end
        function $N(d::Integer, ::Bool, ::Nothing, ::Integer, o::Integer, l::$N)
            Base.depwarn(
                string($N) *
                "(d, c, v, f, o, l) is deprecated. Use " *
                string($(N)) *
                "(op=o, l=l) instead.",
                $(Meta.quot(N)),
            )
            @assert d == 1
            return $N(; op=o, l=l)
        end
        function $N(
            d::Integer, ::Bool, ::Nothing, ::Integer, o::Integer, l::$N{T}, r::$N{T}
        ) where {T}
            Base.depwarn(
                string($N) *
                "(d, c, v, f, o, l, r) is deprecated. Use " *
                string($(N)) *
                "(op=o, l=l, r=r) instead.",
                $(Meta.quot(N)),
            )
            @assert d == 2
            return $N(; op=o, l=l, r=r)
        end
    end
end

Base.@deprecate_binding EquationModule NodeModule
Base.@deprecate_binding EquationUtilsModule NodeUtilsModule
Base.@deprecate_binding EvaluateEquationModule EvaluateModule
Base.@deprecate_binding EvaluateEquationDerivativeModule EvaluateDerivativeModule
Base.@deprecate_binding SimplifyEquationModule SimplifyModule

@deprecate(
    count_constants(tree::Union{AbstractExpression,AbstractExpressionNode}),
    count_constant_nodes(tree)
)
@deprecate(
    get_constants(tree::Union{AbstractExpression,AbstractExpressionNode}),
    get_scalar_constants(tree)
)
@deprecate(
    set_constants!(tree::Union{AbstractExpression,AbstractExpressionNode}, constants, refs),
    set_scalar_constants!(tree, constants, refs)
)

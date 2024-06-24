@testitem "Test if we can create a multi-expression expression type." begin
    using DynamicExpressions
    using DynamicExpressions: DynamicExpressions as DE
    using DynamicExpressions: Metadata, ExpressionInterface
    using Interfaces: Interfaces, test, @implements, Arguments

    struct MultiScalarExpression{
        T,N<:AbstractExpressionNode{T},TREES<:NamedTuple,D<:NamedTuple
    } <: AbstractExpression{T,N}
        trees::TREES
        metadata::Metadata{D}

        function MultiScalarExpression(trees::NamedTuple, metadata::Metadata{D}) where {D}
            example_tree = first(values(trees))
            N = typeof(example_tree)
            T = eltype(example_tree)
            return new{T,N,typeof(trees),D}(trees, metadata)
        end

        """
        Create a multi-expression expression type.

        The `tree_factory` is a function that takes the trees by keyword argument,
        and stitches them together into a single tree (for printing or evaluation).
        """
        function MultiScalarExpression(
            trees::NamedTuple; tree_factory::F, operators, variable_names
        ) where {F<:Function}
            example_tree = first(values(trees))
            N = typeof(example_tree)
            T = eltype(example_tree)
            @assert all(t -> eltype(t) == T, values(trees))
            metadata = (; tree_factory, operators, variable_names)
            return new{T,N,typeof(trees),typeof(metadata)}(trees, Metadata(metadata))
        end
    end

    operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[sin, cos, exp]
    )
    variable_names = ["a", "b", "c"]

    ex1 = @parse_expression(c * 2.5 - cos(a), operators, variable_names)
    ex2 = @parse_expression(b * b * b + c / 0.2, operators, variable_names)

    multi_ex = MultiScalarExpression(
        (; f=ex1.tree, g=ex2.tree);
        tree_factory=(; f, g) -> :($f + cos($g)),
        # TODO: Can we have a custom evaluation routine here, to enable aggregations in the middle part?
        operators,
        variable_names,
    )

    # Verify that the unimplemented methods raise an error
    if VERSION >= v"1.9"
        @test_throws "`get_operators` function must be implemented for" DE.get_operators(
            multi_ex, nothing
        )
        @test_throws "`get_variable_names` function must be implemented for" DE.get_variable_names(
            multi_ex, nothing
        )
        @test_throws "`get_tree` function must be implemented for" DE.get_tree(multi_ex)
        @test_throws "`copy` function must be implemented for" copy(multi_ex)
        @test_throws "`get_constants` function must be implemented for" get_constants(
            multi_ex
        )
        @test_throws "`set_constants!` function must be implemented for" set_constants!(
            multi_ex, nothing, nothing
        )
    end

    tree_factory(f::F, trees) where {F} = f(; trees...)
    function DE.get_contents(ex::MultiScalarExpression)
        return ex.trees
    end
    function DE.get_metadata(ex::MultiScalarExpression)
        return ex.metadata
    end
    function DE.get_tree(ex::MultiScalarExpression{T,N}) where {T,N}
        fused_expression = parse_expression(
            tree_factory(ex.metadata.tree_factory, ex.trees)::Expr;
            calling_module=@__MODULE__,  # TODO: Not needed
            operators=DE.get_operators(ex, nothing),
            variable_names=nothing,
            node_type=N,
            expression_type=Expression,
        )::Expression{T,N}
        return fused_expression.tree
    end
    function DE.get_operators(
        ex::MultiScalarExpression, operators::Union{AbstractOperatorEnum,Nothing}=nothing
    )
        return operators === nothing ? ex.metadata.operators : operators
    end
    function DE.get_variable_names(
        ex::MultiScalarExpression,
        variable_names::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
    )
        return variable_names === nothing ? ex.metadata.variable_names : variable_names
    end
    function Base.copy(ex::MultiScalarExpression)
        t = NamedTuple{keys(ex.trees)}(map(copy, values(ex.trees)))
        m = ex.metadata
        return MultiScalarExpression(
            t;
            tree_factory=m.tree_factory,
            operators=copy(m.operators),
            variable_names=copy(m.variable_names),
        )
    end
    function Base.:(==)(ex1::MultiScalarExpression, ex2::MultiScalarExpression)
        return isempty(Base.structdiff(ex1.trees, ex2.trees)) &&
               all(i -> ex1.trees[i] == ex2.trees[i], keys(ex1.trees)) &&
               ex1.metadata == ex2.metadata
    end

    s = sprint((io, ex) -> show(io, MIME"text/plain"(), ex), multi_ex)

    @test s == "((c * 2.5) - cos(a)) + cos(((b * b) * b) + (c / 0.2))"

    s = sprint((io, ex) -> print_tree(io, ex), multi_ex)

    @test s == "((c * 2.5) - cos(a)) + cos(((b * b) * b) + (c / 0.2))\n"

    @implements ExpressionInterface MultiScalarExpression [Arguments()]
    test(ExpressionInterface, MultiScalarExpression, [multi_ex])
end

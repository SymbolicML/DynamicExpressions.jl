@testitem "Plain Node bytes survive Distributed loading" begin
    using DynamicExpressions, Serialization
    tree = Node{Float64}(;
        op=1, children=(Node{Float64}(; val=-0.0), Node{Float64}(; feature=2))
    )
    before = IOBuffer()
    serialize(before, tree)
    original_bytes = take!(before)
    @eval using Distributed
    after = IOBuffer()
    serialize(after, tree)
    @test take!(after) == original_bytes
end

@testitem "Distributed Node serialization" begin
    using Distributed, Serialization
    using DynamicExpressions
    using DynamicExpressions: get_child, get_tree

    function same_tree(a::Node{T,D}, b::Node{T,D}) where {T,D}
        pending = [(a, b)]
        while !isempty(pending)
            x, y = pop!(pending)
            @test x.degree == y.degree
            if x.degree == 0
                @test x.constant == y.constant
                if x.constant
                    @test isequal(x.val, y.val)
                else
                    @test x.feature == y.feature
                end
            else
                @test x.op == y.op
                for i in 1:Int(x.degree)
                    push!(pending, (get_child(x, i), get_child(y, i)))
                end
            end
        end
    end

    function roundtrip(tree, workers)
        first = remotecall_fetch(getindex, workers[1], (tree,), 1)
        second = remotecall_fetch(getindex, workers[2], (first,), 1)
        return first, second
    end

    workers = addprocs(2; exeflags="--project=$(Base.active_project())")
    try
        for pid in workers
            fetch(
                Distributed.remotecall_eval(
                    Main, pid, :(using DynamicExpressions, Distributed)
                ),
            )
        end

        @test Base.get_extension(DynamicExpressions, :DynamicExpressionsDistributedExt) !==
            nothing
        for T in (Float32, Float64, ComplexF64)
            leaf = Node{T}(; feature=65535)
            constants = [Node{T}(; val=x) for x in (zero(T), -zero(T), T(Inf), T(NaN))]
            trees = [leaf; constants]
            push!(trees, Node{T}(; op=255, children=(leaf,)))
            push!(trees, Node{T}(; op=254, children=(constants[1], leaf)))
            for tree in trees
                for result in roundtrip(tree, workers)
                    @test typeof(result) === typeof(tree)
                    same_tree(tree, result)
                end
            end
        end

        string_tree = Node{String}(;
            op=3,
            children=(
                Node{String}(; val="left"),
                Node{String}(;
                    op=4, children=(Node{String}(; feature=2), Node{String}(; val="right"))
                ),
            ),
        )
        for result in roundtrip(string_tree, workers)
            @test typeof(result) === typeof(string_tree)
            same_tree(string_tree, result)
        end

        ternary = Node{Float64,3}(;
            op=3,
            children=(
                Node{Float64,3}(; feature=1),
                Node{Float64,3}(; val=-0.0),
                Node{Float64,3}(;
                    op=2,
                    children=(Node{Float64,3}(; feature=2), Node{Float64,3}(; val=Inf)),
                ),
            ),
        )
        for result in roundtrip(ternary, workers)
            @test typeof(result) === typeof(ternary)
            same_tree(ternary, result)
        end

        deep = Node{Float64}(; feature=1)
        for _ in 1:5000
            deep = Node{Float64}(; op=1, children=(deep,))
        end
        for result in roundtrip(deep, workers)
            same_tree(deep, result)
        end

        operators = OperatorEnum(; binary_operators=[+], unary_operators=[sin])
        expression = Expression(ternary; operators, variable_names=["x", "y"])
        for result in roundtrip(expression, workers)
            same_tree(get_tree(expression), get_tree(result))
        end
    finally
        rmprocs(workers)
    end
end

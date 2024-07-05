
using DynamicExpressions
using DynamicExpressions:
    Node,
    @extend_operators,
    OperatorEnum,
    get_constants,
    append_number_constants!,
    pop_number_constants,
    count_number_constants

# Max2Tensor (Tensor with a maximum of 3 dimensions) - struct that contains all three datatypes
mutable struct Max2Tensor{T}
    dims::UInt8 # number of dimmentions
    scalar::T
    vector::Vector{T}
    matrix::Matrix{T}
    Max2Tensor{T}() where {T} = new(Int8(0), zero(T), T[], Array{T,2}(undef, 0, 0))
    function Max2Tensor{T}(scalar::W) where {T,W<:Number}
        return new(Int8(0), Base.convert(T, scalar), T[], Array{T,2}(undef, 0, 0))
    end
    function Max2Tensor{T}(vector::Vector{W}) where {T,W<:Number}
        return new(Int8(1), zero(T), Vector{T}(vector), Array{T,2}(undef, 0, 0))
    end
    function Max2Tensor{T}(matrix::Matrix{W}) where {T,W<:Number}
        return new(Int8(2), zero(T), T[], Matrix{T}(matrix))
    end
end

using DynamicExpressions: is_valid, is_valid_array

function DynamicExpressions.is_valid(val::T) where {Q<:Number,T<:Max2Tensor{Q}}
    if val.dims == 0
        return is_valid(val.scalar)
    elseif val.dims == 1
        return is_valid_array(val.vector)
    end
    return is_valid_array(val.matrix)
end

function Base.:(==)(x::Max2Tensor{T}, y::Max2Tensor{T}) where {T}
    if x.dims !== y.dims
        return false
    elseif x.dims == 0
        return x.scalar == y.scalar
    elseif val.dims == 1
        return x.vector == y.vector
    end
    return x.matrix == y.matrix
end

function DynamicExpressions.count_number_constants(val::T) where {BT,T<:Max2Tensor{BT}}
    if val.dims == 0
        return 1
    elseif val.dims == 1
        return length(val.vector)
    end
    return length(val.matrix)
end

function DynamicExpressions.append_number_constants!(
    nvals::AbstractVector{BT}, idx::Int64, val::T
) where {BT<:Number,T<:Max2Tensor{BT}}
    if val.dims == 0
        nvals[idx] = val.scalar
        return idx + 1
    elseif val.dims == 1
        @view(nvals[idx:(idx + length(val.vector) - 1)]) .= val.vector
        return idx + length(val.vector)
    end
    @view(nvals[idx:(idx + length(val.matrix) - 1)]) .= reshape(
        val.matrix, length(val.matrix)
    )
    return idx + length(val.matrix)
end

function DynamicExpressions.pop_number_constants(
    nvals::AbstractVector{BT}, idx::Int64, val::T
) where {BT<:Number,T<:Max2Tensor{BT}}
    if val.dims == 0
        val.scalar = nvals[idx]
        return idx + 1, val
    elseif val.dims == 1
        val.vector .= @view(nvals[idx:(idx + length(val.vector) - 1)])
        return idx + length(val.vector), val
    end
    reshape(val.matrix, length(val.matrix)) .= @view(
        nvals[idx:(idx + length(val.matrix) - 1)]
    )
    return idx + length(val.matrix), val
end

# testing is_valid functions
@test is_valid(Max2Tensor{Float64}())
@test !is_valid(Max2Tensor{Float64}(NaN))
@test is_valid_array([Max2Tensor{Float64}(1), Max2Tensor{Float64}([1, 2, 3])])
@test !is_valid_array([Max2Tensor{Float64}(1), Max2Tensor{Float64}([1, 2, NaN])])

# dummy operators
q(x::Max2Tensor{T}) where {T} = Max2Tensor{T}(x.scalar)
a(x::Max2Tensor{T}, y::Max2Tensor{T}) where {T} = Max2Tensor{T}(x.scalar + y.scalar)

operators = OperatorEnum(; binary_operators=[a], unary_operators=[q])
@extend_operators(operators, on_type = Max2Tensor{Float64})

Base.invokelatest(
    () -> begin

        # test operator extended operators
        @test hasmethod(q, Tuple{Node{Max2Tensor{Float64}}})
        @test hasmethod(a, Tuple{Max2Tensor{Float64},Node{Max2Tensor{Float64}}})
        @test hasmethod(a, Tuple{Node{Max2Tensor{Float64}},Node{Max2Tensor{Float64}}})
        @test !hasmethod(a, Tuple{Float64,Node{Float64}})
        @test !hasmethod(a, Tuple{Node{Max2Tensor{Float32}},Node{Max2Tensor{Float32}}})

        tree = a(Node{Max2Tensor{Float64}}(; feature=1), Max2Tensor{Float64}(3.0))
        results = tree(
            [Max2Tensor{Float64}(1.0) Max2Tensor{Float64}(2.0) Max2Tensor{Float64}(3.0)]
        )
        @test results ==
            [Max2Tensor{Float64}(4), Max2Tensor{Float64}(5), Max2Tensor{Float64}(6)]

        c1 = Node(Max2Tensor{Float64}; val=Max2Tensor{Float64}([1, 2, 3]))
        c2 = Node(Max2Tensor{Float64}; val=Max2Tensor{Float64}(4))
        c3 = Node(Max2Tensor{Float64}; val=Max2Tensor{Float64}(5))
        c4 = Node(Max2Tensor{Float64}; val=Max2Tensor{Float64}([6 7 8; 9 10 11; 12 13 14]))
        c5 = Node(Max2Tensor{Float64}; val=Max2Tensor{Float64}(15))
        c6 = Node(Max2Tensor{Float64}; val=Max2Tensor{Float64}([16, 17, 18]))
        x1 = Node(Max2Tensor{Float64}; feature=1)
        x2 = Node(Max2Tensor{Float64}; feature=2)
        x3 = Node(Max2Tensor{Float64}; feature=3)
        x4 = Node(Max2Tensor{Float64}; feature=4)
        x5 = Node(Max2Tensor{Float64}; feature=5)
        x6 = Node(Max2Tensor{Float64}; feature=6)
        tree = a(
            a(a(a(x1, c1), q(a(x2, c2))), q(a(x3, c3))),
            q(a(a(q(a(x4, c4)), a(x5, c5)), q(a(x6, c6)))),
        )

        constants, refs = get_constants(tree, Float64)
        # matrix is put into the array by columns
        @test constants == [
            1.0,
            2.0,
            3.0,
            4.0,
            5.0,
            6.0,
            9.0,
            12.0,
            7.0,
            10.0,
            13.0,
            8.0,
            11.0,
            14.0,
            15.0,
            16.0,
            17.0,
            18.0,
        ]
        set_constants!(tree, constants .+ 5.0, refs)
        @test get_constants(tree, Float64)[1] == constants .+ 5.0
    end,
)

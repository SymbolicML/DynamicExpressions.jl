
using DynamicExpressions
using DynamicExpressions: Node, @extend_operators, OperatorEnum

# SVM (scalar, vector, matrix) - struct that contains all three datatypes
struct SVM{T}
    dims::Int8 # number of dimmentions
    scalar::T
    vector::Vector{T}
    matrix::Matrix{T}
    SVM{T}() where {T} = new(Int8(0), zero(T), T[], Array{T,2}(undef, 0, 0))
    function SVM{T}(scalar::W) where {T,W<:Number}
        return new(Int8(0), Base.convert(T, scalar), T[], Array{T,2}(undef, 0, 0))
    end
    function SVM{T}(vector::Vector{W}) where {T,W<:Number}
        return new(Int8(1), zero(T), Vector{T}(vector), Array{T,2}(undef, 0, 0))
    end
    function SVM{T}(matrix::Matrix{W}) where {T,W<:Number}
        return new(Int8(2), zero(T), T[], Matrix{T}(matrix))
    end
end

using DynamicExpressions: is_valid, is_valid_array

function DynamicExpressions.is_valid(val::T) where {Q<:Number,T<:SVM{Q}}
    if val.dims == 0
        is_valid(val.scalar)
    elseif val.dims == 1
        is_valid_array(val.vector)
    elseif val.dims == 2
        is_valid_array(val.matrix)
    end
end

function Base.:(==)(x::SVM{T}, y::SVM{T}) where {T}
    if x.dims !== y.dims
        return false
    elseif x.dims == 0
        return x.scalar == y.scalar
    elseif val.dims == 1
        return x.vector == y.vector
    elseif val.dims == 2
        return x.matrix == y.matrix
    end
end

# testing is_valid functions
@test is_valid(SVM{Float64}())
@test !is_valid(SVM{Float64}(NaN))
@test is_valid_array([SVM{Float64}(1), SVM{Float64}([1, 2, 3])])

# dummy operators
q(x::SVM{T}) where {T} = SVM{T}(x.scalar)
a(x::SVM{T}, y::SVM{T}) where {T} = SVM{T}(x.scalar + y.scalar)

operators = OperatorEnum(; binary_operators=[a], unary_operators=[q])
@extend_operators(operators, on_type = SVM{Float64})

Base.invokelatest(
    () -> begin

        # test operator extended operators
        @test hasmethod(q, Tuple{Node{SVM{Float64}}})
        @test hasmethod(a, Tuple{SVM{Float64},Node{SVM{Float64}}})
        @test hasmethod(a, Tuple{Node{SVM{Float64}},Node{SVM{Float64}}})
        @test !hasmethod(a, Tuple{Node{SVM{Float32}},Node{SVM{Float32}}})

        tree = a(Node{SVM{Float64}}(; feature=1), SVM{Float64}(3.0))
        results = tree([SVM{Float64}(1.0) SVM{Float64}(2.0) SVM{Float64}(3.0)])
        @test results == [SVM{Float64}(4), SVM{Float64}(5), SVM{Float64}(6)]
    end
)

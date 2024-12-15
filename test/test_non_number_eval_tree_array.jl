
using Base.Cartesian: @nif
using DynamicExpressions:
    DynamicExpressions as DE,
    ValueInterface,
    Node,
    @extend_operators,
    OperatorEnum,
    get_scalar_constants,
    set_scalar_constants!,
    pack_scalar_constants!,
    unpack_scalar_constants,
    count_scalar_constants,
    is_valid,
    is_valid_array

using Interfaces: Interfaces, @implements, Arguments

"""
    DynamicTensor{T,N,A<:Tuple{Base.RefValue{T},Vararg}}

A tensor with a maximum of `N` dimensions, where `N` is a positive integer.
`.data[1]` is a `Ref` to the scalar value (so that it is mutable), while
`.data[n]` is the array of dimension `n-1`.
"""
struct DynamicTensor{T,N,A<:Tuple{Base.RefValue{T},Vararg}}
    dims::UInt8
    data::A
    # ^For example, when N is 2, this is (Ref(0.0), Vector{Float64}[...], Matrix{Float64}[...])
    # See `Max2Tensor` below for an example

    #! format: off
    function DynamicTensor{T,N}(x::A=nothing) where {T,N,A<:Union{Nothing,Number,Array{<:Number}}}
        nd = x === nothing ? 0 : ndims(x)
        data = (
            Ref(x isa Number ? Base.convert(T, x) : zero(T)),
            ntuple(
                i -> if nd == i
                    Array{T,i}(x)
                else
                    Array{T,i}(undef, ntuple(_ -> 0, Val(i))...)
                end,
                Val(N)
            )...,
        )
        return new{T,N,typeof(data)}(
            x === nothing ? 0 : ndims(x),
            data
        )
    end
    #! format: on
end
const Max2Tensor{T} = DynamicTensor{T,2,Tuple{Base.RefValue{T},Vector{T},Matrix{T}}}
Max2Tensor{T}(x) where {T} = DynamicTensor{T,2}(x)

# Before implementing, we get a nice error message:
if VERSION >= v"1.9-"
    @test_throws "Base number type of type" DE.get_number_type(DynamicTensor{Float64,2})
end

DE.get_number_type(::Type{<:DynamicTensor{T}}) where {T} = T

@generated function DE.is_valid(val::DynamicTensor{<:Any,N}) where {N}
    quote
        @nif($(N + 1), i -> i == val.dims + 1, i -> if i == 1
            is_valid(val.data[i][])
        else
            is_valid_array(val.data[i])
        end)
    end
end
@generated function Base.:(==)(
    x::DynamicTensor{<:Any,N}, y::DynamicTensor{<:Any,N}
) where {N}
    quote
        x.dims != y.dims && return false
        @nif($(N + 1), i -> i == x.dims + 1, i -> if i == 1
            x.data[i][] == y.data[i][]
        else
            x.data[i] == y.data[i]
        end)
    end
end

@generated function DE.count_scalar_constants(val::DynamicTensor{<:Any,N}) where {N}
    quote
        @nif($(N + 1), i -> i == val.dims + 1, i -> i == 1 ? 1 : length(val.data[i]))
    end
end

@generated function DE.pack_scalar_constants!(
    nvals::AbstractVector{BT}, idx::Int64, val::DynamicTensor{BT,N}
) where {BT<:Number,N}
    quote
        @nif($(N + 1), i -> i == val.dims + 1, i -> if i == 1
            nvals[idx] = val.data[i][]
            idx + 1
        else
            data = val.data[i]
            num = length(data)
            copyto!(nvals, idx, @view(data[:]))
            idx + num
        end)
    end
end

@generated function DE.unpack_scalar_constants(
    nvals::AbstractVector{BT}, idx::Int64, val::DynamicTensor{BT,N}
) where {BT<:Number,N}
    quote
        @nif(
            $(N + 1),
            i -> i == val.dims + 1,
            i -> if i == 1
                val.data[i][] = nvals[idx]
                (idx + 1, val)
            else
                data = val.data[i]
                num = length(data)
                copyto!(data, @view(nvals[idx:(idx + num - 1)]))
                (idx + num, val)
            end
        )
    end
end

# Declare that `DynamicTensor` implements `ValueInterface`
@implements(ValueInterface, DynamicTensor, [Arguments()])
# Run the interface tests
@test Interfaces.test(
    ValueInterface,
    DynamicTensor,
    [
        # up to 1D
        DynamicTensor{Float64,1}(1.0),
        DynamicTensor{Float64,1}([1, 2, 3]),
        # up to 2D
        DynamicTensor{Float64,2}(1.0),
        DynamicTensor{Float64,2}([1, 2, 3]),
        DynamicTensor{Float64,2}([1 2 3; 4 5 6]),
        # up to 3D
        DynamicTensor{Float64,3}(1.0),
        DynamicTensor{Float64,3}([1, 2, 3]),
        DynamicTensor{Float64,3}([1 2 3; 4 5 6]),
        DynamicTensor{Float64,3}(rand(1, 2, 3)),
    ],
)

# testing is_valid functions
@test is_valid(DynamicTensor{Float64,2}())
@test !is_valid(DynamicTensor{Float64,2}(NaN))
@test is_valid_array([DynamicTensor{Float64,2}(1), DynamicTensor{Float64,2}([1, 2, 3])])
@test !is_valid_array([DynamicTensor{Float64,2}(1), DynamicTensor{Float64,2}([1, 2, NaN])])

# dummy operators
q(x::DynamicTensor{T,N}) where {T,N} = DynamicTensor{T,N}(x.data[1])
function a(x::DynamicTensor{T,N}, y::DynamicTensor{T,N}) where {T,N}
    return DynamicTensor{T,N}(x.data[1][] + y.data[1][])
end

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
            [Max2Tensor{Float64}(1.0) Max2Tensor{Float64}(2.0) Max2Tensor{Float64}(3.0)],
            operators,
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

        constants, refs = get_scalar_constants(tree, Float64)
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
        set_scalar_constants!(tree, constants .+ 5.0, refs)
        @test get_scalar_constants(tree, Float64)[1] == constants .+ 5.0
    end,
)

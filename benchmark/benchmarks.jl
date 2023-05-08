using DynamicExpressions, BenchmarkTools, Random
using DynamicExpressions.EquationUtilsModule: is_constant

include("benchmark_utils.jl")

const SUITE = BenchmarkGroup()

@generated function _eval_tree_array(args...; specialize_kernels, kwargs...)
    PACKAGE_VERSION < v"0.8.0" && return :(eval_tree_array(args...; kwargs...))
    return :(eval_tree_array(args...; specialize_kernels, kwargs...))
end

function benchmark_evaluation()
    suite = BenchmarkGroup()
    operators = OperatorEnum(;
        binary_operators=[+, -, /, *], unary_operators=[cos, exp], enable_autodiff=true
    )
    for T in (ComplexF32, ComplexF64, Float32, Float64), n in (50, 500, 5000)
        if !(T <: Real) && PACKAGE_VERSION < v"0.5.0" && PACKAGE_VERSION != v"0.0.0"
            continue
        end
        !haskey(suite, T) && (suite[T] = BenchmarkGroup())
        suite[T][n] = BenchmarkGroup()

        #! format: off
        for turbo in (false, true), specialize_kernels in (false, true)
            if turbo && !(T in (Float32, Float64))
                continue
            end
            extra_key = turbo ? "_turbo" : ""
            extra_key *= specialize_kernels ? "_specialized" : ""
            _eval_tree_array(
                gen_random_tree_fixed_size(20, operators, 5, T),
                randn(MersenneTwister(0), T, 5, n),
                operators;
                turbo=turbo,
                specialize_kernels=specialize_kernels
            )
            suite[T][n]["evaluation$(extra_key)"] = @benchmarkable(
                [_eval_tree_array(tree, X, $operators; turbo=$turbo, specialize_kernels=$specialize_kernels) for tree in trees],
                setup=(
                    X=randn(MersenneTwister(0), $T, 5, $n);
                    treesize=20;
                    ntrees=100;
                    trees=[gen_random_tree_fixed_size(treesize, $operators, 5, $T) for _ in 1:ntrees]
                )
            )
            if T <: Real && !specialize_kernels
                eval_grad_tree_array(
                    gen_random_tree_fixed_size(20, operators, 5, T),
                    randn(MersenneTwister(0), T, 5, n),
                    operators;
                    variable=true,
                    turbo=turbo
                )
                suite[T][n]["derivative$(extra_key)"] = @benchmarkable(
                    [eval_grad_tree_array(tree, X, $operators; variable=true, turbo=$turbo) for tree in trees],
                    setup=(
                        X=randn(MersenneTwister(0), $T, 5, $n);
                        treesize=20;
                        ntrees=100;
                        trees=[gen_random_tree_fixed_size(treesize, $operators, 5, $T) for _ in 1:ntrees]
                    )
                )
            end
        end
        #! format: on
    end
    return suite
end

# These macros make the benchmarks work on older versions:
#! format: off
@generated function _convert(::Type{N}, t; preserve_sharing) where {N<:Node}
    PACKAGE_VERSION < v"0.7.0" && return :(convert(N, t))
    return :(convert(N, t; preserve_sharing=preserve_sharing))
end
@generated function _copy_node(t; preserve_sharing)
    PACKAGE_VERSION < v"0.7.0" && return :(copy_node(t; preserve_topology=preserve_sharing))
    return :(copy_node(t; preserve_sharing=preserve_sharing))
end
@generated function get_set_constants!(tree)
    !(@isdefined set_constants!) && return :(set_constants(tree, get_constants(tree)))
    return :(set_constants!(tree, get_constants(tree)))
end
#! format: on

f_tree_op(f::F, tree, operators) where {F} = f(tree, operators)
f_tree_op(f::F, tree) where {F} = f(tree)

function benchmark_utilities()
    suite = BenchmarkGroup()

    all_funcs = (
        :copy,
        :convert,
        :simplify_tree,
        :combine_operators,
        :count_nodes,
        :count_depth,
        :count_constants,
        :has_constants,
        :has_operators,
        :is_constant,
        :get_set_constants!,
        :index_constants,
    )

    operators = OperatorEnum(; binary_operators=[+, -, /, *], unary_operators=[cos, exp])

    for func_k in all_funcs
        suite[func_k] = let s = BenchmarkGroup()
            for k in (:break_sharing, :preserve_sharing)
                k == :preserve_sharing && !(func_k in (:copy, :convert)) && continue

                f = if func_k == :copy
                    tree -> _copy_node(tree; preserve_sharing=(k == :preserve_sharing))
                elseif func_k == :convert
                    tree -> _convert(
                        Node{Float64},
                        tree;
                        preserve_sharing=(k == :preserve_sharing),
                    )
                elseif func_k in (:simplify_tree, :combine_operators)
                    g = getfield(@__MODULE__, func_k)
                    tree -> f_tree_op(g, tree, operators)
                else
                    g = getfield(@__MODULE__, func_k)
                    tree -> f_tree_op(g, tree)
                end

                #! format: off
                s[k] = @benchmarkable(
                    [$(f)(tree) for tree in trees],
                    seconds=10.0,
                    setup=(
                        ntrees=100;
                        n=20;
                        trees=[gen_random_tree_fixed_size(n, $operators, 5, Float32) for _ in 1:ntrees]
                    )
                )
                #! format: on
            end
            s
        end
    end

    return suite
end

SUITE["eval"] = benchmark_evaluation()
SUITE["utils"] = benchmark_utilities()

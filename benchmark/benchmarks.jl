using DynamicExpressions, BenchmarkTools, Random

# Trigger extensions:
using LoopVectorization, Bumper, StrideArrays, Zygote

if PACKAGE_VERSION < v"0.14.0"
    @eval using DynamicExpressions: Node as GraphNode
else
    @eval using DynamicExpressions: GraphNode
end

if PACKAGE_VERSION < v"0.17.0"
    @eval using DynamicExpressions.EquationUtilsModule: is_constant
else
    @eval using DynamicExpressions.NodeUtilsModule: is_constant
end

if PACKAGE_VERSION < v"0.18.6"
    @eval using DynamicExpressions:
        index_constants as index_constant_nodes,
        count_constants as count_constant_nodes,
        get_constants as get_scalar_constants,
        set_constants! as set_scalar_constants!
end

include("../test/tree_gen_utils.jl")

const SUITE = BenchmarkGroup()

function benchmark_evaluation()
    suite = BenchmarkGroup()
    operators = OperatorEnum(;
        binary_operators=[+, -, /, *],
        unary_operators=[cos, exp],
        (PACKAGE_VERSION >= v"0.15" ? () : (; enable_autodiff=true))...,
    )
    for T in (ComplexF32, ComplexF64, Float32, Float64)
        if !(T <: Real) && PACKAGE_VERSION < v"0.5.0" && PACKAGE_VERSION != v"0.0.0"
            continue
        end
        suite[T] = BenchmarkGroup()

        n = 1_000

        #! format: off
        for turbo in (false, true), bumper in (false, true)

            (turbo || bumper) && !(T in (Float32, Float64)) && continue
            if bumper
                try
                    eval_tree_array(Node{T}(val=1.0), ones(T, 5, n), operators; turbo, bumper)
                catch e
                    isa(e, MethodError) || rethrow(e)
                    @warn "Skipping bumper tests"
                    continue  # Assume its not available
                end
            end

            extra_key = if turbo && bumper
                "_turbo_bumper"
            elseif turbo
                "_turbo"
            elseif bumper
                "_bumper"
            else
                ""
            end
            extra_kws = bumper ? (; bumper=Val(true)) : ()
            eval_tree_array(
                gen_random_tree_fixed_size(20, operators, 5, T),
                randn(MersenneTwister(0), T, 5, n),
                operators;
                turbo,
                extra_kws...
            )
            suite[T]["evaluation$(extra_key)"] = @benchmarkable(
                [eval_tree_array(tree, X, $operators; kws...) for tree in trees],
                setup=(
                    X=randn(MersenneTwister(0), $T, 5, $n);
                    treesize=20;
                    ntrees=100;
                    kws=$(
                        if @isdefined(EvalOptions)
                            (; eval_options=EvalOptions(; turbo=turbo, extra_kws...))
                        else
                            (; turbo, extra_kws...)
                        end
                    );
                    trees=[gen_random_tree_fixed_size(treesize, $operators, 5, $T) for _ in 1:ntrees]
                )
            )
            if T <: Real && !bumper
                eval_grad_tree_array(
                    gen_random_tree_fixed_size(20, operators, 5, T),
                    randn(MersenneTwister(0), T, 5, n),
                    operators;
                    variable=true,
                    turbo=turbo
                )
                suite[T]["derivative$(extra_key)"] = @benchmarkable(
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
@generated function _convert(::Type{N}, t; preserve_sharing) where {N}
    PACKAGE_VERSION < v"0.7.0" && return :(convert(N, t))
    PACKAGE_VERSION < v"0.14.0" && return :(convert(N, t; preserve_sharing=preserve_sharing))
    return :(convert(N, t))  # Assume type used to infer sharing
end
@generated function _copy_node(t; preserve_sharing)
    PACKAGE_VERSION < v"0.7.0" && return :(copy_node(t; preserve_topology=preserve_sharing))
    PACKAGE_VERSION < v"0.14.0" && return :(copy_node(t; preserve_sharing=preserve_sharing))
    return :(copy_node(t))  # Assume type used to infer sharing
end
@generated function get_set_constants!(tree::N) where {N}
    T = eltype(N)
    if !(@isdefined set_scalar_constants!)
        return :(set_scalar_constants(tree, get_scalar_constants(tree)))
    elseif hasmethod(set_scalar_constants!, Tuple{N, Vector{T}})
        return :(set_scalar_constants!(tree, get_scalar_constants(tree)))
    else
        return quote
                let (x, refs) = get_scalar_constants(tree)
                set_scalar_constants!(tree, x, refs)
            end
        end
    end
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
        :count_constant_nodes,
        :has_constants,
        :has_operators,
        :is_constant,
        :get_set_constants!,
        :index_constant_nodes,
        :string_tree,
        :hash,
    )
    has_both_modes = [:copy, :convert]
    if PACKAGE_VERSION >= v"0.14.0"
        append!(
            has_both_modes,
            [
                :simplify_tree,
                :count_nodes,
                :count_constant_nodes,
                :get_set_constants!,
                :index_constant_nodes,
                :string_tree,
            ],
        )
    end
    if PACKAGE_VERSION >= v"0.14.1"
        append!(has_both_modes, [:hash])
    end

    operators = OperatorEnum(; binary_operators=[+, -, /, *], unary_operators=[cos, exp])
    for func_k in all_funcs
        suite[func_k] = let s = BenchmarkGroup()
            for k in (
                if func_k in has_both_modes
                    [:break_sharing, :preserve_sharing]
                else
                    [:break_sharing]
                end
            )
                preprocess = if k == :preserve_sharing && PACKAGE_VERSION >= v"0.14.0"
                    tree -> GraphNode(tree)
                else
                    identity
                end

                f = if func_k == :copy
                    tree -> _copy_node(tree; preserve_sharing=(k == :preserve_sharing))
                elseif func_k == :convert
                    tree -> _convert(
                        Node{Float64},
                        tree;
                        preserve_sharing=(k == :preserve_sharing),
                    )
                elseif func_k in (:simplify_tree, :combine_operators, :string_tree)
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
                        rng=Random.MersenneTwister(0);
                        trees=[$preprocess(gen_random_tree_fixed_size(n, $operators, 5, Float32, Node, rng)) for _ in 1:ntrees]
                    )
                )
                #! format: on
            end
            s
        end
    end

    # Additional methods
    @static if PACKAGE_VERSION >= v"0.18.0"
        suite["get_set_constants_parametric"] = @benchmarkable(
            [get_set_constants!(ex) for ex in exs],
            seconds = 10.0,
            setup = (
                operators = $operators;
                ntrees = 100;
                n = 20;
                n_features = 5;
                n_params = 3;
                n_param_classes = 10;
                rng = Random.MersenneTwister(0);
                exs = [
                    let tree = gen_random_tree_fixed_size(
                            n, operators, n_features, Float32, ParametricNode, rng
                        )
                        ex = ParametricExpression(
                            tree;
                            operators,
                            variable_names=map(i -> "x$i", 1:n_features),
                            parameters=randn(rng, Float32, n_params, n_param_classes),
                            parameter_names=map(i -> "p$i", 1:n_params),
                        )
                        ex
                    end for _ in 1:ntrees
                ]
            )
        )
    end

    return suite
end

SUITE["eval"] = benchmark_evaluation()
SUITE["utils"] = benchmark_utilities()

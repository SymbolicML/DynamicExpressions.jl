using DynamicExpressions, BenchmarkTools, Random
using DynamicExpressions: copy_node
using SymbolicRegression: gen_random_tree_fixed_size, Options

const v_PACKAGE_VERSION = try
    VersionNumber(DynamicExpressions.PACKAGE_VERSION)
catch
    VersionNumber("v0.0.0")
end

const SUITE = BenchmarkGroup()

function benchmark_evaluation()
    suite = BenchmarkGroup()
    operators = OperatorEnum(;
        binary_operators=[+, -, /, *], unary_operators=[cos, exp], enable_autodiff=true
    )
    simple_tree = Node(
        2,
        Node(
            1,
            Node(
                3,
                Node(1, Node(; val=1.0f0), Node(; feature=2)),
                Node(2, Node(; val=-1.0f0)),
            ),
            Node(1, Node(; feature=3), Node(; feature=4)),
        ),
        Node(
            4,
            Node(
                3,
                Node(1, Node(; val=1.0f0), Node(; feature=2)),
                Node(2, Node(; val=-1.0f0)),
            ),
            Node(1, Node(; feature=3), Node(; feature=4)),
        ),
    )
    for T in (ComplexF32, ComplexF64, Float32, Float64)
        if !(T <: Real) && v_PACKAGE_VERSION < v"0.5.0" && v_PACKAGE_VERSION != v"0.0.0"
            continue
        end
        suite[T] = BenchmarkGroup()

        evals = 10
        samples = 1_000
        n = 1_000

        #! format: off
        for turbo in (false, true)
            if turbo && !(T in (Float32, Float64))
                continue
            end
            extra_key = turbo ? "_turbo" : ""
            suite[T]["evaluation$(extra_key)"] = @benchmarkable(
                eval_tree_array(tree, X, $operators; turbo=$turbo),
                evals=evals,
                samples=samples,
                seconds=5.0,
                setup=(
                    X=randn(MersenneTwister(0), $T, 5, $n);
                    tree=convert(Node{$T}, copy_node($simple_tree))
                )
            )
            if T <: Real
                suite[T]["derivative$(extra_key)"] = @benchmarkable(
                    eval_grad_tree_array(tree, X, $operators; variable=true, turbo=$turbo),
                    evals=evals,
                    samples=samples,
                    seconds=5.0,
                    setup=(
                        X=randn(MersenneTwister(0), $T, 5, $n);
                        tree=convert(Node{$T}, copy_node($simple_tree))
                    )
                )
            end
        end
        #! format: on
    end
    return suite
end

function benchmark_utilities()
    suite = BenchmarkGroup()
    options = Options(; binary_operators=[+, -, /, *], unary_operators=[cos, exp])
    for func_k in ("copy", "convert", "simplify_tree", "combine_operators")
        suite[func_k] = let s = BenchmarkGroup()
            for k in ("break_sharing", "preserve_sharing")
                k == "preserve_sharing" &&
                    func_k in ("simplify_tree", "combine_operators") &&
                    continue

                f = if func_k == "copy"
                    tree -> copy_node(tree; preserve_sharing=(k == "preserve_sharing"))
                elseif func_k == "convert"
                    tree -> convert(
                        Node{Float64},
                        tree;
                        preserve_sharing=(k == "preserve_sharing"),
                    )
                elseif func_k == "simplify_tree"
                    tree -> simplify_tree(tree, options.operators)
                elseif func_k == "combine_operators"
                    tree -> combine_operators(tree, options.operators)
                end

                #! format: off
                s[k] = @benchmarkable(
                    $(f)(tree),
                    evals=300,
                    samples=300,
                    seconds=10.0,
                    setup=(
                        n=20;
                        tree=gen_random_tree_fixed_size(n, $options, 5, Float32)
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

using DynamicExpressions
using DynamicExpressions: EvalOptions, ArrayBuffer
using Random: MersenneTwister

include(joinpath(@__DIR__, "..", "test", "tree_gen_utils.jl"))

const AN = DynamicExpressions.ArenaNodeModule

operators = OperatorEnum(1 => (cos, exp), 2 => (+, -, *, /))
const T = Float64
nfeat = 5
n = 1_000
rng = MersenneTwister(0)
X = randn(rng, T, nfeat, n)
buf = zeros(T, 64, n)

function bench(trees, X, operators, buffer; reps=300)
    best = Inf
    for _ in 1:reps
        t0 = time_ns()
        for tree in trees
            buffer.index[] = 0
            eval_tree_array(tree, X, operators; eval_options=EvalOptions(; buffer))
        end
        best = min(best, (time_ns() - t0) / length(trees))
    end
    return best
end

function allocs_per_eval(tree, X, operators, buffer)
    @allocated(eval_tree_array(tree, X, operators; eval_options=EvalOptions(; buffer)))
end

for treesize in (7, 15, 31)
    trees = [gen_random_tree_fixed_size(treesize, operators, nfeat, T) for _ in 1:50]
    atrees = [convert(AN.ArenaNode{T,2}, t) for t in trees]
    buffer = ArrayBuffer(buf, Ref(0))

    # correctness sanity: both paths must agree
    for (t, a) in zip(trees, atrees)
        buffer.index[] = 0
        yn, okn = eval_tree_array(t, X, operators; eval_options=EvalOptions(; buffer))
        buffer.index[] = 0
        ya, oka = eval_tree_array(a, X, operators; eval_options=EvalOptions(; buffer))
        okn == oka || error("ok mismatch at size $treesize")
        okn && (yn ≈ ya || error("value mismatch at size $treesize"))
    end

    # warmup
    bench(trees, X, operators, buffer; reps=3)
    bench(atrees, X, operators, buffer; reps=3)

    t_node = bench(trees, X, operators, buffer)
    t_arena = bench(atrees, X, operators, buffer)
    buffer.index[] = 0
    a_node = allocs_per_eval(trees[1], X, operators, buffer)
    buffer.index[] = 0
    a_arena = allocs_per_eval(atrees[1], X, operators, buffer)
    println(
        "n=$treesize  Node: $(round(t_node/1e3; digits=2))us  " *
        "ArenaNode: $(round(t_arena/1e3; digits=2))us  " *
        "ratio=$(round(t_arena/t_node; digits=3))  " *
        "allocs/eval Node=$a_node Arena=$a_arena",
    )

    bench_nobuf(trees) = begin
        best = Inf
        for _ in 1:300
            t0 = time_ns()
            for tree in trees
                eval_tree_array(tree, X, operators)
            end
            best = min(best, (time_ns() - t0) / length(trees))
        end
        best
    end
    bench_nobuf(trees[1:2]);
    bench_nobuf(atrees[1:2])  # warmup
    tn_nb = bench_nobuf(trees)
    ta_nb = bench_nobuf(atrees)
    an_nb = @allocated(eval_tree_array(trees[1], X, operators))
    aa_nb = @allocated(eval_tree_array(atrees[1], X, operators))
    println(
        "n=$treesize  unbuffered:  Node: $(round(tn_nb/1e3; digits=2))us  " *
        "ArenaNode: $(round(ta_nb/1e3; digits=2))us  " *
        "ratio=$(round(ta_nb/tn_nb; digits=3))  " *
        "allocs/eval Node=$an_nb Arena=$aa_nb",
    )
end

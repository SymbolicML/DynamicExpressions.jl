using DynamicExpressions
using Random
using Test
using Zygote

operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin]);
x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)
tree = cos(x1 * 3.2 - 5.8) * 0.2 - 0.5 * x2 * x3 * x3 + 0.9 / (x1 * x1 + 1);

@testset "all" begin
    ctree = copy(tree)
    @test all(t -> t.degree != -1, ctree)
    @test all(
        t -> t.degree != 0 || !t.constant || t.val in (3.2, 5.8, 0.2, 0.5, 0.9, 1.0), ctree
    )
    @test !all(
        t -> t.degree != 0 || !t.constant || t.val in (3.2, 5.8, 0.2, 0.9, 1.0), ctree
    )
    @test all(t -> t.degree != 0 || t.constant || t.feature in (1, 2, 3), ctree)
    @test !all(t -> t.degree != 0 || t.constant || t.feature in (2, 3), ctree)
    @test all(t -> t.degree != 1 || t.op == 1, ctree)
    @test !all(t -> t.degree != 1 || t.op == 2, ctree)
end

@testset "any" begin
    ctree = copy(tree)
    @test any(t -> t.degree == 2, ctree)
    @test any(_ -> true, ctree)
    @test any(t -> t.degree == 0 && t.constant && t.val == 3.2, ctree)
    @test !any(t -> t.degree == 0 && t.constant && t.val == 3.3, ctree)
end

@testset "collect" begin
    ctree = copy(tree)
    @test typeof(first(collect(ctree))) == Node{Float64}
    @test objectid(first(collect(ctree))) == objectid(ctree)
    @test objectid(first(collect(ctree))) == objectid(ctree)
    @test objectid(first(collect(ctree))) == objectid(ctree)
    @test typeof(collect(ctree)) == Vector{Node{Float64}}
    @test length(collect(ctree)) == 24
    @test sum((t -> (t.degree == 0 && t.constant) ? t.val : 0.0).(collect(ctree))) ≈ 11.6
end

@testset "count" begin
    ctree = copy(tree)
    @test count(_ -> true, ctree) == 24
    @test count(t -> t.degree == 0, ctree) == 12
    @test count(t -> t.degree == 1, ctree) == 1
    @test count(t -> t.degree == 2, ctree) == 11
    @test count(t -> t.degree == 0 && t.constant, ctree) == 6
    @test count(t -> t.degree == 0 && t.constant && t.val == 1, ctree) == 1
end

@testset "filter" begin
    ctree = copy(tree)
    @test filter(_ -> true, ctree) == collect(ctree)
    @test length(filter(t -> t.degree == 0 && !t.constant, ctree)) == 6
    @test unique(filter(t -> t.degree == 0 && !t.constant, ctree)) == [x1, x2, x3]
    @test length(filter(t -> t.degree == 1, ctree)) == 1
    @test length(filter(t -> t.degree == 2, ctree)) == 11
    @test filter(==(x1), ctree) == [x1, x1, x1]
end

@testset "foreach" begin
    ctree = copy(tree)
    counter = Ref(0)
    foreach(ctree) do t
        counter.x += 1
    end
    @test counter.x == 24
    foreach(ctree) do t
        if t.degree == 0 && t.constant
            t.val *= 2
        end
    end
    @test sum(t -> t.val, filter(t -> t.degree == 0 && t.constant, ctree)) ≈ 11.6 * 2
end

@testset "iterate" begin
    ctree = copy(tree)
    counter = Ref(0)
    for t in ctree
        counter.x += 1
    end
    @test counter.x == 24
    for t in ctree
        if t.degree == 0 && t.constant
            t.val *= 2
        end
    end
    @test sum(t -> t.val, filter(t -> t.degree == 0 && t.constant, ctree)) ≈ 11.6 * 2

    # iterate within iterate:
    counter = Ref(0)
    for t in ctree
        for t2 in t
            counter.x += 1
        end
    end
    @test counter.x == 104
end

@testset "map" begin
    ctree = copy(tree)
    vals = map(t -> t.val, ctree)
    vals = [v for v in vals if v !== nothing]
    @test sum(vals) ≈ 11.6
    @test sum(map(_ -> 1, ctree)) == 24
    @test sum(map(_ -> 2, ctree)) == 24 * 2
    @test sum(map(t -> t.degree == 1, ctree)) == 1
    @test length(unique(map(objectid, copy_node(tree)))) == 24
    map(t -> (t.degree == 0 && t.constant) ? (t.val *= 2) : nothing, ctree)
    @test sum(t -> t.val, filter(t -> t.degree == 0 && t.constant, ctree)) ≈ 11.6 * 2
    local T = fieldtype(typeof(ctree), :degree)
    @test typeof(map(t -> t.degree, ctree, T)) == Vector{T}
    @test first(map(t -> t.degree, ctree, T)) == 2
end

@testset "in" begin
    ctree = copy(tree)
    @test x1 in ctree
    @test Node(Float64; val=1.0) ∈ ctree
    @test Node(Float32; val=1.0) ∈ ctree
    @test Node(Float64; val=1.1) ∉ ctree
    @test ctree.l ∈ ctree
    @test ctree.l * 2 ∉ ctree
end

@testset "isempty" begin
    @test !isempty(tree)
    @test !isempty(Node(Float32; val=1))
end

@testset "length" begin
    @test length(tree) == 24
    @test length(tree.l) == 16
    @test length(tree.r) == 24 - 16 - 1
end

@testset "mapreduce" begin
    @test mapreduce(_ -> 1, +, tree) == 24
    @test mapreduce(_ -> 2, +, tree) == 48
    @test mapreduce(_ -> 1, *, tree) == 1
    @test mapreduce(_ -> 2, *, tree) == 2^24
    @test mapreduce(t -> t.degree, *, tree) == 0
    @test mapreduce(t -> t.degree + 1, *, tree) == 354294
    @test mapreduce(t -> Int(t.degree), (l, r) -> (max(l, 1) * max(r, 1)), tree) == 2048
    @test mapreduce(+, tree) do t
        1
    end == 24
end

@testset "sum" begin
    ctree = copy(tree)
    @test sum(t -> t.degree == 0 && t.constant ? t.val : 0.0, ctree) == 11.6
    @test sum(t -> t.degree == 0 && !t.constant ? Int(t.feature) : 0, ctree) ==
        3 * 1 + 1 * 2 + 2 * 3
    @test sum(t -> t.degree == 1 ? Int(t.op) : 0, ctree) == 1
    @test sum(t -> (t.degree == 0 && t.constant) ? t.val * 2 : 0.0, ctree) == 11.6 * 2
    for t in ctree
        if t.degree == 0 && t.constant
            t.val *= 1.5
        end
    end
    @test sum(t -> (t.degree == 0 && t.constant) ? t.val : 0.0, ctree) ≈ 11.6 * 1.5
end

@testset "Unsupported" begin
    if VERSION >= v"1.7.0"
        for func in (:reduce, :foldl, :foldr, :mapfoldl, :mapfoldr)
            wrapped_func(args...) = (@eval $func)(args...)
            @test_throws ErrorException wrapped_func(Returns(1), tree)
        end
    end
end

import Combinatorics: permutations
import SnoopPrecompile: @precompile_all_calls, @precompile_setup

macro ignore_domain_error(ex)
    return esc(
        quote
            try
                $ex
            catch e
                if e isa DomainError
                    return nothing
                else
                    rethrow(e)
                end
            end
        end,
    )
end

"""
    test_all_combinations(; binary_operators, unary_operators, turbo, types)

Test all combinations of the given operators and types. Useful for precompilation.
"""
function test_all_combinations(; binary_operators, unary_operators, turbo, types)
    for binops in permutations(binary_operators),
        unaops in permutations(unary_operators),
        use_turbo in turbo,
        T in types

        length(binops) == 0 && length(unaops) == 0 && continue
        T == Float16 && use_turbo && continue

        X = rand(T, 3, 10)
        operators = OperatorEnum(;
            binary_operators=binops, unary_operators=unaops, define_helper_functions=false
        )
        x = Node(T; feature=1)
        c = Node(T; val=one(T))

        # Trivial:
        for l in (x, c)
            @ignore_domain_error eval_tree_array(l, X, operators; turbo=use_turbo)
        end

        # Binary operators
        for i in eachindex(binops), l in (x, c), r in (x, c)
            tree = Node(i, l, r)
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(tree, X, operators; turbo=use_turbo)
        end

        # Unary operators
        for j in eachindex(unaops), k in eachindex(unaops), l in (x, c)
            tree = Node(j, l)
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(tree, X, operators; turbo=use_turbo)

            tree = Node(j, Node(k, l))
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(tree, X, operators; turbo=use_turbo)
        end

        # Both operators
        for i in eachindex(binary_operators),
            j1 in eachindex(unary_operators),
            j2 in eachindex(unary_operators),
            l in (x, c),
            r in (x, c)

            tree = Node(i, Node(j1, l), Node(j2, r))
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(tree, X, operators; turbo=use_turbo)

            tree = Node(j1, Node(i, l, r))
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(tree, X, operators; turbo=use_turbo)
        end
    end
end

@precompile_setup begin
    binary_operators = [+, -, *, /, ^]
    unary_operators = [sin, cos, exp, log, sqrt, abs]
    turbo = [true, false]
    types = [Float16, Float32, Float64]
    @precompile_all_calls begin
        test_all_combinations(;
            binary_operators=binary_operators,
            unary_operators=unary_operators,
            turbo=turbo,
            types=types,
        )
    end
end

import SnoopPrecompile: @precompile_all_calls, @precompile_setup

macro ignore_domain_error(ex)
    return esc(
        quote
            try
                $ex
            catch e
                if !(e isa DomainError)
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
    for binops in binary_operators,
        unaops in unary_operators,
        use_turbo in turbo,
        T in types

        length(binops) == 0 && length(unaops) == 0 && continue
        T == Float16 && use_turbo && continue

        X = rand(T, 3, 10)
        operators = OperatorEnum(;
            binary_operators=binops,
            unary_operators=unaops,
            define_helper_functions=false,
            enable_autodiff=true,
        )
        x = Node(T; feature=1)
        c = Node(T; val=one(T))

        # Trivial:
        for l in (x, c)
            @ignore_domain_error eval_tree_array(l, X, operators; turbo=use_turbo)
            for variable in (true, false)
                @ignore_domain_error eval_grad_tree_array(
                    l, X, operators; turbo=use_turbo, variable
                )
            end
        end

        # Binary operators
        for i in eachindex(binops), l in (x, c), r in (x, c)
            tree = Node(i, l, r)
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(tree, X, operators; turbo=use_turbo)
            for variable in (true, false)
                @ignore_domain_error eval_grad_tree_array(
                    l, X, operators; turbo=use_turbo, variable
                )
            end
        end

        # Unary operators
        for j in eachindex(unaops), k in eachindex(unaops), l in (x, c)
            tree = Node(j, l)
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(tree, X, operators; turbo=use_turbo)
            for variable in (true, false)
                @ignore_domain_error eval_grad_tree_array(
                    l, X, operators; turbo=use_turbo, variable
                )
            end

            tree = Node(j, Node(k, l))
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(tree, X, operators; turbo=use_turbo)
            for variable in (true, false)
                @ignore_domain_error eval_grad_tree_array(
                    l, X, operators; turbo=use_turbo, variable
                )
            end
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
            for variable in (true, false)
                @ignore_domain_error eval_grad_tree_array(
                    l, X, operators; turbo=use_turbo, variable
                )
            end

            tree = Node(j1, Node(i, l, r))
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(tree, X, operators; turbo=use_turbo)
            for variable in (true, false)
                @ignore_domain_error eval_grad_tree_array(
                    l, X, operators; turbo=use_turbo, variable
                )
            end
        end
    end
    return nothing
end

function test_functions_on_trees(::Type{T}, operators) where {T}
    local x, c, tree
    for T1 in [Float16, Float32, Float64]
        x = Node(T1; feature=1)
        c = Node(T1; val=T1(1.0))
        tree = Node(
            2,
            Node(1, Node(1, Node(2, x, c), Node(3, c, Node(1, x)))),
            Node(3, Node(1, Node(4, x, x))),
        )
    end
    tree = convert(Node{T}, tree)
    for preserve_topology in [true, false]
        tree = copy_node(tree; preserve_topology)
        set_node!(tree, copy_node(tree; preserve_topology))
    end

    string_tree(tree, operators)
    count_nodes(tree)
    count_constants(tree)
    count_depth(tree)
    index_constants(tree)
    has_operators(tree)
    has_constants(tree)
    get_constants(tree)
    set_constants(tree, get_constants(tree))
    combine_operators(tree, operators)
    simplify_tree(tree, operators)
    return nothing
end

"""Run force_run=true; @precompile_setup otherwise."""
macro maybe_precompile_setup(force_run, ex)
    precompile_ex = Expr(
        :macrocall, Symbol("@precompile_setup"), LineNumberNode(@__LINE__), ex
    )
    return quote
        if $(esc(force_run))
            $(esc(ex))
        else
            $(esc(precompile_ex))
        end
    end
end

"""Run force_run=true; @precompile_all_calls otherwise."""
macro maybe_precompile_all_calls(force_run, ex)
    precompile_ex = Expr(
        :macrocall, Symbol("@precompile_all_calls"), LineNumberNode(@__LINE__), ex
    )
    return quote
        if $(esc(force_run))
            $(esc(ex))
        else
            $(esc(precompile_ex))
        end
    end
end

function do_precompilation(; force_run=false)
    @maybe_precompile_setup force_run begin
        binary_operators = [[+, -, *, /, ^]]
        unary_operators = [[sin, cos, exp, log, sqrt, abs]]
        turbo = [true, false]
        types = [Float16, Float32, Float64]
        @maybe_precompile_all_calls force_run begin
            test_all_combinations(;
                binary_operators=binary_operators,
                unary_operators=unary_operators,
                turbo=turbo,
                types=types,
            )
        end
        operators = OperatorEnum(;
            binary_operators=binary_operators[1],
            unary_operators=unary_operators[1],
            define_helper_functions=false,
        )
        # Want to precompile all above calls.
        types = [Float16, Float32, Float64]
        for T in types
            @maybe_precompile_all_calls force_run begin
                test_functions_on_trees(T, operators)
            end
        end
    end
end

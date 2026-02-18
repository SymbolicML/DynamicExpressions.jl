import PrecompileTools: @compile_workload, @setup_workload

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
        T == Float16 && use_turbo isa Val{true} && continue

        X = rand(T, 3, 10)
        operators = OperatorEnum(;
            binary_operators=binops, unary_operators=unaops, define_helper_functions=false
        )
        x = Node(T; feature=1)
        c = Node(T; val=one(T))

        # Trivial:
        for l in (x, c)
            @ignore_domain_error eval_tree_array(
                l, X, operators; eval_options=EvalOptions(; turbo=use_turbo)
            )
        end

        # Binary operators
        for i in eachindex(binops), l in (x, c), r in (x, c)
            tree = Node(i, l, r)
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(
                tree, X, operators; eval_options=EvalOptions(; turbo=use_turbo)
            )
        end

        # Unary operators
        for j in eachindex(unaops), k in eachindex(unaops), l in (x, c)
            tree = Node(j, l)
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(
                tree, X, operators; eval_options=EvalOptions(; turbo=use_turbo)
            )

            tree = Node(j, Node(k, l))
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(
                tree, X, operators; eval_options=EvalOptions(; turbo=use_turbo)
            )
        end

        # Both operators
        for i in eachindex(binary_operators),
            j1 in eachindex(unary_operators),
            j2 in eachindex(unary_operators),
            l in (x, c),
            r in (x, c)

            tree = Node(i, Node(j1, l), Node(j2, r))
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(
                tree, X, operators; eval_options=EvalOptions(; turbo=use_turbo)
            )

            tree = Node(j1, Node(i, l, r))
            tree = convert(Node{T}, tree)
            @ignore_domain_error eval_tree_array(
                tree, X, operators; eval_options=EvalOptions(; turbo=use_turbo)
            )
        end
    end
    return nothing
end

function test_functions_on_trees(::Type{T}, operators) where {T}
    local x, c, tree
    num_unaops = length(operators.unaops)
    num_binops = length(operators.binops)
    @assert num_unaops > 0 && num_binops > 0

    for T1 in [Float32, Float64]
        x = Node(T1; feature=1)
        c = Node(T1; val=T1(1.0))

        i_una = 1
        i_bin = 1

        # Here, we just cycle through the operators and build
        # a more complex expression.
        a1 = Node(i_una, x)
        i_una = (i_una % num_unaops) + 1
        a2 = Node(i_bin, c, a1)
        i_bin = (i_bin % num_binops) + 1
        a3 = Node(i_bin, x, c)
        i_bin = (i_bin % num_binops) + 1
        a4 = Node(i_bin, a3, a2)
        i_bin = (i_bin % num_binops) + 1
        a5 = Node(i_bin, x, x)
        i_bin = (i_bin % num_binops) + 1
        a6 = Node(i_una, a5)
        i_una = (i_una % num_unaops) + 1
        a7 = Node(i_una, a6)
        i_una = (i_una % num_unaops) + 1
        a8 = Node(i_una, a4)
        i_una = (i_una % num_unaops) + 1
        tree = Node(i_bin, a8, a7)
    end
    tree = convert(Node{T}, tree)
    tree = copy_node(tree)
    set_node!(tree, tree)

    string_tree(tree, operators)
    count_nodes(tree)
    count_constant_nodes(tree)
    count_depth(tree)
    index_constant_nodes(tree)
    has_operators(tree)
    has_constants(tree)
    set_scalar_constants!(tree, get_scalar_constants(tree)...)
    combine_operators(tree, operators)
    simplify_tree!(tree, operators)
    return nothing
end

macro maybe_setup_workload(mode, ex)
    precompile_ex = Expr(
        :macrocall, Symbol("@setup_workload"), LineNumberNode(@__LINE__), ex
    )
    return quote
        if $(esc(mode)) == :compile
            $(esc(ex))
        elseif $(esc(mode)) == :precompile
            $(esc(precompile_ex))
        else
            error("Invalid value for mode: " * show($(esc(mode))))
        end
    end
end

macro maybe_compile_workload(mode, ex)
    precompile_ex = Expr(
        :macrocall, Symbol("@compile_workload"), LineNumberNode(@__LINE__), ex
    )
    return quote
        if $(esc(mode)) == :compile
            $(esc(ex))
        elseif $(esc(mode)) == :precompile
            $(esc(precompile_ex))
        else
            error("Invalid value for mode: " * show($(esc(mode))))
        end
    end
end

"""`mode=:precompile` will use `@precompile_*` directives; `mode=:compile` runs."""
function do_precompilation(; mode=:precompile)
    @maybe_setup_workload mode begin
        binary_operators = ((+, -, *, /),)
        unary_operators = ((sin, cos),)
        turbo = (Val(false),)
        types = (Float32, Float64)
        @maybe_compile_workload mode begin
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
        types = [Float32, Float64]
        for T in types
            @maybe_compile_workload mode begin
                test_functions_on_trees(T, operators)
            end
        end
    end
end

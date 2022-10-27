using DynamicExpressions
import DynamicExpressions.EvaluateEquationModule: deg0_eval

const TapeType{T} = IdDict{Node{T},Vector{T}}

function reverse_grad(tree::Node{T}, X::AbstractMatrix{T}, operators::OperatorEnum; variable=true) where {T}
    tape = TapeType{T}()
    load_tape!(tape, tree, X, operators)
    init_grad = ones(T, 1, size(X, 2))
    return reverse_grad_from_tape(init_grad, tape, tree, operators; variable=variable, ngrads=size(X, 1))
end

function reverse_grad_from_tape(grad::AbstractMatrix{T}, tape::TapeType{T}, tree::Node{T}, operators::OperatorEnum; variable::Bool, ngrads::Int)::AbstractMatrix{T} where {T}
    if tree.degree == 0
        if variable
            tree.constant && return grad .* 0
            my_grad = zeros(T, ngrads, size(grad))
            my_grad[tree.feature] .= one(T)
        else
        end
    elseif tree.degree == 1
    else
    end
end

function load_tape!(
    tape::TapeType{T}, tree::Node{T}, X::AbstractMatrix{T}, operators::OperatorEnum
)::Vector{T} where {T}
    get!(tape, tree) do
        if tree.degree == 0
            return deg0_eval(tree, X, operators)[1]
        elseif tree.degree == 1
            return deg1_eval_tape!(tape, tree, X, Val(tree.op), operators)
        else
            return deg2_eval_tape!(tape, tree, X, Val(tree.op), operators)
        end
    end
end

function deg1_eval_tape!(
    tape::TapeType{T},
    tree::Node{T},
    cX::AbstractMatrix{T},
    ::Val{op_idx},
    operators::OperatorEnum,
) where {T,op_idx}
    left = load_tape!(tape, tree.l, cX, operators)
    op = operators.unaops[op_idx]
    return op.(left)
end

function deg2_eval_tape!(
    tape::TapeType{T},
    tree::Node{T},
    cX::AbstractMatrix{T},
    ::Val{op_idx},
    operators::OperatorEnum,
) where {T,op_idx}
    left = load_tape!(tape, tree.l, cX, operators)
    right = load_tape!(tape, tree.r, cX, operators)
    op = operators.binops[op_idx]
    return op.(left, right)
end

operators = OperatorEnum(;
    binary_operators=[+, -, *, /], unary_operators=[cos, exp, sin], enable_autodiff=true
)
x1, x2, x3 = Node(; feature=1), Node(; feature=2), Node(; feature=3)
tree = cos(x1 - 3.2) * x2
X = randn(3, 100)
@btime tape = rev(tree, X, operators);

module OperatorEnumModule

struct OperatorEnum{A<:Tuple,B<:Tuple,dA<:Union{Tuple,Nothing},dB<:Union{Tuple,Nothing}}
    binops::A
    unaops::B
    diff_binops::dA
    diff_unaops::dB
end

end

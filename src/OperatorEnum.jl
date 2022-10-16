module OperatorEnumModule

struct OperatorEnum{A,B,dA,dB}
    binops::A
    unaops::B
    diff_binops::dA
    diff_unaops::dB
end

end
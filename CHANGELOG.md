# Changelog

## [1.11.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.10.4...v1.11.0) (2026-03-21)

## [1.10.4](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.10.3...v1.10.4) (2026-03-08)

## [2.5.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v2.5.0...v2.5.1) (2026-02-12)

## [2.5.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v2.4.1...v2.5.0) (2026-02-04)


### Features

* add SymbolicUtils v4 compatibility ([#154](https://github.com/SymbolicML/DynamicExpressions.jl/issues/154)) ([9b77e3c](https://github.com/SymbolicML/DynamicExpressions.jl/commit/9b77e3cc4cc6982325c55f5a00cf80d4a1d6e6cf))

## [2.4.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v2.4.0...v2.4.1) (2026-01-31)

## [2.4.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v2.3.0...v2.4.0) (2025-08-31)


### Features

* get `parse_expressions` to work with aliases ([9e129d2](https://github.com/SymbolicML/DynamicExpressions.jl/commit/9e129d20b578b0e63945d0b0d1e6eacc32e4642b))
* get `parse_expressions` to work with aliases ([d4b12df](https://github.com/SymbolicML/DynamicExpressions.jl/commit/d4b12dfd4d6b1ceeda922eae756a33f22549c58b))

## [2.3.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v2.2.1...v2.3.0) (2025-07-26)


### Features

* allow passing String to `parse_expression` ([7c62924](https://github.com/SymbolicML/DynamicExpressions.jl/commit/7c629248027513e0a7cd6a99e9dcecf7936bc0e4))
* attempt expression parsing without interpolation ([f1c36c2](https://github.com/SymbolicML/DynamicExpressions.jl/commit/f1c36c2e1d82e6b07d5115bb46d826e677b05339))

## [2.2.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v2.2.0...v2.2.1) (2025-06-27)


### Bug Fixes

* precompilation warning ([eab884e](https://github.com/SymbolicML/DynamicExpressions.jl/commit/eab884e88357ce2204a9ce7dc1d6be9d4e1b96dd))
* precompilation warning ([e46ee58](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e46ee58e7fe9f3335360153c08891372e21f029b))

## [2.2.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v2.1.0...v2.2.0) (2025-06-26)


### Features

* create `EvalOptions(use_fused=false)` to avoid fused operations ([ef8a51a](https://github.com/SymbolicML/DynamicExpressions.jl/commit/ef8a51addfb86e5f184610cc8db8f5789990ed79))
* make `ResultOk` available at top level ([e92645d](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e92645d72ffae536e6fbecbb24b7eec348c41ac7))
* option to skip fused kernels ([b114eb0](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b114eb08612d3e453f709b300933fae726a8a8de))

## [2.1.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v2.0.1...v2.1.0) (2025-06-24)


### Bug Fixes

* `default_node_type` should not prescribe degree ([60bbf7b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/60bbf7b9a1b029954e6caa0bcbadfd587914a708))
* `default_node_type` should not prescribe degree ([26c6c6f](https://github.com/SymbolicML/DynamicExpressions.jl/commit/26c6c6fdd191fe0cb8f31d762565822d71d20b60))

## [2.0.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v2.0.0...v2.0.1) (2025-06-20)


### Bug Fixes

* recursive type inference issue ([002be14](https://github.com/SymbolicML/DynamicExpressions.jl/commit/002be14a61f42edebb13b504c635f2f5d76194bd))
* recursive type inference issue ([7badcc1](https://github.com/SymbolicML/DynamicExpressions.jl/commit/7badcc1e21649c8823a8fc14001cf537d8a4fc37))

## [2.0.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.10.3...v2.0.0) (2025-06-15)


### Features

* add generic degree pathway in evaluation ([cda0896](https://github.com/SymbolicML/DynamicExpressions.jl/commit/cda089653297352554ff463b1a911438274b157d))
* add `setproperty!`5 to required interface ([befee81](https://github.com/SymbolicML/DynamicExpressions.jl/commit/befee815e9027a01805736e98c21575b466085d1))
* add `children` to required interface ([b78097a](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b78097a3df6330a6819cf2e61877b18469fb3bc8))
* allow vector of functions to OperatorEnum ([01ef42a](https://github.com/SymbolicML/DynamicExpressions.jl/commit/01ef42ad35e2e0dee507f1e2666e6ce4d0c846c3))
* better interface for children ([008dfbc](https://github.com/SymbolicML/DynamicExpressions.jl/commit/008dfbc818302bf5a1e5da558732ceee5fad612e))
* complete node interface for n-arity ([59c0878](https://github.com/SymbolicML/DynamicExpressions.jl/commit/59c0878e079e37fde341f3d5cdeb3037c175af79))
* create n-arity operator enum ([87102d8](https://github.com/SymbolicML/DynamicExpressions.jl/commit/87102d8201d8c7bd94fef0c80f8f8c668c6ca2b6))
* fix children set in ParametricNode ([0af1700](https://github.com/SymbolicML/DynamicExpressions.jl/commit/0af17009d9bbbb1be67ddff08c2c5e67a5335c87))
* get expression algebra working ([633cc94](https://github.com/SymbolicML/DynamicExpressions.jl/commit/633cc943d562c895a803bed60a96f68b1fd83446))
* get expression operators working with 3-arg input ([9ccb46a](https://github.com/SymbolicML/DynamicExpressions.jl/commit/9ccb46a00bc1b441929e5c29a886e264a5468e86))
* guard undefined children behind Nullable ([cdc980d](https://github.com/SymbolicML/DynamicExpressions.jl/commit/cdc980d3c9aa3f0a09442924c85a7b63ca742487))
* `setproperty!`6 to work with vector ([2bf20fa](https://github.com/SymbolicML/DynamicExpressions.jl/commit/2bf20fa3a6d41209f71fc5a9b39e969d1e0ad48d))
* `any` and `==` working with n-arity nodes ([1d3d834](https://github.com/SymbolicML/DynamicExpressions.jl/commit/1d3d834a441ba9feec94c4f362a131eb177f455e))
* make diff compatibility with n-arity ([83902e3](https://github.com/SymbolicML/DynamicExpressions.jl/commit/83902e3d847ae01db66f9a9a28a32a1bdcf34e2e))
* make differentiable eval work for n-arity ([97abbd0](https://github.com/SymbolicML/DynamicExpressions.jl/commit/97abbd0199524f85383adec29247b7cc2e3d79d9))
* make generic eval allow n-arity nodes ([067734a](https://github.com/SymbolicML/DynamicExpressions.jl/commit/067734ac2b387b2def187e4a7e8728a756d95804))
* make grad compatible with n-arity ([5f977ab](https://github.com/SymbolicML/DynamicExpressions.jl/commit/5f977abf649e44a8844161332ae22669ff999096))
* n-arity compat with bumper ([4f31a85](https://github.com/SymbolicML/DynamicExpressions.jl/commit/4f31a85519ce9287c5bd9509b23f29eb74a8f67b))
* n-arity compat with simplification ([7b51c06](https://github.com/SymbolicML/DynamicExpressions.jl/commit/7b51c060f5b3b6e87c46287a64d1ac9fb079f183))
* n-arity LoopVectorization compat ([092b945](https://github.com/SymbolicML/DynamicExpressions.jl/commit/092b9454bf5d5c4d25365e265482fe2927a0e35c))
* n-arity strings ([673146c](https://github.com/SymbolicML/DynamicExpressions.jl/commit/673146c9944ac9b2073e85d0825c31bba2911f3d))
* new operator enum construction and `setproperty!`8 ([6fbc5ea](https://github.com/SymbolicML/DynamicExpressions.jl/commit/6fbc5eabc5e7d8c26acd1c0f68dd1c2be985420d))
* parsing for D-degree nodes ([422b5c5](https://github.com/SymbolicML/DynamicExpressions.jl/commit/422b5c51035efb7d182191db60f2495f9987d4fd))


### Bug Fixes

* a few merge issues ([5505506](https://github.com/SymbolicML/DynamicExpressions.jl/commit/5505506215896dc0479c73003f9cffcdf744a377))
* attempt type stability fix for union of `setproperty!`1 with other type ([6e78b27](https://github.com/SymbolicML/DynamicExpressions.jl/commit/6e78b279a68617b257e0b247f4f2cdc01ec074da))
* bad edit on get_child ([67248f7](https://github.com/SymbolicML/DynamicExpressions.jl/commit/67248f768dec19235f486de9f1dbe6ff8b54a622))
* constructor from explicit eltype ([0079acf](https://github.com/SymbolicML/DynamicExpressions.jl/commit/0079acfcbc36cdee5eecc1d02313e9d0185b3272))
* correctly mark unstable ([3da2fae](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3da2fae5ed855d93e126e568912dda147f63ab0b))
* edge case with chainable operators ([eef67f0](https://github.com/SymbolicML/DynamicExpressions.jl/commit/eef67f0081877d0c1ff71b62dbf0038f49c41860))
* fix degree in `NodeIndex` ([90fe177](https://github.com/SymbolicML/DynamicExpressions.jl/commit/90fe1776846f5da62ff19aee813168413ff8148e))
* fix JET identified missing method ([7a69a7b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/7a69a7bab0674f856baed353c008d26c3044d17d))
* `setproperty!` for tuple children ([0514598](https://github.com/SymbolicML/DynamicExpressions.jl/commit/051459859a84ca9706aaafb7df3b14487bc2b1d3))
* `setproperty!`2 should check degree ([1d61192](https://github.com/SymbolicML/DynamicExpressions.jl/commit/1d6119246428f1f53fca28843b0f28a764ac4813))
* `children`0 from `children`1 ([59c41fd](https://github.com/SymbolicML/DynamicExpressions.jl/commit/59c41fdcfb05919637b5086c3c78a0d5096608cf))
* `set_node!` should set to `children` ([b4a5ba8](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b4a5ba8c9af9c60a820c3f0a3a46162985bd524b))
* missing import ([0668a58](https://github.com/SymbolicML/DynamicExpressions.jl/commit/0668a582c180f60628330de810c6a54bf9281cb6))
* mutation error for Zygote ([f948ddb](https://github.com/SymbolicML/DynamicExpressions.jl/commit/f948ddb1d0cae2410ae6925ea661d2652bd1335b))
* node conversion changing degree ([38101d1](https://github.com/SymbolicML/DynamicExpressions.jl/commit/38101d1dee4d8d47ac3806f2e4a8dae73156da50))
* node preallocation for n-arity nodes ([3905fc8](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3905fc8a197e3d8eac92077395a10f81b511f0e8))
* operator conversion ([6c9d345](https://github.com/SymbolicML/DynamicExpressions.jl/commit/6c9d34578817f561f99851c7057b1402cb461176))
* other fixes for d-degree nodes ([33e5966](https://github.com/SymbolicML/DynamicExpressions.jl/commit/33e59660fa6f33b534f35541d583afa428870c58))
* permit vector children to constructor ([97c8868](https://github.com/SymbolicML/DynamicExpressions.jl/commit/97c88687205c3abf02ede86cd2c614856d0e5596))
* read only nodes when given ref ([b6d187b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b6d187b3bd7a838d064d2e5e5ebc0b61412c2557))
* segfault in NodeIndex ([b5285f7](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b5285f7c355129deed855d12632749d32f00915f))
* some issues with D-degree ParametricNode ([1e672bc](https://github.com/SymbolicML/DynamicExpressions.jl/commit/1e672bc3a930c6c79726b9f0852e0044934e9f95))
* type assertion issue in constructor ([24aec43](https://github.com/SymbolicML/DynamicExpressions.jl/commit/24aec43d67dd094876551cbfdde5c4b11c87cad9))
* type instabilities ([22154d0](https://github.com/SymbolicML/DynamicExpressions.jl/commit/22154d0575ffed901e9b4211b67a97cbc99a7bf8))
* various aspects of degree interface ([3ed6b41](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3ed6b41067c48f8e374f0c198ce3d6203fab8185))
* various aspects of degree interface ([2a0bd05](https://github.com/SymbolicML/DynamicExpressions.jl/commit/2a0bd054578c88f47b70b61ad0141e40c8e6ce47))
* various issues with n-arity parametric node ([d5a69b7](https://github.com/SymbolicML/DynamicExpressions.jl/commit/d5a69b74d512e3e673ca1edd344b3d207ae1cd3a))

## [1.10.3](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.10.2...v1.10.3) (2025-06-14)


### Bug Fixes

* move non_differentiable to special module for JET masking ([0e66242](https://github.com/SymbolicML/DynamicExpressions.jl/commit/0e662423688babda273c13b88f207040481a35ed))
* move non_differentiable to special module for JET masking ([3279fbc](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3279fbce1a3b1779845fc580ff2f1f1b5711d423))
* mutation error for Zygote ([cd69d8d](https://github.com/SymbolicML/DynamicExpressions.jl/commit/cd69d8d25eacf86161d4613375063e877cc3e9f5))
* mutation error for Zygote ([24371c2](https://github.com/SymbolicML/DynamicExpressions.jl/commit/24371c262197a92548eb96889df6e9132d4f1526))

## [1.10.2](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.10.1...v1.10.2) (2025-05-23)


### Bug Fixes

* unthunk according to JuliaDiff/ChainRulesCore.jl[#687](https://github.com/SymbolicML/DynamicExpressions.jl/issues/687) ([4b22d2c](https://github.com/SymbolicML/DynamicExpressions.jl/commit/4b22d2ca94e1c67a03bc3d3405cde9461f20b3ed))
* work around for JuliaDiff/ChainRulesCore.jl[#687](https://github.com/SymbolicML/DynamicExpressions.jl/issues/687) ([d2d7d07](https://github.com/SymbolicML/DynamicExpressions.jl/commit/d2d7d070474c45f611b5785f420606976a54dd44))

## [1.10.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.10.0...v1.10.1) (2025-04-29)


### Bug Fixes

* avoid returning view for generic operators ([43421e0](https://github.com/SymbolicML/DynamicExpressions.jl/commit/43421e095d0e3ba484fd8b94481fd36a3eef5bf4))
* avoid returning view for generic operators ([605e111](https://github.com/SymbolicML/DynamicExpressions.jl/commit/605e1116041a0d36a51b0ee90617a4ca9bea39fd))

## [1.10.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.9.4...v1.10.0) (2025-02-28)


### Features

* allow separate operator names for pretty printing ([e3cd937](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e3cd93761b6802ce0f43555ad7279185c5d44234))
* allow separate operator names for pretty printing ([0f04769](https://github.com/SymbolicML/DynamicExpressions.jl/commit/0f04769a72b54d93ae195e532ee27de36c68e549))

## [1.9.4](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.9.3...v1.9.4) (2025-02-06)


### Features

* overload additional Base operators ([b3ed0b6](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b3ed0b6823f799ebb82ecb6268b95a1b7f9faa1b))

## [1.9.3](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.9.2...v1.9.3) (2025-02-06)


### Features

* infix printing for logical operations ([9c56542](https://github.com/SymbolicML/DynamicExpressions.jl/commit/9c56542c71f5162dd55895a46cf4a95760f680ef))

## [1.9.2](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.9.1...v1.9.2) (2025-01-01)


### Features

* make ArrayBuffer copyable ([6f72270](https://github.com/SymbolicML/DynamicExpressions.jl/commit/6f722706a799d30eac8c705c85bf10713be42250))
* make EvalOptions copyable ([4015f5e](https://github.com/SymbolicML/DynamicExpressions.jl/commit/4015f5ee5015a0aa0913ba5d7e3492acbdecbd24))

## [1.9.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.9.0...v1.9.1) (2024-12-24)


### Bug Fixes

* missing extraction of `operators` in SymbolicUtils convert ([e469bad](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e469badd493c6eaa0ff11589c4505d6d262acdfe))
* missing extraction of `operators` in SymbolicUtils convert ([68ae660](https://github.com/SymbolicML/DynamicExpressions.jl/commit/68ae660c8d83d5340764019cc892bd005bf1bdc5))

## [1.9.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.8.0...v1.9.0) (2024-12-20)


### Features

* allow `nothing` for operators ([175b3d6](https://github.com/SymbolicML/DynamicExpressions.jl/commit/175b3d6f772b8fe0c843be75172db545e472236a))
* allow partial updates to `with_metadata` ([01f01ac](https://github.com/SymbolicML/DynamicExpressions.jl/commit/01f01ac42a13d35017959746968ccc84a7103cdc))
* remove generic metadata unpacker ([e63317c](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e63317c08806be5c8398914554c270216ed7a646))

## [1.8.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.7.0...v1.8.0) (2024-12-13)


### Features

* add `copy_into!` for GraphNode ([95f2bdb](https://github.com/SymbolicML/DynamicExpressions.jl/commit/95f2bdbce4080ba263832b6b326cd70cd14a6ab3))
* add preallocation for abstract structured expression ([eecc9da](https://github.com/SymbolicML/DynamicExpressions.jl/commit/eecc9da652fe965a4169b4696894cd4829ed022f))
* add preallocation utilities for expression ([ed086e7](https://github.com/SymbolicML/DynamicExpressions.jl/commit/ed086e77985672f224be37a84b13fe10b0e77ef3))
* avoid creating dummy nodes ([facdaae](https://github.com/SymbolicML/DynamicExpressions.jl/commit/facdaaebc64e9db587d2754030210eadb5afee93))
* rename to `allocate_container` and `copy_into!` ([1f97a0b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/1f97a0b08ab354d30437d77dd021a166071b38b4))


### Bug Fixes

* add missing `set_node!` for parametric expressions ([34b150b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/34b150bd367d820d3144225e6bab0145d314dc5d))
* various issues in preallocation interface ([b5b40a7](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b5b40a751ac416732b65eda9a8b8d2eca26fcb5e))

## [1.7.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.6.0...v1.7.0) (2024-12-12)


### Features

* compat with turbo mode and buffered evals ([fffaaee](https://github.com/SymbolicML/DynamicExpressions.jl/commit/fffaaeee806a998ad70c3a686792d55727ee77b3))
* create in-place copy operator ([25a1112](https://github.com/SymbolicML/DynamicExpressions.jl/commit/25a1112cd9430bdbaa36b0e07d25991e5139a596))
* remove mixed types warning ([0b5e4a8](https://github.com/SymbolicML/DynamicExpressions.jl/commit/0b5e4a85b092712576edc6ccb732a6495f5772dd))
* require user to pass `ArrayBuffer` object explicitly ([d9d05e2](https://github.com/SymbolicML/DynamicExpressions.jl/commit/d9d05e2966feb54f3e259487b44807b68aad388a))
* towards proper buffering of evals ([56577b8](https://github.com/SymbolicML/DynamicExpressions.jl/commit/56577b8197a9a77f892fa9b70b5ece94ac51a2d0))


### Bug Fixes

* missing arguments ([a7de5e8](https://github.com/SymbolicML/DynamicExpressions.jl/commit/a7de5e829b18295c5a02d729a76f374ce973b5ac))

## [1.6.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.5.1...v1.6.0) (2024-12-07)


### Features

* also use pretty printing for regular `show` ([699fded](https://github.com/SymbolicML/DynamicExpressions.jl/commit/699fded1c67c42c6815742af5d1cf83d77b5451f))
* pretty printing for gradient operators ([913a00c](https://github.com/SymbolicML/DynamicExpressions.jl/commit/913a00cec2305955c20e041907d2ae575ae514a0))

## [1.5.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.5.0...v1.5.1) (2024-12-03)


### Bug Fixes

* non-finite `.val` evaluation issue ([a035f4b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/a035f4bfcb2cce555ada7ab0a9ad69c02e7d7f3e))
* non-finite `.val` evaluation issue ([c73a705](https://github.com/SymbolicML/DynamicExpressions.jl/commit/c73a705ca698c0ecef81397a476d199e0ab3f86f))

## [1.5.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.4.1...v1.5.0) (2024-11-03)


### Features

* deprecate `raw` for `pretty` ([fef28bf](https://github.com/SymbolicML/DynamicExpressions.jl/commit/fef28bf3d225b695b25819c0ff7bba8c8db82851))
* deprecate `raw` for `pretty` ([d5fab73](https://github.com/SymbolicML/DynamicExpressions.jl/commit/d5fab73cebf3a1bdf2d94a11e4da6660cf707764))

## [1.4.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.4.0...v1.4.1) (2024-11-02)


### Bug Fixes

* string interface test ([f80b938](https://github.com/SymbolicML/DynamicExpressions.jl/commit/f80b9383d30b5cb5d791f5e1c78b60109c084338))

## [1.4.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.3.0...v1.4.0) (2024-10-28)


### Features

* allow expression algebra for safe aliases ([b58c299](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b58c29923a78c5220d2fee5641995bd89e133931))
* allow expression algebra for safe aliases ([3fb963d](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3fb963d097f7b07223eebbd0c9832534fb0e15de))

## [1.3.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.2.0...v1.3.0) (2024-10-14)


### ⚠ BREAKING CHANGES

* create `ReadOnlyNode` for `StructuredExpression` `get_tree` access

### Features

* create `ReadOnlyNode` for `StructuredExpression` `get_tree` access ([6cba47f](https://github.com/SymbolicML/DynamicExpressions.jl/commit/6cba47fc40ef776f3cf5c963073c2cca57e456a0))

## [1.2.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.1.0...v1.2.0) (2024-10-13)


### Features

* create `AbstractStructuredExpression` ([f5b645f](https://github.com/SymbolicML/DynamicExpressions.jl/commit/f5b645f73ce48b0d393b115e1104f176ba849208))

## [1.1.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.0.1...v1.1.0) (2024-10-12)


### Bug Fixes

* HACK - remove D-degree capability ([c7ba7fc](https://github.com/SymbolicML/DynamicExpressions.jl/commit/c7ba7fc974016b5d82fb630d208ce6b3c87b4582))

## [1.0.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v1.0.0...v1.0.1) (2024-10-07)

## [1.0.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.19.3...v1.0.0) (2024-10-06)


### ⚠ BREAKING CHANGES

* force `StructuredExpression` construction to be done with kwarg

### Features

* force `StructuredExpression` construction to be done with kwarg ([2e8a6b6](https://github.com/SymbolicML/DynamicExpressions.jl/commit/2e8a6b64714ced0feb2b6e2f295e8e4ed0c27111))


### Bug Fixes

* type parameter ([6183deb](https://github.com/SymbolicML/DynamicExpressions.jl/commit/6183debbdc09bbaf3d73467f1ad28a093bc18dc4))

## [0.19.3](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.19.2...v0.19.3) (2024-08-26)


### Bug Fixes

* compat with older SymbolicUtils ([d931c69](https://github.com/SymbolicML/DynamicExpressions.jl/commit/d931c692d9ecdf1c2267d81109632768268fb3c7))

## [0.19.2](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.19.1...v0.19.2) (2024-08-26)

## [0.19.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.19.0...v0.19.1) (2024-08-01)


### Features

* add functional version of `nif` for easier `@generated` ([e745e3f](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e745e3fabbd09a97393528eb291c99a1df5f5e04))
* use dispatch version instead of immutable list ([d762662](https://github.com/SymbolicML/DynamicExpressions.jl/commit/d7626620c5c4d02af3a313f7268a54475fc6b0cc))


### Bug Fixes

* avoid use of `@generated` for caching strings ([181f962](https://github.com/SymbolicML/DynamicExpressions.jl/commit/181f962eb29ad3cdb50953904ca6ed50094881ae))
* ensure `@generated` functions pure ([3679d50](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3679d50e5c7a76e876f1a950b4973b0c589ea4a7))
* `@lock` unavailable on 1.6 ([730dcf0](https://github.com/SymbolicML/DynamicExpressions.jl/commit/730dcf0b4ad6b634c43cebe20a6feeea58113f9e))
* type instability in `get_op_name` ([ee88b23](https://github.com/SymbolicML/DynamicExpressions.jl/commit/ee88b232b0658b394d520f5b61fcdcfce41b1bbd))

## [0.19.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.18.5...v0.19.0) (2024-07-28)


### Features

* add `ExpressionAlgebraModule`8 and 2-arg `ExpressionAlgebraModule`9 to expression math ([e6ffbca](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e6ffbca95308b65db41df4b7e1aff7a9635e85c5))
* add `ValueInterface` to formalize interface ([0b8bd43](https://github.com/SymbolicML/DynamicExpressions.jl/commit/0b8bd4353edb1d75a0a63f3368ef7d18350eed93))
* allow dispatching on parts of StructuredExpression ([17d0def](https://github.com/SymbolicML/DynamicExpressions.jl/commit/17d0def00eca960333bea493973dbbc4a76e022d))
* allow regular types to be used on expression algebra ([18eb188](https://github.com/SymbolicML/DynamicExpressions.jl/commit/18eb188363f8b5f776209d7801c75d0d80bad1a2))
* allow using math directly on expressions ([39a56cb](https://github.com/SymbolicML/DynamicExpressions.jl/commit/39a56cb2382ba02188a0fbc789e890450853db82))
* also include `types`6 in scalar checks ([09b7a3d](https://github.com/SymbolicML/DynamicExpressions.jl/commit/09b7a3d1acc48821786a024fe5e38f9f7356382f))
* change `types`0 to always print with pretty string ([19e44c0](https://github.com/SymbolicML/DynamicExpressions.jl/commit/19e44c00087961a287f7dcfdcf393d5910aa76ac))
* create `StructuredExpression` ([4aa516f](https://github.com/SymbolicML/DynamicExpressions.jl/commit/4aa516f9a3236a901d9bd60883c1691424f828b9))
* create `ExpressionAlgebraModule`5 and `ExpressionAlgebraModule`6 ([79d2826](https://github.com/SymbolicML/DynamicExpressions.jl/commit/79d28264cec5c268a93470b197c5683ec30fc483))
* only warn if defining helper functions ([1b576fe](https://github.com/SymbolicML/DynamicExpressions.jl/commit/1b576fe42364bb1c15abe1370704bfc377e051dc))
* use safer `;`9 syntax ([b162f75](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b162f75a0280d937746ff4f7b3eaefa17304e544))
* warn for `;`8 ([5ce69c5](https://github.com/SymbolicML/DynamicExpressions.jl/commit/5ce69c5ba7ba0544dfb84078891e3712788cc1f9))


### Bug Fixes

* equality check between dims ([398b640](https://github.com/SymbolicML/DynamicExpressions.jl/commit/398b640e303151e06b09ae45fc6142c7b3b68d43))
* error catching for generic eval ([2fc5e87](https://github.com/SymbolicML/DynamicExpressions.jl/commit/2fc5e8792f61eba0344e7ee56452cc1ad817036e))
* fix generic eval errors ([17ae595](https://github.com/SymbolicML/DynamicExpressions.jl/commit/17ae595b6e8e979f637239628ce2a4258eda4cad))
* incorporate `types`7 in LoopVectorization extension ([17a4a24](https://github.com/SymbolicML/DynamicExpressions.jl/commit/17a4a24ef4e93f68fe8cebc080fa55cfedf87ada))
* `;`1 checker of packing ([e2e9b21](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e2e9b21ef0c01b4aeecd270a259057b6f5738e21))
* `ExpressionAlgebraModule`2 typo ([204a9df](https://github.com/SymbolicML/DynamicExpressions.jl/commit/204a9df7bf53a020885a20fdee450cd654e743e4))
* mark `ExpressionAlgebraModule`0 ([7ff927b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/7ff927b93d6e5e0f757855fb090bfa26b296ae29))
* merge edits to eval options ([c0b5a46](https://github.com/SymbolicML/DynamicExpressions.jl/commit/c0b5a461d0a42d789624e1e128c2ddf829cef7a7))
* name of ValueInterfaceModule ([571518c](https://github.com/SymbolicML/DynamicExpressions.jl/commit/571518c4c50300389075d213dca52e4000a29211))
* note instability in kw deprecation ([3d7b529](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3d7b52941e45b715dc714e73c8ff635b1f6889ae))
* some issues in expression math ([4872503](https://github.com/SymbolicML/DynamicExpressions.jl/commit/487250348acca0f470679babf082d9177bb788ee))
* specific branch calls ([5399625](https://github.com/SymbolicML/DynamicExpressions.jl/commit/539962589cd33b05267608868a18f23bc07572ac))
* type unstalbe tests ([6302012](https://github.com/SymbolicML/DynamicExpressions.jl/commit/63020121e946b18e1cfb39e8e0f32ab02275171f))
* unbound type parameter ([7237f1d](https://github.com/SymbolicML/DynamicExpressions.jl/commit/7237f1d833fe99db74bdad7cfea73f8ab8ab4ae7))
* update names of constant getters ([ffe1461](https://github.com/SymbolicML/DynamicExpressions.jl/commit/ffe14612ca297ade98ae75c6d2bab3c382f62861))
* update precompile function names ([2189c7f](https://github.com/SymbolicML/DynamicExpressions.jl/commit/2189c7fbaa1b54811892d73d71f6de32a0efa7f7))

## [0.18.5](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.18.4...v0.18.5) (2024-07-01)


### Features

* avoid double descent in `rrule` ([fc2497b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/fc2497be2271c83bd4b5a1fcb9dbcf28cc0b65d4))


### Bug Fixes

* JET errors in non differentiable errors ([1e7cf1d](https://github.com/SymbolicML/DynamicExpressions.jl/commit/1e7cf1d06f21bc672e7d7f5b0adacc124e270be6))
* remove CRC import ([c9eaedf](https://github.com/SymbolicML/DynamicExpressions.jl/commit/c9eaedf63e36a227db702b4ea3257938892447d5))
* some JET errors ([3392ab5](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3392ab50ede204b68de154186a9cfc52a94a55b5))

## [0.18.4](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.18.3...v0.18.4) (2024-06-29)


### Features

* introduce generic `extract_gradients` for flattening gradients ([9963ef5](https://github.com/SymbolicML/DynamicExpressions.jl/commit/9963ef572750ea5604502f1b3880add83264e43f))


### Bug Fixes

* `extract_gradient` for regular expressions ([ed5deaa](https://github.com/SymbolicML/DynamicExpressions.jl/commit/ed5deaae5e4b885815ea3de952b3028425512e61))

## [0.18.3](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.18.2...v0.18.3) (2024-06-29)


### Features

* add chain rules for parametric expression ([bcfc360](https://github.com/SymbolicML/DynamicExpressions.jl/commit/bcfc3608ccf8efb6dc9bd3e3783daaa175778a97))
* define chain rule for `convert` rather than `eval_tree_array` ([2711934](https://github.com/SymbolicML/DynamicExpressions.jl/commit/2711934f33b07feb02a37cbb4dbe9d1c9fa0a4b6))
* turn off node tangent assertions ([a2f5673](https://github.com/SymbolicML/DynamicExpressions.jl/commit/a2f567337d1d3a7fbe09cd332561143450881865))

## [0.18.2](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.18.1...v0.18.2) (2024-06-24)


### Features

* add `is_node_constant` to interface ([bf6c403](https://github.com/SymbolicML/DynamicExpressions.jl/commit/bf6c4032c224d2a4e8a9f672819d05c185d53137))


### Bug Fixes

* generalize combine_operators ([1d86cc2](https://github.com/SymbolicML/DynamicExpressions.jl/commit/1d86cc207a3683d5d086a345b05e503d4a447da2))

## [0.18.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.18.0...v0.18.1) (2024-06-24)


### Features

* generalize get_operators ([1fcdfce](https://github.com/SymbolicML/DynamicExpressions.jl/commit/1fcdfce756d0c48bc2658c2f49063812dd49ca2f))


### Bug Fixes

* jet issues with ambiguous methods ([5bcf0a7](https://github.com/SymbolicML/DynamicExpressions.jl/commit/5bcf0a746df991f3bfcd058af94bfb86bae96dfa))
* various method ambiguities ([a739df2](https://github.com/SymbolicML/DynamicExpressions.jl/commit/a739df290f9b7f5bcedb99968a806569a61b7a90))
* various method ambiguities ([acf3dbc](https://github.com/SymbolicML/DynamicExpressions.jl/commit/acf3dbc7a37643f67a9e7fd76a41f0d029c95696))

## [0.18.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.18.0-alpha.1...v0.18.0) (2024-06-24)


### Features

* default node type for nothing ([717eb4f](https://github.com/SymbolicML/DynamicExpressions.jl/commit/717eb4fa08a574f33c181d8ce7c51401f405b83b))
* more extensible expression interface ([c8010da](https://github.com/SymbolicML/DynamicExpressions.jl/commit/c8010da1ed5073ddf18ec16d56b65698f41302f7))


### Bug Fixes

* case of parameter_names as none ([4066428](https://github.com/SymbolicML/DynamicExpressions.jl/commit/406642824cee15813c4074085e264facd94991f0))
* declare instability in constructorof ([ed3e497](https://github.com/SymbolicML/DynamicExpressions.jl/commit/ed3e4978cca205f632a018f4673be071477d884e))
* declare instability in constructorof for expressions ([3dbded5](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3dbded54339561de8cd7f3260b906f25f4c91862))
* issue with varying order of fields ([55d5e6e](https://github.com/SymbolicML/DynamicExpressions.jl/commit/55d5e6e23e7f8e3cf37919c35a2c20c4e180005e))
* with_metadata to handle kwargs properly ([3f30a23](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3f30a23a0bcf5769c3f7bcb7cf9d325cca304f2b))

## [0.18.0-alpha.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.17.0...v0.18.0-alpha.1) (2024-06-23)


### ⚠ BREAKING CHANGES

* modify expression to allow additional features
* pass keywords rather than named tuple to expression
* rename to `result_type` and use `Val` type

### Features

* add additional parts of node interface ([08333dd](https://github.com/SymbolicML/DynamicExpressions.jl/commit/08333dd56a6a185cd65de52c9b31acfeec92e50c))
* add example of parametric expression ([cdbac2c](https://github.com/SymbolicML/DynamicExpressions.jl/commit/cdbac2c5325cc5ec1a048df21eb780aaca4c282a))
* add Interfaces.jl implementation ([f156b86](https://github.com/SymbolicML/DynamicExpressions.jl/commit/f156b8652313f4fc97908f781f7b3ddcf1af91a2))
* allow empty operators ([5d511aa](https://github.com/SymbolicML/DynamicExpressions.jl/commit/5d511aac9dd77b29c6e0443afce92140b14f902e))
* allow `@parse_expression`0 to take arg not kwarg ([d6fc222](https://github.com/SymbolicML/DynamicExpressions.jl/commit/d6fc222800a0ee897b739874b5b705c8241e66e6))
* allow `@parse_expression`6 passed to `@parse_expression`7 ([23b1bd3](https://github.com/SymbolicML/DynamicExpressions.jl/commit/23b1bd344fdd70e03ff31fe71ca44b961f21c842))
* allow user to overload pieces of copy, hash, and isequal ([e45c7c8](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e45c7c88ed62fcf784ec0844fa44a15537289986))
* allow user to pass external functions to `@parse_expression` ([48fd127](https://github.com/SymbolicML/DynamicExpressions.jl/commit/48fd12704ce4dd657d13b675dfdd2a83d42c7a63))
* create `Expression` type and `@parse_expression` macro ([c1343c2](https://github.com/SymbolicML/DynamicExpressions.jl/commit/c1343c26edeaa1716431f93a0baaa7c4bd33c48b))
* create `@parse_expression`8 type to store operators with expression and `@parse_expression`9 to have robust parsing ([a55f966](https://github.com/SymbolicML/DynamicExpressions.jl/commit/a55f966826cf9f46dda515b75d68ad0c1a5890c4))
* export default_node ([15817a3](https://github.com/SymbolicML/DynamicExpressions.jl/commit/15817a39adf36b8130072ff90dc8b7f14b3d5cf3))
* export node_type ([97256f9](https://github.com/SymbolicML/DynamicExpressions.jl/commit/97256f970d42281c422f6364b18c8990ce190f3e))
* export with_type_parameters ([e1f938b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e1f938b85f72dee1b37bf7cc69e0fcca6c233ddb))
* generalize parsing to different expression types ([ac7420d](https://github.com/SymbolicML/DynamicExpressions.jl/commit/ac7420dd183e04b23aeb09518162b9a499214339))
* make parse_expression work for tuple and vector leafs ([44e541b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/44e541b6ff4ffb92c29d90464622ddf5d571efd3))
* move parametric expression inside package ([8c78596](https://github.com/SymbolicML/DynamicExpressions.jl/commit/8c78596679798bc5f3f729d39b17ceb3d094e83e))
* pass keywords rather than named tuple to expression ([c11ad9e](https://github.com/SymbolicML/DynamicExpressions.jl/commit/c11ad9e016f5add65af64e579487a27655fbef98))
* rename to `@parse_expression`3 and use `@parse_expression`4 type ([b2c1658](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b2c1658867682ebd3dab17f4a9b6f70145c23ff8))


### Bug Fixes

* account for parsing chained operators ([a7c8580](https://github.com/SymbolicML/DynamicExpressions.jl/commit/a7c85805e18fbe658a26fee5fe8ddf3190c20804))
* add break_sharing kw to copy ([9fd8265](https://github.com/SymbolicML/DynamicExpressions.jl/commit/9fd8265c4b942a9836ca8eabe6ce663e84780c8d))
* add missing `*` special behavior ([3ea426a](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3ea426ae370a685871ab9b1e02b7ded8692f0c74))
* add missing parts of interface ([8bd5b78](https://github.com/SymbolicML/DynamicExpressions.jl/commit/8bd5b783a1e59674c18eb5de69e77bc09a1ad471))
* allow `display_variable_names`2 to return parametrized type ([b002a1c](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b002a1cfc83bd8bb7540112d45b37b1ed2e15a85))
* compat of Interfaces.jl ([6372860](https://github.com/SymbolicML/DynamicExpressions.jl/commit/637286077ce5b867c6f3ad8f35f6c3ea9f3a0800))
* compat with newer matrix syntax ([6dace82](https://github.com/SymbolicML/DynamicExpressions.jl/commit/6dace8212880d9be67af397ccc680597b8418184))
* destructuring syntax for Julia 1.6 ([666ad1b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/666ad1b643fb9b42c7d04912d2204017cebd242e))
* eltype issue in parse ([76abcde](https://github.com/SymbolicML/DynamicExpressions.jl/commit/76abcde424148b8fe81009b3e1dad66893c0fff2))
* generalisation issue with parameter names ([222754b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/222754bf6e4e9968a8f70c743614d5c6db184099))
* implement various edge cases for Expression ([6d238ec](https://github.com/SymbolicML/DynamicExpressions.jl/commit/6d238ecf4a70e0a58c527037e9b19c65dc88f3f1))
* incorrect import of get_constants ([3734f5a](https://github.com/SymbolicML/DynamicExpressions.jl/commit/3734f5ae4cc9b3de2c4a1b15ce3ad3d9d387d5ef))
* `@parse_expression`1 for custom expressions ([224dca4](https://github.com/SymbolicML/DynamicExpressions.jl/commit/224dca49f79e10d17340745feadb28351a9b83b9))
* issues spotted by JET ([b70e7f7](https://github.com/SymbolicML/DynamicExpressions.jl/commit/b70e7f7a49cd78b97699e5fe3200ceae5a83cbe3))
* jet error ([7b20687](https://github.com/SymbolicML/DynamicExpressions.jl/commit/7b20687c8fee55ceb85731627164636fffb893fe))
* leaf_equal for parametric node test ([5a8d7b2](https://github.com/SymbolicML/DynamicExpressions.jl/commit/5a8d7b2eb0cc7398880005d2c14b0fcfe7271300))
* name conflict ([6c6485a](https://github.com/SymbolicML/DynamicExpressions.jl/commit/6c6485a32a63436cee23e0ff837d8653cd6ec223))
* note instability in parse_leaf ([8af3c2c](https://github.com/SymbolicML/DynamicExpressions.jl/commit/8af3c2c703ceda271465a3ea237dbdd3d10747d6))
* only validate Expression input for OperatorEnum ([a6716f5](https://github.com/SymbolicML/DynamicExpressions.jl/commit/a6716f519f69539e8dbccbd15c1dd9c84ea71c4d))
* some instabilities in base ([ce84be1](https://github.com/SymbolicML/DynamicExpressions.jl/commit/ce84be143ad201f71e35aa87c8d676566fd887bc))
* stack overflow with bad struct def ([51a1a93](https://github.com/SymbolicML/DynamicExpressions.jl/commit/51a1a93abb6eeae744a68b1fa7fd80a029744f47))
* tag instabilities in expressions ([fa01ed6](https://github.com/SymbolicML/DynamicExpressions.jl/commit/fa01ed693e6ee1f5c20be3045fb819f16205b76e))
* type constraints ([8c51d5c](https://github.com/SymbolicML/DynamicExpressions.jl/commit/8c51d5c770727a297fc906356c2079de67c37ce6))
* type instability in `@parse_expression`5 ([c850e97](https://github.com/SymbolicML/DynamicExpressions.jl/commit/c850e972d09054f3d907d1ff2f81bdcb71352a1c))
* type instability in `display_variable_names`0 ([116ab1a](https://github.com/SymbolicML/DynamicExpressions.jl/commit/116ab1abf2093fdff72726268e37fca301dcb052))
* type instability in `display_variable_names`1 ([538abcd](https://github.com/SymbolicML/DynamicExpressions.jl/commit/538abcdbbe47edb8b80fd61766e6702583d1e0b3))
* type instability in map ([e48ddae](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e48ddae24a4cae3ea22531faa79d1d99c9099b19))
* type unstable copy ([950e95b](https://github.com/SymbolicML/DynamicExpressions.jl/commit/950e95bd994e79c16f1e1d4ecefde9005926d736))


### Code Refactoring

* modify expression to allow additional features ([e0cbced](https://github.com/SymbolicML/DynamicExpressions.jl/commit/e0cbced6a62a7fb27fb5e4dc0312c23b9fa1c2dc))

## [0.17.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.16.0...v0.17.0) (2024-04-29)


### ⚠ BREAKING CHANGES

* simplify expression optimization routine
* module names to match struct names

### Features

* add ChainRulesCore.rrule for eval_tree_array ([1aabe1d](https://github.com/SymbolicML/DynamicExpressions.jl/commit/1aabe1d55829492d5a332c00a771f9fe991619b4))
* simplify expression optimization routine ([21acfa1](https://github.com/SymbolicML/DynamicExpressions.jl/commit/21acfa1db9d6533ab094f9cee3c9707d6dafa788))


### Code Refactoring

* module names to match struct names ([fe19c14](https://github.com/SymbolicML/DynamicExpressions.jl/commit/fe19c145f309077b12c6658b034f9ba1c8345aec))

## [0.16.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.15.0...v0.16.0) (2024-03-09)

## [0.15.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.14.1...v0.15.0) (2024-02-03)

## [0.14.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.14.0...v0.14.1) (2023-12-27)

## [0.14.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.13.1...v0.14.0) (2023-12-19)

## [0.13.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.13.0...v0.13.1) (2023-08-13)

## [0.13.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.12.3...v0.13.0) (2023-08-12)

## [0.12.3](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.12.2...v0.12.3) (2023-08-08)

## [0.12.2](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.12.1...v0.12.2) (2023-08-05)

## [0.12.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.12.0...v0.12.1) (2023-08-02)

## [0.12.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.11.0...v0.12.0) (2023-08-02)

## [0.11.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.10.1...v0.11.0) (2023-07-22)

## [0.10.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.10.0...v0.10.1) (2023-06-19)

## [0.10.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.9.0...v0.10.0) (2023-06-19)

## [0.9.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.8.1...v0.9.0) (2023-05-26)

## [0.8.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.8.0...v0.8.1) (2023-05-12)

## [0.8.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.7.0...v0.8.0) (2023-05-08)

## [0.7.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.6.1...v0.7.0) (2023-04-27)

## [0.6.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.6.0...v0.6.1) (2023-04-26)

## [0.6.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.5.1...v0.6.0) (2023-04-15)

## [0.5.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.5.0...v0.5.1) (2023-04-13)

## [0.5.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.4.3...v0.5.0) (2023-03-19)

## [0.4.3](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.4.2...v0.4.3) (2023-03-17)

## [0.4.2](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.4.1...v0.4.2) (2022-11-27)

## [0.4.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.4.0...v0.4.1) (2022-11-13)

## [0.4.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.3.2...v0.4.0) (2022-10-30)

## [0.3.2](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.3.1...v0.3.2) (2022-10-27)

## [0.3.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.3.0...v0.3.1) (2022-10-27)

## [0.3.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.2.3...v0.3.0) (2022-10-23)

## [0.2.3](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.2.2...v0.2.3) (2022-10-22)

## [0.2.2](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.2.1...v0.2.2) (2022-10-22)

## [0.2.1](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.2.0...v0.2.1) (2022-10-21)

## [0.2.0](https://github.com/SymbolicML/DynamicExpressions.jl/compare/v0.1.0...v0.2.0) (2022-10-20)

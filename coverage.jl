using Coverage

const EXCLUDED_COVERAGE_FILES = Set([normpath(joinpath(pwd(), "src", "ArenaNode.jl"))])

function filter_coverage!(coverage)
    return filter!(coverage) do file_coverage
        path = normpath(
            if isabspath(file_coverage.filename)
                file_coverage.filename
            else
                joinpath(pwd(), file_coverage.filename)
            end,
        )
        return path ∉ EXCLUDED_COVERAGE_FILES
    end
end

# process '*.cov' files
coverage = process_folder() # defaults to src/; alternatively, supply the folder name as argument
push!(coverage, process_folder("ext")...)
filter_coverage!(coverage)

LCOV.writefile("lcov.info", coverage)

# process '*.info' files
coverage = merge_coverage_counts(
    coverage,
    filter!(
        let prefixes = (joinpath(pwd(), "src", ""), joinpath(pwd(), "ext", ""))
            c -> any(p -> startswith(c.filename, p), prefixes)
        end,
        LCOV.readfolder("test"),
    ),
)
filter_coverage!(coverage)
# Get total coverage for all Julia files
covered_lines, total_lines = get_summary(coverage)
@show covered_lines, total_lines

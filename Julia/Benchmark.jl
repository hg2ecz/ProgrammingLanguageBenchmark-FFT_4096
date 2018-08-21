push!(LOAD_PATH, ".")
using Fft:fft

LOG2POINT = 12

SIZE = 1<<LOG2POINT

xy_in = append!(
    [1.0 + 0.0im for i=1:SIZE/2],
    [-1.0 + 0.0im for i=SIZE/2+1:SIZE]
)

t_start = time_ns()
xy_out = fft(LOG2POINT, xy_in)
t_end = time_ns()

println((t_end-t_start)/1e6, " msec")
[println(xy_out[i]) for i=1:6]

#!/usr/bin/julia

push!(LOAD_PATH, ".")
using Fft:fft, fft_init

LOG2POINT = 12
REPEAT = 1000

SIZE = 1<<LOG2POINT

xy_in = append!(
    [1.0 + 0.0im for i=1:SIZE/2],
    [-1.0 + 0.0im for i=SIZE/2+1:SIZE]
)

phasevec = fft_init()
xy_out = fft(LOG2POINT, xy_in, phasevec)

t_start = time_ns()
for i=1:REPEAT
    xy_out = fft(LOG2POINT, xy_in, phasevec)
end
t_end = time_ns()

println((t_end-t_start)/1e6/REPEAT, " msec")
[println(xy_out[i]) for i=1:6]

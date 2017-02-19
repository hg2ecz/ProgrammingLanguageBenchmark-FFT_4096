# 4096 point FFT test result

- Raspberry Pi3: ARM Cortex A53, 1.2 GHz, 32 bit Raspbian 9 beta (gcc-6.2)
- Odroid-C2: ARM Cortex A53, 1.5 GHz, 64 bit Ubuntu 17.04 alpha (gcc-6.3)
- Intel i5-3337u: Ivy Bridge, 1.8 GHz (turbo 2,7 GHz), 64 bit Ubuntu 16.10 (gcc-6.2)

<p>C relative speed compared with C-fast_double; floatemu if not FPU in the processor (e.g. some 32 bit microcontroller)</p>

<table border="1">
<tr><th>Language</th><th>Raspberry Pi3</th><th>Odroid-C2@64 bit</th><th>i5-3337u</th></tr>
<tr><td>C-fast_double</td><td>1 (0.915 ms)</td><td>1 (0.726 ms)</td><td>1 (0.180 ms)</td></tr>
<tr><td>C-fast_float</td><td>0.82</td><td>0.74</td><td>0.69</td></tr>
<tr><td>C-fast_integer</td><td>1.12</td><td>0.58</td><td>0.79</td></tr>
<tr><td>C-fast_longlong_64bit</td><td>-</td><td>1.46</td><td>1.38</td></tr>
<tr><td>C-fast_float-nocomplex-floatemu</td><td>4.77</td><td>4.21</td><td>5.87</td></tr>
</table>

<p>Other compiled language speed compared with C-fast_double</p>

<table border="1">
<tr><th>Language</th><th>Raspberry Pi3</th><th>Odroid-C2@64 bit</th><th>i5-3337u</th></tr>
<tr><td>Go (gccgo-6.3)</td><td>1.45</td><td>1.30</td><td>1.49</td></tr>
<tr><td>Java (Oracle)</td><td>7.94</td><td> </td><td>1.38</td></tr>
<tr><td>C# (mono-4.x)</td><td>8.32</td><td>5.73</td><td>5.22</td></tr>
<tr><td>F# (fsharpc 4.0)</td><td>8.31</td><td> </td><td>4.93</td></tr>
</table>

<p>Script language speed compared with C-fast_double</p>

<table border="1">
<tr><th>Language</th><th>Raspberry Pi3</th><th>Odroid-C2@64 bit</th><th>i5-3337u</th></tr>
<tr><td>LUA (LUAjit)</td><td>8.01</td><td>8.49</td><td>5.4</td></tr>
<tr><td>Python (Pypy 5.4)</td><td>27.5</td><td> </td><td>4.91</td></tr>
<tr><td>JavaScript (nodejs 4.x)</td><td>25</td><td>23.7</td><td>17.3 </td></tr>
<tr><td>PHP7</td><td>129</td><td>135</td><td>88.2</td></tr>
<tr><td>Ruby</td><td>481</td><td>395</td><td>287</td></tr>
<tr><td>AWK</td><td>625</td><td>541</td><td>499</td></tr>
<tr><td>PERL</td><td>5139</td><td>5377</td><td>5710</td></tr>
<tr><td>Octave</td><td>9326</td><td> </td><td>7972</td></tr>
</table>
</pre>

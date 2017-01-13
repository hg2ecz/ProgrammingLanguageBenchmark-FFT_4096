# 4096 point FFT test result

- Raspberry Pi3: ARM Cortex A53, 1.2 GHz, 32 bit Raspbian 9 beta
- Odroid-C2: ARM Cortex A53, 1.5 GHz, 64 bit Ubuntu 16.04
- Intel i5-3337u: Ivy Bridge, 1.8 GHz (turbo 2,7 GHz), 64 bit Ubuntu 16.04

<p>C relative speed compared with C-fast_double; floatemu if not FPU in the processor (e.g. some 32 bit microcontroller)</p>

<table border="1">
<tr><th>Language</th><th>Raspberry Pi3</th><th>Odroid-C2@64 bit</th><th>i5-3337u</th></tr>
<tr><td>C-fast_double</td><td>1 (0.915 ms)</td><td>1 (0.787 ms)</td><th>1 (0.180 ms)</th></tr>
<tr><td>C-fast_float</td><td>0.82</td><td>0.72</td><th>0.69</th></tr>
<tr><td>C-fast_integer</td><td>1.12</td><td>0.52</td><th>0.79</th></tr>
<tr><td>C-fast_longlong_64bit</td><td>-</td><td>1.40</td><th>1.38</th></tr>
<tr><td>C-fast_float-nocomplex-floatemu</td><td>4.77</td><td>3.92</td><th>5.87</th></tr>
</table>

<p>Other compiled language speed compared with C-fast_double</p>

<table border="1">
<tr><th>Language</th><th>Raspberry Pi3</th><th>Odroid-C2@64 bit</th><th>i5-3337u</th></tr>
<tr><td>Java (Oracle)</td><td>7.94</td><td> </td><th>1.38</th></tr>
<tr><td>C#</td><td>8.32</td><td>5.30</td><th>5.22</th></tr>
<tr><td>F#</td><td>8.31</td><td> </td><th>4.93</th></tr>
</table>

<p>Script language speed compared with C-fast_double</p>

<table border="1">
<tr><th>Language</th><th>Raspberry Pi3</th><th>Odroid-C2@64 bit</th><th>i5-3337u</th></tr>
<tr><td>LUA (LUAjit)</td><td>8.01</td><td>70.8 (beta)</td><th>5.4</th></tr>
<tr><td>Python (Pypy 5.4)</td><td>27.5</td><td>183</td><th>4.91</th></tr>
<tr><td>JavaScript (nodejs 4.x)</td><td>25</td><td>22 </td><th>17.3 </th></tr>
<tr><td>PHP7</td><td>129</td><td>121</td><th>88.2</th></tr>
<tr><td>AWK</td><td>625</td><td>499</td><th>499</th></tr>
<tr><td>PERL</td><td>5139</td><td>5386</td><th>5710</th></tr>
<tr><td>Octave</td><td>9326</td><td> </td><th>7972</th></tr>
</table>
</pre>

# 4096 point FFT test result

- Raspberry Pi3: ARM Cortex A53, 1.2 GHz, 32 bit Raspbian 9 beta
- Odroid-C2: ARM Cortex A53, 1.5 GHz, 64 bit Ubuntu 16.04
- Intel i5-3337u: Ivy Bridge, 1.8 GHz (turbo 2,7 GHz), 64 bit Ubuntu 16.04

<p>C relative speed compared with C-fast_double; floatemu if not FPU in the processor (e.g. some 32 bit microcontroller)</p>

<table border="1">
<tr><th>Language</th><th>Raspberry Pi3</th><th>Odroid-C2@64 bit</th><th>i5-3337u</th></tr>
<tr><td>C-fast_double</td><td>1 (0.915 ms)</td><td>1 (0.787 ms)</td><td>1 (0.180 ms)</td></tr>
<tr><td>C-fast_float</td><td>0.82</td><td>0.72</td><td>0.69</td></tr>
<tr><td>C-fast_integer</td><td>1.12</td><td>0.52</td><td>0.79</td></tr>
<tr><td>C-fast_longlong_64bit</td><td>-</td><td>1.40</td><td>1.38</td></tr>
<tr><td>C-fast_float-nocomplex-floatemu</td><td>4.77</td><td>3.92</td><td>5.87</td></tr>
</table>

<p>Other compiled language speed compared with C-fast_double</p>

<table border="1">
<tr><th>Language</th><th>Raspberry Pi3</th><th>Odroid-C2@64 bit</th><th>i5-3337u</th></tr>
<tr><td>Java (Oracle)</td><td>7.94</td><td> </td><td>1.38</td></tr>
<tr><td>C# (mono-4.x)</td><td>8.32</td><td>5.30</td><td>5.22</td></tr>
<tr><td>F# (fsharpc 4.0)</td><td>8.31</td><td> </td><td>4.93</td></tr>
</table>

<p>Script language speed compared with C-fast_double</p>

<table border="1">
<tr><th>Language</th><th>Raspberry Pi3</th><th>Odroid-C2@64 bit</th><th>i5-3337u</th></tr>
<tr><td>LUA (LUAjit)</td><td>8.01</td><td> </td><td>5.4</td></tr>
<tr><td>Python (Pypy 5.4)</td><td>27.5</td><td> </td><td>4.91</td></tr>
<tr><td>JavaScript (nodejs 4.x)</td><td>25</td><td>22 </td><td>17.3 </td></tr>
<tr><td>PHP7</td><td>129</td><td>121</td><td>88.2</td></tr>
<tr><td>AWK</td><td>625</td><td>499</td><td>499</td></tr>
<tr><td>PERL</td><td>5139</td><td>5386</td><td>5710</td></tr>
<tr><td>Octave</td><td>9326</td><td> </td><td>7972</td></tr>
</table>
</pre>

dotnet publish -r win-x64 -o dist-windows
"c:\Program Files (x86)\Inno Setup 5\ISCC.exe" Setup.iss /Qp /O. /FSetup

if exist "C:\Program Files\FFT Benchmark\1.3\unins000.exe" "C:\Program Files\FFT Benchmark\1.3\unins000.exe"
Setup.exe

dotnet publish -r win-x64 -o dist-windows
"c:\Program Files (x86)\WiX Toolset v3.11\bin\candle.exe" Setup.wxs
REM "c:\Program Files (x86)\WiX Toolset v3.11\bin\heat.exe" dir dist-windows -dr dist-windows -gg -o Files.wxs
REM "c:\Program Files (x86)\WiX Toolset v3.11\bin\candle.exe" Files.wxs
"c:\Program Files (x86)\WiX Toolset v3.11\bin\light.exe" Setup.wixobj
REM msiexec /x Setup.msi
msiexec /i Setup.msi

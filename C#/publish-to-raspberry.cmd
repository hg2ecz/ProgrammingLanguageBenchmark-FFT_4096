@echo off

echo ----------------------------------------------------------------
echo Building the source
echo ----------------------------------------------------------------

dotnet restore
dotnet publish -r linux-arm -c Release -o dist-arm

echo ----------------------------------------------------------------
echo Copying artifacts
echo ----------------------------------------------------------------

xcopy dist-arm\* packaging\opt\fft-benchmark /E/I/Y

echo ----------------------------------------------------------------
echo Deploying artifacts
echo ----------------------------------------------------------------

"c:\Program Files\pscp.exe" -i c:\Users\robert.fuszenecker\ssh_login.ppk -r -C packaging fuszenecker@192.168.1.200:

echo ----------------------------------------------------------------
echo Post-deploy setup
echo ----------------------------------------------------------------

"c:\Program Files\plink.exe" -i c:\Users\robert.fuszenecker\ssh_login.ppk fuszenecker@192.168.1.200 "chmod 755 packaging/opt/fft-benchmark/fft-benchmark"

echo ----------------------------------------------------------------
echo Creating packages
echo ----------------------------------------------------------------

"c:\Program Files\plink.exe" -i c:\Users\robert.fuszenecker\ssh_login.ppk fuszenecker@192.168.1.200 "dpkg-deb -b packaging fft-benchmark-1.3.armhf.deb"

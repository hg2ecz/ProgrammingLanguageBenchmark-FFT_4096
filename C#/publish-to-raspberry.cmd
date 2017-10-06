@echo off

echo ----------------------------------------------------------------
echo Configuration
echo ----------------------------------------------------------------

set ssh_key="c:\Users\robert.fuszenecker\ssh_login.ppk"
set remote="fuszenecker@192.168.1.200"

set pscp="c:\Program Files\pscp.exe"
set plink="c:\Program Files\plink.exe"

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

%pscp% -i %ssh_key% -r -C packaging %remote%:

echo ----------------------------------------------------------------
echo Post-deploy setup
echo ----------------------------------------------------------------

echo Setting permissions of fft-benchmark
%plink% -i %ssh_key% %remote% "chmod 755 packaging/opt/fft-benchmark/fft-benchmark"

echo Setting permissions of postinst
%plink% -i %ssh_key% %remote% "chmod 755 packaging/DEBIAN/postinst"

echo ----------------------------------------------------------------
echo Creating packages
echo ----------------------------------------------------------------

%plink% -i %ssh_key% %remote% "dpkg-deb -b packaging fft-benchmark.armhf.deb"

echo ----------------------------------------------------------------
echo Downloading package
echo ----------------------------------------------------------------

%pscp% -i %ssh_key% -r -C %remote%:fft-benchmark.armhf.deb .

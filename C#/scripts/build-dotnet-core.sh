dotnet restore
dotnet publish -r linux-arm -c Release -o dist-arm
dotnet publish -r linux-arm64 -c Release -o dist-arm64
dotnet publish -r linux-x64 -c Release -o dist-linux64
dotnet publish -r win10-x64 -c Release -o dist-win64

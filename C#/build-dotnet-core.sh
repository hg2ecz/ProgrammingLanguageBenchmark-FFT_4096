dotnet restore
dotnet publish -r linux-arm -c Release -o dist-arm
dotnet publish -r linux-x64 -c Release -o dist-x86_64

dotnet restore
dotnet build -c Release
dotnet publish -r linux-arm -c Release
dotnet publish -r linux-x64 -c Release


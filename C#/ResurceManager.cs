using System.Globalization;
using System.Reflection;
using System.Resources;

namespace CSharpFftDemo;

internal static class GlobalResourceManager
{
    private static readonly ResourceManager resourceManager = new ResourceManager("FftBenchmark.Resources.Strings",
        Assembly.GetExecutingAssembly());

    public static string? GetStringResource(string name)
    {
        return resourceManager.GetString(name, CultureInfo.CurrentUICulture);
    }
}
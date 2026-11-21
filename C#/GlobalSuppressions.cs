using System.Diagnostics.CodeAnalysis;

[assembly: SuppressMessage("Security", "CA5393:Do not use unsafe DllImportSearchPath value", Justification = "The native benchmark libraries are shipped next to the assembly.")]
[assembly: SuppressMessage("Maintainability", "CA1515:Consider making public types internal", Justification = "BenchmarkDotNet requires public benchmark classes.")]
[assembly: SuppressMessage("Globalization", "CA1303:Do not pass literals as localized parameters", Justification = "Console output of a benchmark tool is not localized.")]

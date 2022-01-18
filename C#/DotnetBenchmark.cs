using System.Numerics;
using BenchmarkDotNet.Attributes;

namespace CSharpFftDemo;

[MediumRunJob]
[MarkdownExporterAttribute.GitHub]
[MinColumn, MaxColumn]
public class DotnetBenchmark
{
    private const int size = 1 << Params.Log2FftSize;
    readonly Complex[] xyManaged = new Complex[size];
    readonly Complex[] xyOutManaged = new Complex[size];

    private readonly FftNative.DoubleComplex[] xyNative = new FftNative.DoubleComplex[size];
    private readonly FftNative.DoubleComplex[] xyOutNative = new FftNative.DoubleComplex[size];


    public DotnetBenchmark()
    {
        int i;

        for (i = 0; i < size / 2; i++)
            xyManaged[i] = new Complex(1.0, 0.0);

        for (i = size / 2; i < size; i++)
            xyManaged[i] = new Complex(-1.0, 0.0);

        for (i = 0; i < size / 2; i++)
            xyNative[i] = new FftNative.DoubleComplex(1.0f, 0.0f);

        for (i = size / 2; i < size; i++)
            xyNative[i] = new FftNative.DoubleComplex(-1.0f, 0.0f);
    }

    [Benchmark]
    public void Managed() => Fft.Calculate(Params.Log2FftSize, xyManaged, xyOutManaged);

    [Benchmark]
    public void Native() => FftNative.fft(Params.Log2FftSize, xyNative, xyOutNative);
}
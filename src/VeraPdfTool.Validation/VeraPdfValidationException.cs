namespace VeraPdfTool.Validation;

/// <summary>
/// Thrown when the underlying veraPDF (Java, via IKVM) validation engine
/// fails to open or validate a PDF document. Wraps the original Java
/// exception's message so callers never need to reference IKVM/Java types.
/// </summary>
public sealed class VeraPdfValidationException : Exception
{
    public VeraPdfValidationException(string message) : base(message)
    {
    }

    public VeraPdfValidationException(string message, Exception innerException)
        : base(message, innerException)
    {
    }
}

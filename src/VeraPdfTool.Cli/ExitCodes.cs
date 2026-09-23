namespace VeraPdfTool.Cli;

/// <summary>
/// Process exit codes. Designed so an agent/script can distinguish "the PDF has
/// compliance problems" (1) from "the tool itself failed to run" (2/3).
/// </summary>
internal static class ExitCodes
{
    /// <summary>All requested files validated and are compliant.</summary>
    public const int Success = 0;

    /// <summary>All files were validated successfully, but at least one is non-compliant.</summary>
    public const int NonCompliant = 1;

    /// <summary>At least one file could not be validated (missing, corrupt, encrypted, etc).</summary>
    public const int ValidationError = 2;

    /// <summary>Invalid command-line usage (bad arguments, unknown profile, no files given).</summary>
    public const int UsageError = 64; // matches sysexits.h EX_USAGE convention
}

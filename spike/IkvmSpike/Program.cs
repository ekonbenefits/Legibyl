using System;
using org.verapdf.gf.foundry;
using org.verapdf.pdfa;
using org.verapdf.pdfa.flavours;
using org.verapdf.pdfa.results;

if (args.Length < 1)
{
    Console.WriteLine("Usage: IkvmSpike <path-to-pdf>");
    return 1;
}

string pdfPath = args[0];

Console.WriteLine("Initialising veraPDF Greenfield foundry via IKVM...");
VeraGreenfieldFoundryProvider.initialise();

VeraPDFFoundry foundry = Foundries.defaultInstance();
Console.WriteLine($"Foundry parser id: {foundry.getParserId()}");

var javaFile = new java.io.File(pdfPath);

PDFAParser? parser = null;
try
{
    parser = foundry.createParser(javaFile, PDFAFlavour.PDFUA_1);
    PDFAValidator validator = foundry.createValidator(PDFAFlavour.PDFUA_1, false);
    ValidationResult result = validator.validate(parser);

    Console.WriteLine($"Flavour validated: {result.getPDFAFlavour()}");
    Console.WriteLine($"Compliant: {result.isCompliant()}");
    Console.WriteLine($"Total assertions: {result.getTotalAssertions()}");

    var assertions = result.getTestAssertions();
    int shown = 0;
    var it = assertions.iterator();
    while (it.hasNext() && shown < 10)
    {
        var assertion = (org.verapdf.pdfa.results.TestAssertion)it.next();
        if (assertion.getStatus() == org.verapdf.pdfa.results.TestAssertion.Status.FAILED)
        {
            Console.WriteLine($"  FAILED rule {assertion.getRuleId()}: {assertion.getMessage()}");
            shown++;
        }
    }
}
finally
{
    parser?.close();
}

return 0;

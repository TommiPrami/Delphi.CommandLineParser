program DelphiCommandlineParserUnitTests;

{$IFNDEF TESTINSIGHT}
  {$APPTYPE CONSOLE}
{$ENDIF}
{.$DEFINE CHECK_MEMORY_LEAKS}

{$STRONGLINKTYPES ON}

uses
  FastMM5,
  {$IFDEF CHECK_MEMORY_LEAKS}
  DUnitX.MemoryLeakMonitor.FastMM5,
  {$ENDIF }
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  {$ENDIF }
  DUnitX.TestFramework,
  Delphi.CommandLineParser in '..\Delphi.CommandLineParser.pas',
  Delphi.CommandLineParser.SimpleTests.DUnitX in 'Tests\Delphi.CommandLineParser.SimpleTests.DUnitX.pas',
  Delphi.CommandLineParser.SwitchSyntaxTests.DUnitX in 'Tests\Delphi.CommandLineParser.SwitchSyntaxTests.DUnitX.pas',
  Delphi.CommandLineParser.BooleanTests.DUnitX in 'Tests\Delphi.CommandLineParser.BooleanTests.DUnitX.pas',
  Delphi.CommandLineParser.IntegerTests.DUnitX in 'Tests\Delphi.CommandLineParser.IntegerTests.DUnitX.pas',
  Delphi.CommandLineParser.EnumTests.DUnitX in 'Tests\Delphi.CommandLineParser.EnumTests.DUnitX.pas',
  Delphi.CommandLineParser.StringArrayTests.DUnitX in 'Tests\Delphi.CommandLineParser.StringArrayTests.DUnitX.pas',
  Delphi.CommandLineParser.QuotingTests.DUnitX in 'Tests\Delphi.CommandLineParser.QuotingTests.DUnitX.pas',
  Delphi.CommandLineParser.PositionalTests.DUnitX in 'Tests\Delphi.CommandLineParser.PositionalTests.DUnitX.pas',
  Delphi.CommandLineParser.PositionRestTests.DUnitX in 'Tests\Delphi.CommandLineParser.PositionRestTests.DUnitX.pas',
  Delphi.CommandLineParser.RequiredAndDefaultsTests.DUnitX in 'Tests\Delphi.CommandLineParser.RequiredAndDefaultsTests.DUnitX.pas',
  Delphi.CommandLineParser.LongNameShortFormTests.DUnitX in 'Tests\Delphi.CommandLineParser.LongNameShortFormTests.DUnitX.pas',
  Delphi.CommandLineParser.ShortNameTests.DUnitX in 'Tests\Delphi.CommandLineParser.ShortNameTests.DUnitX.pas',
  Delphi.CommandLineParser.FileSystemTests.DUnitX in 'Tests\Delphi.CommandLineParser.FileSystemTests.DUnitX.pas',
  Delphi.CommandLineParser.ExtendableTests.DUnitX in 'Tests\Delphi.CommandLineParser.ExtendableTests.DUnitX.pas',
  Delphi.CommandLineParser.IgnoreUnknownTests.DUnitX in 'Tests\Delphi.CommandLineParser.IgnoreUnknownTests.DUnitX.pas',
  Delphi.CommandLineParser.ErrorInfoTests.DUnitX in 'Tests\Delphi.CommandLineParser.ErrorInfoTests.DUnitX.pas',
  Delphi.CommandLineParser.InheritanceTests.DUnitX in 'Tests\Delphi.CommandLineParser.InheritanceTests.DUnitX.pas',
  Delphi.CommandLineParser.ConfigErrorTests.DUnitX in 'Tests\Delphi.CommandLineParser.ConfigErrorTests.DUnitX.pas',
  Delphi.CommandLineParser.ParserLifecycleTests.DUnitX in 'Tests\Delphi.CommandLineParser.ParserLifecycleTests.DUnitX.pas',
  Delphi.CommandLineParser.FloatTests.DUnitX in 'Tests\Delphi.CommandLineParser.FloatTests.DUnitX.pas',
  Delphi.CommandLineParser.DateTimeTests.DUnitX in 'Tests\Delphi.CommandLineParser.DateTimeTests.DUnitX.pas',
  Delphi.CommandLineParser.RangeTests.DUnitX in 'Tests\Delphi.CommandLineParser.RangeTests.DUnitX.pas',
  Delphi.CommandLineParser.SkipTests.DUnitX in 'Tests\Delphi.CommandLineParser.SkipTests.DUnitX.pas',
  Delphi.CommandLineParser.HelpSwitchTests.DUnitX in 'Tests\Delphi.CommandLineParser.HelpSwitchTests.DUnitX.pas',
  Delphi.CommandLineParser.EnvironmentTests.DUnitX in 'Tests\Delphi.CommandLineParser.EnvironmentTests.DUnitX.pas',
  Delphi.CommandLineParser.IllegalEnumValueTests.DUnitX in 'Tests\Delphi.CommandLineParser.IllegalEnumValueTests.DUnitX.pas',
  Delphi.CommandLineParser.EmptyValueTests.DUnitX in 'Tests\Delphi.CommandLineParser.EmptyValueTests.DUnitX.pas',
  Delphi.CommandLineParser.ResponseFileTests.DUnitX in 'Tests\Delphi.CommandLineParser.ResponseFileTests.DUnitX.pas';

{ keep comment here to protect the following conditional from being removed by the IDE when adding a unit }
{$IFNDEF TESTINSIGHT}
var
  LRunner: ITestRunner;
  LResults: IRunResults;
  LLogger: ITestLogger;
  LNunitLogger: ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    LRunner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    LRunner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    LRunner.FailsOnNoAsserts := False;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      LLogger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      LRunner.AddLogger(LLogger);
    end;
    //Generate an NUnit compatible XML File
    LNunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    LRunner.AddLogger(LNunitLogger);

    //Run tests
    LResults := LRunner.Execute;
    if not LResults.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.

program MemorySafetyCheck;

{$APPTYPE CONSOLE}

{
  Standalone memory-safety regression guard for Delphi.CommandLineParser.

  WHY THIS IS A SEPARATE PROGRAM, NOT A DUnitX TEST
  -------------------------------------------------
  TSwitchData reaches the target object's properties through RTTI. The
  TRttiProperty handed out by GetRttiProperty is owned by Delphi's shared RTTI
  pool, which is freed the instant the last live TRttiContext is released. A
  regression that lets the property outlive its context is a use-after-free -
  but it only manifests when NO other TRttiContext is alive at that moment.

  DUnitX keeps a TRttiContext alive for the whole run (UseRTTI := True), which
  keeps the pool alive and masks the bug: the buggy parser passes the entire
  DUnitX suite even under FastMM5 debug mode. So this class of regression cannot
  be caught in-process by a normal unit test. This program deliberately links no
  framework that holds RTTI alive, runs the parser with no live context under
  FastMM5 debug mode (which scrubs freed memory), and faults deterministically
  if a property dangles.

  EXIT CODE: 0 = all checks passed, 1 = a failure/fault was detected.

  BUILD & RUN (Win32), from the UnitTests directory:
    "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
    dcc32 -B MemorySafetyCheck.dpr
    MemorySafetyCheck.exe
}

uses
  FastMM5 in 'ThirdPartyCode\FastMM5\FastMM5.pas',
  System.SysUtils,
  System.Classes,
  Delphi.CommandLineParser in '..\Delphi.CommandLineParser.pas';

type
  TColor = (clRed, clGreen, clBlue);

  // One property per supported switch type, each with a default, so a single
  // empty-command-line parse drives every branch of TSwitchData.SetValue.
  TAllTypes = class
  strict private
    FStr: string;
    FInt: Integer;
    FBig: Int64;
    FFloat: Double;
    FBool: Boolean;
    FColor: TColor;
  public
    [CLPLongName('Str'), CLPDescription('Str', '<s>'), CLPDefault('hello')]
    property Str: string read FStr write FStr;
    [CLPLongName('Int'), CLPDescription('Int', '<i>'), CLPDefault('42')]
    property Int: Integer read FInt write FInt;
    [CLPLongName('Big'), CLPDescription('Big', '<i>'), CLPDefault('9999999999')]
    property Big: Int64 read FBig write FBig;
    [CLPLongName('Float'), CLPDescription('Float', '<f>'), CLPDefault('3.14')]
    property Float: Double read FFloat write FFloat;
    [CLPLongName('Bool'), CLPDescription('Bool', '<b>'), CLPDefault('True')]
    property Bool: Boolean read FBool write FBool;
    [CLPLongName('Color'), CLPDescription('Color', '<c>'), CLPDefault('clGreen')]
    property Color: TColor read FColor write FColor;
  end;

var
  GFailures: Integer = 0;

procedure Check(const ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    Inc(GFailures);
    Writeln('  FAIL: ', AMessage);
  end;
end;

procedure RunOnce;
var
  LParser: ICommandLineParser;
  LOpts: TAllTypes;
  LUsage: TArray<string>;
begin
  // Nothing here holds a TRttiContext, so when a GetRttiProperty call inside the
  // parser returns, its local context was the only token on the pool.
  LParser := CreateCommandLineParser;
  LOpts := TAllTypes.Create;
  try
    Check(LParser.Parse('', LOpts), 'Parse('''') failed: ' + LParser.ErrorInfo.Text);

    Check(LOpts.Str = 'hello', 'Str default');
    Check(LOpts.Int = 42, 'Int default');
    Check(LOpts.Big = Int64(9999999999), 'Big default');
    Check(Abs(LOpts.Float - 3.14) < 0.0001, 'Float default');
    Check(LOpts.Bool, 'Bool default');
    Check(LOpts.Color = clGreen, 'Color default');

    // Usage() exercises the read side: GetValue -> GetRttiProperty.GetValue.
    LUsage := LParser.Usage;
    Check(Length(LUsage) > 0, 'Usage produced no output');
  finally
    LOpts.Free;
  end;
end;

begin
  if not FastMM_EnterDebugMode then
    Writeln('WARNING: FastMM debug mode not fully enabled; the check is weaker.');

  Writeln('Memory-safety check: parsing defaults across all types with no live RTTI context...');
  try
    // Repeat with heap churn between runs so a freed RTTI-pool block is reused /
    // poisoned before any dangling property is touched - making a regression
    // deterministic rather than "works on my machine".
    for var I := 1 to 200 do
    begin
      RunOnce;

      var LJunk := TStringList.Create;
      try
        for var J := 1 to 50 do
          LJunk.Add(StringOfChar('x', 64));
      finally
        LJunk.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      Inc(GFailures);
      Writeln('  FAULT: ', E.ClassName, ': ', E.Message);
    end;
  end;

  if GFailures = 0 then
  begin
    Writeln('PASS: no memory-safety problems detected.');
    ExitCode := 0;
  end
  else
  begin
    Writeln(Format('FAIL: %d problem(s) detected.', [GFailures]));
    ExitCode := 1;
  end;
end.

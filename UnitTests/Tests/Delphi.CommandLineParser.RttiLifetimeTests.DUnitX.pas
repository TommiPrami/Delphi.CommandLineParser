unit Delphi.CommandLineParser.RttiLifetimeTests.DUnitX;

{
  Regression tests for the RTTI-context lifetime in TSwitchData.

  TSwitchData reads and writes the target object's properties through RTTI
  (GetRttiProperty -> TRttiProperty.GetValue/SetValue). The TRttiProperty is
  owned by the shared RTTI pool, which Delphi frees as soon as the last live
  TRttiContext is released. If the context that produced the property does not
  outlive the property's use, the property dangles and using it is a
  use-after-free.

  Applying defaults parses an empty command line, which drives SetValue for
  EVERY switch type; Usage drives GetValue for every switch. These assert the
  correct behaviour across all types.

  IMPORTANT - what these tests can and cannot catch:
  They do NOT, on their own, catch the dangling-TRttiProperty lifetime bug.
  DUnitX discovers fixtures via RTTI (UseRTTI := True) and keeps a TRttiContext
  alive for the whole test run; that single live context keeps the shared RTTI
  pool alive, so GetRttiProperty's dying local context is never the *last* token
  and the pool is never freed. The use-after-free is therefore masked - even
  under FastMM5 debug mode the buggy parser passes every test. (The bug only
  bites when NO other context is alive, which is exactly when a host app AVs.)

  The deterministic guard for the lifetime regression is the standalone
  UnitTests\MemorySafetyCheck.dpr, which runs the parser with no live context
  under FastMM5 debug mode. Keep these tests as behavioural coverage of the
  default / Usage RTTI paths.
}

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  [TestFixture]
  TRttiLifetimeTests = class(TObject)
  public
    [Test] procedure AllTypeDefaultsAppliedViaRtti;
    [Test] procedure UsageReadsEveryPropertyViaRtti;
    [Test] procedure RepeatedParseDoesNotCorruptRttiPool;
  end;

implementation

uses
  System.SysUtils;

type
  TEnumKind = (ekAlpha, ekBeta, ekGamma);

  // One property per supported switch type, each with a default, so a single
  // empty-command-line parse exercises every branch of TSwitchData.SetValue.
  TAllTypes = class
  strict private
    FStr: string;
    FInt: Integer;
    FBig: Int64;
    FFloat: Double;
    FBool: Boolean;
    FEnum: TEnumKind;
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
    [CLPLongName('Enum'), CLPDescription('Enum', '<e>'), CLPDefault('ekBeta')]
    property Enum: TEnumKind read FEnum write FEnum;
  end;

procedure TRttiLifetimeTests.AllTypeDefaultsAppliedViaRtti;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TAllTypes.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);

    Assert.AreEqual('hello', LOpts.Str);
    Assert.AreEqual(42, LOpts.Int);
    Assert.AreEqual(Int64(9999999999), LOpts.Big);
    Assert.AreEqual(Double(3.14), LOpts.Float, 0.0001);
    Assert.IsTrue(LOpts.Bool);
    Assert.AreEqual(Integer(ekBeta), Integer(LOpts.Enum));
  finally
    LOpts.Free;
  end;
end;

procedure TRttiLifetimeTests.UsageReadsEveryPropertyViaRtti;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TAllTypes.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);

    // Usage() walks every switch and calls GetValue -> GetRttiProperty.GetValue,
    // exercising the read side of the same RTTI lookup.
    var LUsage := LParser.Usage;
    Assert.IsTrue(Length(LUsage) > 0, 'Usage produced no output');
  finally
    LOpts.Free;
  end;
end;

procedure TRttiLifetimeTests.RepeatedParseDoesNotCorruptRttiPool;
begin
  // Churn many parser/target pairs so that, if a switch ever returned a property
  // from an already-freed pool, the freed block is very likely reused/overwritten
  // before use - turning the latent corruption into a deterministic failure.
  for var I := 1 to 50 do
  begin
    var LParser := CreateCommandLineParser;
    var LOpts := TAllTypes.Create;
    try
      Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
      Assert.AreEqual('hello', LOpts.Str);
      Assert.AreEqual(Integer(ekBeta), Integer(LOpts.Enum));
    finally
      LOpts.Free;
    end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TRttiLifetimeTests);

end.

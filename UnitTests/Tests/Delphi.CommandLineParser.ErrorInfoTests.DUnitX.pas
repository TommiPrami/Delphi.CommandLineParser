unit Delphi.CommandLineParser.ErrorInfoTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TRequiredOpts = class
  strict private
    FName: string;
  public
    [CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPRequired]
    property Name: string read FName write FName;
  end;

  TIntOpts = class
  strict private
    FN: Integer;
  public
    [CLPLongName('N'), CLPDescription('N', '<int>'), CLPDefault('1')]
    property N: Integer read FN write FN;
  end;

  TBoolOpts = class
  strict private
    FFlag: Boolean;
  public
    [CLPLongName('Flag'), CLPDescription('Flag', '<bool>')]
    property Flag: Boolean read FFlag write FFlag;
  end;

  TPositionalOpts = class
  strict private
    FFirst: string;
  public
    [CLPPosition(1), CLPRequired, CLPDescription('First', '<string>')]
    property First: string read FFirst write FFirst;
  end;

  [TestFixture]
  TErrorInfoTests = class(TObject)
  public
    [Test] procedure SuccessfulParseHasNoError;
    [Test] procedure UnknownNamedSetsExpectedKind;
    [Test] procedure InvalidIntegerSetsInvalidDataKind;
    [Test] procedure BooleanWithBadValueSetsInvalidDataKind;
    [Test] procedure MissingNamedSetsExpectedKind;
    [Test] procedure MissingPositionalSetsExpectedKind;
    [Test] procedure ExtraPositionalSetsExpectedKind;
    [Test] procedure ErrorInfoTextIsPopulated;
    [Test] procedure ErrorInfoSwitchNameIsPopulated;
    [Test] procedure ParseClearsBetweenSuccessfulCalls;
  end;

implementation

uses
  System.SysUtils;

procedure TErrorInfoTests.SuccessfulParseHasNoError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOpts.Create;
  try
    LParser.Parse('-N:5', LOpts);
    Assert.IsFalse(LParser.ErrorInfo.IsError);
  finally
    LOpts.Free;
  end;
end;

procedure TErrorInfoTests.UnknownNamedSetsExpectedKind;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOpts.Create;
  try
    LParser.Parse('-Unknown:x', LOpts);
    Assert.AreEqual(Integer(ekUnknownNamed), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TErrorInfoTests.InvalidIntegerSetsInvalidDataKind;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOpts.Create;
  try
    LParser.Parse('-N:notanumber', LOpts);
    Assert.AreEqual(Integer(ekInvalidData), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TErrorInfoTests.BooleanWithBadValueSetsInvalidDataKind;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TBoolOpts.Create;
  try
    LParser.Parse('-Flag:nope', LOpts);
    Assert.AreEqual(Integer(ekInvalidData), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TErrorInfoTests.MissingNamedSetsExpectedKind;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRequiredOpts.Create;
  try
    LParser.Parse('', LOpts);
    Assert.AreEqual(Integer(ekMissingNamed), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TErrorInfoTests.MissingPositionalSetsExpectedKind;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TPositionalOpts.Create;
  try
    LParser.Parse('', LOpts);
    Assert.AreEqual(Integer(ekMissingPositional), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TErrorInfoTests.ExtraPositionalSetsExpectedKind;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TPositionalOpts.Create;
  try
    LParser.Parse('one extra', LOpts);
    Assert.AreEqual(Integer(ekExtraPositional), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TErrorInfoTests.ErrorInfoTextIsPopulated;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRequiredOpts.Create;
  try
    LParser.Parse('', LOpts);
    Assert.IsFalse(LParser.ErrorInfo.Text.IsEmpty);
  finally
    LOpts.Free;
  end;
end;

procedure TErrorInfoTests.ErrorInfoSwitchNameIsPopulated;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRequiredOpts.Create;
  try
    LParser.Parse('', LOpts);
    Assert.AreEqual('Name', LParser.ErrorInfo.SwitchName);
  finally
    LOpts.Free;
  end;
end;

procedure TErrorInfoTests.ParseClearsBetweenSuccessfulCalls;
begin
  // After a failed Parse, a subsequent successful Parse on the same instance
  // should still report success — but ErrorInfo from the prior failure may
  // linger. This pins down current behavior; revisit if reset is desired.
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOpts.Create;
  try
    LParser.Parse('-N:bad', LOpts);
    Assert.IsTrue(LParser.ErrorInfo.IsError);

    Assert.IsTrue(LParser.Parse('-N:5', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(5, LOpts.N);
  finally
    LOpts.Free;
  end;
end;

end.

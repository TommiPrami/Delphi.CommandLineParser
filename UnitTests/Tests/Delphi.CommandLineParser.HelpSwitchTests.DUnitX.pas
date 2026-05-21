unit Delphi.CommandLineParser.HelpSwitchTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TWithRequired = class
  strict private
    FName: string;
  public
    [CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPRequired]
    property Name: string read FName write FName;
  end;

  [TestFixture]
  THelpSwitchTests = class(TObject)
  public
    [Test] procedure DashHRecognizedWhenEnabled;
    [Test] procedure DashQuestionMarkRecognizedWhenEnabled;
    [Test] procedure DoubleDashHelpRecognizedWhenEnabled;
    [Test] procedure HelpSkipsRequiredValidation;
    [Test] procedure HelpNotRecognizedByDefault;
    [Test] procedure HelpRequestedFalseWhenAbsent;
    [Test] procedure HelpRequestedResetBetweenParseCalls;
  end;

implementation

procedure THelpSwitchTests.DashHRecognizedWhenEnabled;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithRequired.Create;
  try
    LParser.Options := [opEnableBuiltInHelp];
    Assert.IsTrue(LParser.Parse('-h', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LParser.HelpRequested);
  finally
    LOpts.Free;
  end;
end;

procedure THelpSwitchTests.DashQuestionMarkRecognizedWhenEnabled;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithRequired.Create;
  try
    LParser.Options := [opEnableBuiltInHelp];
    Assert.IsTrue(LParser.Parse('-?', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LParser.HelpRequested);
  finally
    LOpts.Free;
  end;
end;

procedure THelpSwitchTests.DoubleDashHelpRecognizedWhenEnabled;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithRequired.Create;
  try
    LParser.Options := [opEnableBuiltInHelp];
    Assert.IsTrue(LParser.Parse('--help', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LParser.HelpRequested);
  finally
    LOpts.Free;
  end;
end;

procedure THelpSwitchTests.HelpSkipsRequiredValidation;
begin
  // -Name is required, but --help short-circuits the required check.
  var LParser := CreateCommandLineParser;
  var LOpts := TWithRequired.Create;
  try
    LParser.Options := [opEnableBuiltInHelp];
    Assert.IsTrue(LParser.Parse('--help', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LParser.HelpRequested);
  finally
    LOpts.Free;
  end;
end;

procedure THelpSwitchTests.HelpNotRecognizedByDefault;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithRequired.Create;
  try
    // Default options ([]) - -h should be treated as an unknown switch.
    Assert.IsFalse(LParser.Parse('-h', LOpts));
    Assert.IsFalse(LParser.HelpRequested);
  finally
    LOpts.Free;
  end;
end;

procedure THelpSwitchTests.HelpRequestedFalseWhenAbsent;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithRequired.Create;
  try
    LParser.Options := [opEnableBuiltInHelp];
    LParser.Parse('-Name:foo', LOpts);
    Assert.IsFalse(LParser.HelpRequested);
  finally
    LOpts.Free;
  end;
end;

procedure THelpSwitchTests.HelpRequestedResetBetweenParseCalls;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithRequired.Create;
  try
    LParser.Options := [opEnableBuiltInHelp];
    LParser.Parse('--help', LOpts);
    Assert.IsTrue(LParser.HelpRequested);

    LParser.Parse('-Name:foo', LOpts);
    Assert.IsFalse(LParser.HelpRequested, 'HelpRequested should reset on next Parse');
  finally
    LOpts.Free;
  end;
end;

end.

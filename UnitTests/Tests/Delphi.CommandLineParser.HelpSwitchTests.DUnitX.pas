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

  // A definition class that deliberately owns a Boolean switch whose short name
  // is 'h'. This collides with the built-in help token '-h' when
  // opEnableBuiltInHelp is set. See HelpTokenTakesPrecedenceOverUserSwitchH.
  THasShortNameH = class
  strict private
    FHugeMode: Boolean;
  public
    [CLPName('h'), CLPLongName('HugeMode'), CLPDescription('Huge mode', '<bool>')]
    property HugeMode: Boolean read FHugeMode write FHugeMode;
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
    // #5: characterization test documenting the (deliberate) precedence rule.
    [Test] procedure HelpTokenTakesPrecedenceOverUserSwitchH;
    [Test] procedure UserSwitchHReachableWhenHelpDisabled;
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

procedure THelpSwitchTests.HelpTokenTakesPrecedenceOverUserSwitchH;
begin
  // DOCUMENTED BEHAVIOR (#5): when opEnableBuiltInHelp is enabled, the help
  // tokens (-h, -?, --help) are intercepted BEFORE normal switch lookup. So a
  // user-defined switch whose short name is 'h' becomes unreachable via '-h':
  // the token is treated as a help request instead. This is a deliberate
  // trade-off documented on opEnableBuiltInHelp; the test locks it so the
  // precedence can't change silently.
  var LParser := CreateCommandLineParser;
  var LOpts := THasShortNameH.Create;
  try
    LParser.Options := [opEnableBuiltInHelp];
    Assert.IsTrue(LParser.Parse('-h', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LParser.HelpRequested, 'help should win over the user switch named h');
    Assert.IsFalse(LOpts.HugeMode, 'the user switch must NOT have been toggled');
  finally
    LOpts.Free;
  end;
end;

procedure THelpSwitchTests.UserSwitchHReachableWhenHelpDisabled;
begin
  // The flip side: with help disabled (default), '-h' reaches the user switch
  // normally. This proves the collision only exists while help is enabled.
  var LParser := CreateCommandLineParser;
  var LOpts := THasShortNameH.Create;
  try
    Assert.IsTrue(LParser.Parse('-h', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.HugeMode, 'the user switch named h should toggle when help is off');
    Assert.IsFalse(LParser.HelpRequested);
  finally
    LOpts.Free;
  end;
end;

end.

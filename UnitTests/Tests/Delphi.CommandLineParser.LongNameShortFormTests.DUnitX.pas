unit Delphi.CommandLineParser.LongNameShortFormTests.DUnitX;

interface

uses
  System.SysUtils, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TShortFormOptions = class
  strict private
    FAutoTest: string;
  public
    [CLPLongName('autotest', 'auto'), CLPDescription('Autotest', '<string>'), CLPDefault('')]
    property AutoTest: string read FAutoTest write FAutoTest;
  end;

  TMultipleLongNames = class
  strict private
    FAlpha: string;
  public
    [CLPLongName('first'), CLPLongName('alternate'), CLPLongName('also'),
     CLPDescription('Alpha', '<string>'), CLPDefault('')]
    property Alpha: string read FAlpha write FAlpha;
  end;

  TBadShortForm = class
  strict private
    FAlpha: string;
  public
    // 'xyz' is not a prefix of 'autotest' — configuration error.
    [CLPLongName('autotest', 'xyz'), CLPDescription('Alpha', '<string>'), CLPDefault('')]
    property Alpha: string read FAlpha write FAlpha;
  end;

  TShortNameTooLong = class
  strict private
    FAlpha: string;
  public
    // Short name 'ab' is two letters — configuration error.
    [CLPName('ab'), CLPDescription('Alpha', '<string>'), CLPDefault('')]
    property Alpha: string read FAlpha write FAlpha;
  end;

  [TestFixture]
  TLongNameShortFormTests = class(TObject)
  public
    [Test] procedure FullLongFormAccepted;
    [Test] procedure ShortFormPrefixAccepted;
    [Test] procedure IntermediatePrefixAccepted;
    [Test] procedure PrefixShorterThanShortFormRejected;
    [Test] procedure AlternateLongName1Accepted;
    [Test] procedure AlternateLongName2Accepted;
    [Test] procedure AlternateLongName3Accepted;
    [Test] procedure MismatchedShortFormRaisesConfigError;
    [Test] procedure TwoLetterShortNameRaisesConfigError;
  end;

implementation

procedure TLongNameShortFormTests.FullLongFormAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TShortFormOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-autotest:value', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('value', LOpts.AutoTest);
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.ShortFormPrefixAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TShortFormOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-auto:value', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('value', LOpts.AutoTest);
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.IntermediatePrefixAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TShortFormOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-autot:value', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('value', LOpts.AutoTest);
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.PrefixShorterThanShortFormRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TShortFormOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-aut:value', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.AlternateLongName1Accepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TMultipleLongNames.Create;
  try
    Assert.IsTrue(LParser.Parse('-first:one', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('one', LOpts.Alpha);
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.AlternateLongName2Accepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TMultipleLongNames.Create;
  try
    Assert.IsTrue(LParser.Parse('-alternate:two', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('two', LOpts.Alpha);
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.AlternateLongName3Accepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TMultipleLongNames.Create;
  try
    Assert.IsTrue(LParser.Parse('-also:three', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('three', LOpts.Alpha);
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.MismatchedShortFormRaisesConfigError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TBadShortForm.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        LParser.Parse('', LOpts);
      end,
      ECLPConfigurationError);
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.TwoLetterShortNameRaisesConfigError;
var
  LRaised: Boolean;
  LRaisedClass: string;
  LRaisedMessage: string;
  LParseResult: Boolean;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TShortNameTooLong.Create;
  try
    LRaised := False;
    LRaisedClass := '';
    LRaisedMessage := '';
    LParseResult := False;
    try
      LParseResult := LParser.Parse('', LOpts);
    except
      on E: Exception do
      begin
        LRaised := True;
        LRaisedClass := E.ClassName;
        LRaisedMessage := E.Message;
      end;
    end;

    if not LRaised then
      Assert.Fail(Format(
        'Expected ECLPConfigurationError for two-letter short name, but Parse returned %s. ' +
        'ErrorInfo.IsError=%s, Kind=%d, Detailed=%d, SwitchName="%s", Text="%s"',
        [BoolToStr(LParseResult, True),
         BoolToStr(LParser.ErrorInfo.IsError, True),
         Integer(LParser.ErrorInfo.Kind),
         Integer(LParser.ErrorInfo.Detailed),
         LParser.ErrorInfo.SwitchName,
         LParser.ErrorInfo.Text]));

    Assert.AreEqual('ECLPConfigurationError', LRaisedClass,
      'Wrong exception class. Message: ' + LRaisedMessage);
  finally
    LOpts.Free;
  end;
end;

end.

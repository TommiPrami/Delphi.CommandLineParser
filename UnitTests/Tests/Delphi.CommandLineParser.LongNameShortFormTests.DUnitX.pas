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

  TTwoLetterShortName = class
  strict private
    FAlpha: string;
  public
    // Short name 'ab' is shorter than the long name 'alpha' — accepted.
    [CLPName('ab'), CLPLongName('alpha'), CLPDescription('Alpha', '<string>'), CLPDefault('')]
    property Alpha: string read FAlpha write FAlpha;
  end;

  TShortNameLongerThanLongName = class
  strict private
    FAlpha: string;
  public
    // Short name 'alphabet' is longer than the long name 'alpha' — configuration error.
    [CLPName('alphabet'), CLPLongName('alpha'), CLPDescription('Alpha', '<string>'), CLPDefault('')]
    property Alpha: string read FAlpha write FAlpha;
  end;

  TShortNameSameLengthAsLongName = class
  strict private
    FAlpha: string;
  public
    // 'abcde' is as long as 'alpha'; a short name must be strictly shorter — configuration error.
    [CLPName('abcde'), CLPLongName('alpha'), CLPDescription('Alpha', '<string>'), CLPDefault('')]
    property Alpha: string read FAlpha write FAlpha;
  end;

  TShortNameLongerThanPropertyName = class
  strict private
    FDays: Integer;
  public
    // No CLPLongName, so the property name 'Days' is the long name, and the
    // short name 'NumberOfDays' is longer than it — configuration error.
    [CLPName('NumberOfDays'), CLPDescription('Days', '<int>'), CLPDefault('0')]
    property Days: Integer read FDays write FDays;
  end;

  TShortNameLongerThanAlternateLongName = class
  strict private
    FUseService: Boolean;
  public
    // 'Serv' is shorter than 'UseService' but not shorter than the alternate
    // long name 'Svc'; the short name must be shorter than EVERY long name.
    [CLPName('Serv'), CLPLongName('UseService'), CLPLongName('Svc'), CLPDescription('Service', '<bool>')]
    property UseService: Boolean read FUseService write FUseService;
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
    // CLPName length rule: shorter than every long name of the switch.
    [Test] procedure TwoLetterShortNameAccepted;
    [Test] procedure ShortNameLongerThanLongNameRaisesConfigError;
    [Test] procedure ShortNameSameLengthAsLongNameRaisesConfigError;
    [Test] procedure ShortNameLongerThanPropertyNameRaisesConfigError;
    [Test] procedure ShortNameLongerThanAlternateLongNameRaisesConfigError;
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

// Parses an empty command line against AOptions and checks that the definition
// is rejected with ekShortNameTooLong / edShortNameTooLong, that the offending
// short name is reported as the switch name and that the error text names the
// long name the short name collides with.
procedure AssertShortNameTooLong(const AOptions: TObject; const AShortName, ALongName: string);
begin
  var LParser := CreateCommandLineParser;
  try
    LParser.Parse('', AOptions);
    Assert.Fail('Expected ECLPConfigurationError, but Parse did not raise. ErrorInfo.Text="' + LParser.ErrorInfo.Text + '"');
  except
    on E: ECLPConfigurationError do
    begin
      Assert.IsTrue(E.ErrorInfo.Kind = ekShortNameTooLong, 'ErrorInfo.Kind. Message: ' + E.Message);
      Assert.IsTrue(E.ErrorInfo.Detailed = edShortNameTooLong, 'ErrorInfo.Detailed. Message: ' + E.Message);
      Assert.AreEqual(AShortName, E.ErrorInfo.SwitchName, 'ErrorInfo.SwitchName');
      Assert.IsTrue(Pos('"' + ALongName + '"', E.ErrorInfo.Text) > 0, 'ErrorInfo.Text should name the long name. Message: ' + E.Message);
    end;
  end;
end;

procedure TLongNameShortFormTests.TwoLetterShortNameAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TTwoLetterShortName.Create;
  try
    Assert.IsTrue(LParser.Parse('-ab:value', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('value', LOpts.Alpha);
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.ShortNameLongerThanLongNameRaisesConfigError;
begin
  var LOpts := TShortNameLongerThanLongName.Create;
  try
    AssertShortNameTooLong(LOpts, 'alphabet', 'alpha');
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.ShortNameSameLengthAsLongNameRaisesConfigError;
begin
  var LOpts := TShortNameSameLengthAsLongName.Create;
  try
    AssertShortNameTooLong(LOpts, 'abcde', 'alpha');
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.ShortNameLongerThanPropertyNameRaisesConfigError;
begin
  var LOpts := TShortNameLongerThanPropertyName.Create;
  try
    AssertShortNameTooLong(LOpts, 'NumberOfDays', 'Days');
  finally
    LOpts.Free;
  end;
end;

procedure TLongNameShortFormTests.ShortNameLongerThanAlternateLongNameRaisesConfigError;
begin
  var LOpts := TShortNameLongerThanAlternateLongName.Create;
  try
    AssertShortNameTooLong(LOpts, 'Serv', 'Svc');
  finally
    LOpts.Free;
  end;
end;

end.

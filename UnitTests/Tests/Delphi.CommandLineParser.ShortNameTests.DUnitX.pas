unit Delphi.CommandLineParser.ShortNameTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TShortNameOptions = class
  strict private
    FVerbose: Boolean;
    FCount: Integer;
    FName: string;
  public
    [CLPName('v'), CLPLongName('Verbose'), CLPDescription('Verbose flag', '<bool>')]
    property Verbose: Boolean read FVerbose write FVerbose;
    [CLPName('c'), CLPLongName('Count'), CLPDescription('Count', '<int>'), CLPDefault('0')]
    property Count: Integer read FCount write FCount;
    [CLPName('n'), CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPDefault('')]
    property Name: string read FName write FName;
  end;

  TMultiCharShortNameOptions = class
  strict private
    FRestore: Boolean;
    FServer: string;
    FSource: string;
    FUseService: Boolean;
  public
    [CLPName('R'), CLPLongName('Restore'), CLPDescription('Restore flag', '<bool>')]
    property Restore: Boolean read FRestore write FRestore;
    [CLPName('Srv'), CLPLongName('Server'), CLPDescription('Server', '<host>'), CLPDefault('')]
    property Server: string read FServer write FServer;
    [CLPName('S'), CLPLongName('Source'), CLPDescription('Source', '<path>'), CLPDefault('')]
    property Source: string read FSource write FSource;
    [CLPName('Se'), CLPLongName('UseService'), CLPDescription('Use service flag', '<bool>')]
    property UseService: Boolean read FUseService write FUseService;
  end;

  // No CLPLongName and the short name equals the property name, so the switch
  // has no long name at all: the short name may then be of any length.
  TShortNameOnlyOptions = class
  strict private
    FVerbose: Boolean;
  public
    [CLPName('Verbose'), CLPDescription('Verbose flag', '<bool>')]
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

  [TestFixture]
  TShortNameTests = class(TObject)
  public
    [Test] procedure BooleanShortNameAsFlag;
    [Test] procedure IntegerShortNameWithColon;
    [Test] procedure StringShortNameWithColon;
    [Test] procedure StringShortNameAttachedValue;
    [Test] procedure MultipleShortNamesOnOneLine;
    [Test] procedure ShortAndLongNameInterchangeable;
    // Short names longer than one character.
    [Test] procedure MultiCharShortNameAsFlag;
    [Test] procedure MultiCharShortNameWithColon;
    [Test] procedure MultiCharShortNameAttachedValue;
    [Test] procedure MultiCharShortNameIsCaseInsensitive;
    [Test] procedure LongestMatchingShortNameWins;
    [Test] procedure MultiCharShortAndLongNameInterchangeable;
    [Test] procedure ShortNameOnlySwitchMayBeAnyLength;
  end;

implementation

procedure TShortNameTests.BooleanShortNameAsFlag;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-v', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.Verbose);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.IntegerShortNameWithColon;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-c:25', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(25, LOpts.Count);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.StringShortNameWithColon;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-n:hello', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('hello', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.StringShortNameAttachedValue;
begin
  // Old DOS-style attached value: -nValue
  var LParser := CreateCommandLineParser;
  var LOpts := TShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-nattached', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('attached', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.MultipleShortNamesOnOneLine;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-v -c:3 -n:foo', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.Verbose);
    Assert.AreEqual(3, LOpts.Count);
    Assert.AreEqual('foo', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.ShortAndLongNameInterchangeable;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Verbose -c:7 -Name:long', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.Verbose);
    Assert.AreEqual(7, LOpts.Count);
    Assert.AreEqual('long', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.MultiCharShortNameAsFlag;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TMultiCharShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Se', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.UseService);
    Assert.IsFalse(LOpts.Restore);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.MultiCharShortNameWithColon;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TMultiCharShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Srv:host1', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('host1', LOpts.Server);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.MultiCharShortNameAttachedValue;
begin
  // Old DOS-style attached value works for longer short names too: -SrvValue
  var LParser := CreateCommandLineParser;
  var LOpts := TMultiCharShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Srvhost1', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('host1', LOpts.Server);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.MultiCharShortNameIsCaseInsensitive;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TMultiCharShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-SRV:host1 -se', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('host1', LOpts.Server);
    Assert.IsTrue(LOpts.UseService);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.LongestMatchingShortNameWins;
begin
  // '-Srvhost' starts with both short names 'S' and 'Srv'. The longest match
  // wins, so the result is Server='host' and not Source='rvhost'.
  var LParser := CreateCommandLineParser;
  var LOpts := TMultiCharShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Srvhost', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('host', LOpts.Server);
    Assert.AreEqual('', LOpts.Source);

    // Only 'S' matches here.
    Assert.IsTrue(LParser.Parse('-Sdata', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('data', LOpts.Source);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.MultiCharShortAndLongNameInterchangeable;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TMultiCharShortNameOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-UseService -Server:h1 -R', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.UseService);
    Assert.AreEqual('h1', LOpts.Server);
    Assert.IsTrue(LOpts.Restore);
  finally
    LOpts.Free;
  end;
end;

procedure TShortNameTests.ShortNameOnlySwitchMayBeAnyLength;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TShortNameOnlyOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Verbose', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.Verbose);
  finally
    LOpts.Free;
  end;
end;

end.

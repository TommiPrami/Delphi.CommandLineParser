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

  [TestFixture]
  TShortNameTests = class(TObject)
  public
    [Test] procedure BooleanShortNameAsFlag;
    [Test] procedure IntegerShortNameWithColon;
    [Test] procedure StringShortNameWithColon;
    [Test] procedure StringShortNameAttachedValue;
    [Test] procedure MultipleShortNamesOnOneLine;
    [Test] procedure ShortAndLongNameInterchangeable;
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

end.

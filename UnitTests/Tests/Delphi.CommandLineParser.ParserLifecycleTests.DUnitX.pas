unit Delphi.CommandLineParser.ParserLifecycleTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TLifecycleOpts = class
  strict private
    FName: string;
  public
    [CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPDefault('')]
    property Name: string read FName write FName;
  end;

  [TestFixture]
  TParserLifecycleTests = class(TObject)
  public
    [Test] procedure CreateParserReturnsInterface;
    [Test] procedure GlobalParserIsSingleton;
    [Test] procedure CreateParserReturnsNewInstances;
    [Test] procedure ParserCanBeReusedAcrossMultipleParseCalls;
    [Test] procedure ParserAllowsDifferentDefinitionClassesPerCall;
  end;

implementation

uses
  System.SysUtils;

type
  TOtherOpts = class
  strict private
    FN: Integer;
  public
    [CLPLongName('N'), CLPDescription('N', '<int>'), CLPDefault('0')]
    property N: Integer read FN write FN;
  end;

procedure TParserLifecycleTests.CreateParserReturnsInterface;
begin
  var LParser := CreateCommandLineParser;
  Assert.IsNotNull(LParser);
end;

procedure TParserLifecycleTests.GlobalParserIsSingleton;
begin
  var LFirst := CommandLineParser;
  LFirst.Options := [opIgnoreUnknownSwitches];
  try
    var LSecond := CommandLineParser;
    Assert.IsTrue(opIgnoreUnknownSwitches in LSecond.Options,
      'Second call to CommandLineParser returned a different instance');
  finally
    LFirst.Options := [];
  end;
end;

procedure TParserLifecycleTests.CreateParserReturnsNewInstances;
begin
  var LFirst := CreateCommandLineParser;
  LFirst.Options := [opIgnoreUnknownSwitches];

  var LSecond := CreateCommandLineParser;
  Assert.IsFalse(opIgnoreUnknownSwitches in LSecond.Options,
    'CreateCommandLineParser should return a fresh, independent instance');
end;

procedure TParserLifecycleTests.ParserCanBeReusedAcrossMultipleParseCalls;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TLifecycleOpts.Create;
  try
    Assert.IsTrue(LParser.Parse('-Name:one', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('one', LOpts.Name);

    Assert.IsTrue(LParser.Parse('-Name:two', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('two', LOpts.Name);

    Assert.IsTrue(LParser.Parse('-Name:three', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('three', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TParserLifecycleTests.ParserAllowsDifferentDefinitionClassesPerCall;
begin
  var LParser := CreateCommandLineParser;
  var LA := TLifecycleOpts.Create;
  var LB := TOtherOpts.Create;
  try
    Assert.IsTrue(LParser.Parse('-Name:hello', LA), LParser.ErrorInfo.Text);
    Assert.AreEqual('hello', LA.Name);

    Assert.IsTrue(LParser.Parse('-N:99', LB), LParser.ErrorInfo.Text);
    Assert.AreEqual(99, LB.N);
  finally
    LA.Free;
    LB.Free;
  end;
end;

end.

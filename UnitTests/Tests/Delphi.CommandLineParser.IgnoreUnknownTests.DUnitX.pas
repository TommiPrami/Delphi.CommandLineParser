unit Delphi.CommandLineParser.IgnoreUnknownTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TSimpleOpts = class
  strict private
    FName: string;
  public
    [CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPDefault('')]
    property Name: string read FName write FName;
  end;

  [TestFixture]
  TIgnoreUnknownTests = class(TObject)
  public
    [Test] procedure UnknownSwitchFailsByDefault;
    [Test] procedure IgnoreUnknownOptionSwallowsUnknown;
    [Test] procedure KnownSwitchStillProcessedWhenIgnoring;
    [Test] procedure DefaultOptionsAreEmptySet;
    [Test] procedure OptionsRoundTrip;
  end;

implementation

uses
  System.SysUtils;

procedure TIgnoreUnknownTests.UnknownSwitchFailsByDefault;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSimpleOpts.Create;
  try
    Assert.IsFalse(LParser.Parse('-Mystery:value', LOpts));
    Assert.AreEqual(Integer(ekUnknownNamed), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TIgnoreUnknownTests.IgnoreUnknownOptionSwallowsUnknown;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSimpleOpts.Create;
  try
    LParser.Options := [opIgnoreUnknownSwitches];
    Assert.IsTrue(LParser.Parse('-Mystery:value', LOpts), LParser.ErrorInfo.Text);
  finally
    LOpts.Free;
  end;
end;

procedure TIgnoreUnknownTests.KnownSwitchStillProcessedWhenIgnoring;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSimpleOpts.Create;
  try
    LParser.Options := [opIgnoreUnknownSwitches];
    Assert.IsTrue(LParser.Parse('-Mystery:x -Name:keep', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('keep', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TIgnoreUnknownTests.DefaultOptionsAreEmptySet;
begin
  var LParser := CreateCommandLineParser;
  Assert.IsTrue(LParser.Options = []);
end;

procedure TIgnoreUnknownTests.OptionsRoundTrip;
begin
  var LParser := CreateCommandLineParser;
  LParser.Options := [opIgnoreUnknownSwitches];
  Assert.IsTrue(opIgnoreUnknownSwitches in LParser.Options);

  LParser.Options := [];
  Assert.IsFalse(opIgnoreUnknownSwitches in LParser.Options);
end;

end.

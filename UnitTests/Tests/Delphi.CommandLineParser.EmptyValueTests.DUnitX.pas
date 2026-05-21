unit Delphi.CommandLineParser.EmptyValueTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TEmptyOpts = class
  strict private
    FAlpha: string;
    FCount: Integer;
  public
    [CLPLongName('Alpha'), CLPDescription('Alpha', '<string>'), CLPDefault('preset')]
    property Alpha: string read FAlpha write FAlpha;
    [CLPLongName('Count'), CLPDescription('Count', '<int>'), CLPDefault('5')]
    property Count: Integer read FCount write FCount;
  end;

  [TestFixture]
  TEmptyValueTests = class(TObject)
  public
    [Test] procedure ExplicitEmptyColonSetsStringToEmpty;
    [Test] procedure ExplicitEmptyEqualsSetsStringToEmpty;
    [Test] procedure SwitchWithoutDelimiterDoesNotOverrideDefault;
    [Test] procedure ExplicitEmptyOnIntegerFails;
  end;

implementation

uses
  System.SysUtils;

procedure TEmptyValueTests.ExplicitEmptyColonSetsStringToEmpty;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TEmptyOpts.Create;
  try
    Assert.IsTrue(LParser.Parse('-Alpha:""', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('', LOpts.Alpha);
  finally
    LOpts.Free;
  end;
end;

procedure TEmptyValueTests.ExplicitEmptyEqualsSetsStringToEmpty;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TEmptyOpts.Create;
  try
    Assert.IsTrue(LParser.Parse('-Alpha=""', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('', LOpts.Alpha);
  finally
    LOpts.Free;
  end;
end;

procedure TEmptyValueTests.SwitchWithoutDelimiterDoesNotOverrideDefault;
begin
  // No ':' or '=' means no explicit value supplied. Default stays in effect.
  var LParser := CreateCommandLineParser;
  var LOpts := TEmptyOpts.Create;
  try
    Assert.IsTrue(LParser.Parse('-Alpha', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('preset', LOpts.Alpha);
  finally
    LOpts.Free;
  end;
end;

procedure TEmptyValueTests.ExplicitEmptyOnIntegerFails;
begin
  // '' is not a valid integer; the explicit ':' should still trigger SetValue
  // which fails the parse.
  var LParser := CreateCommandLineParser;
  var LOpts := TEmptyOpts.Create;
  try
    Assert.IsFalse(LParser.Parse('-Count:""', LOpts));
  finally
    LOpts.Free;
  end;
end;

end.

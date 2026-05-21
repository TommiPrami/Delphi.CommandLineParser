unit Delphi.CommandLineParser.RequiredAndDefaultsTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  [TestFixture]
  TRequiredAndDefaultsTests = class(TObject)
  public
    [Test] procedure StringDefaultApplied;
    [Test] procedure IntegerDefaultApplied;
    [Test] procedure ProvidedValueOverridesDefault;
    [Test] procedure RequiredMissingFails;
    [Test] procedure RequiredProvidedSucceeds;
    [Test] procedure RequiredErrorKindIsMissingNamed;
    [Test] procedure RequiredWithDefaultAcceptedWhenAbsent;
  end;

implementation

uses
  System.SysUtils;

type
  TWithDefaults = class
  strict private
    FName: string;
    FNumber: Integer;
  public
    [CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPDefault('defaultName')]
    property Name: string read FName write FName;
    [CLPLongName('Number'), CLPDescription('Number', '<int>'), CLPDefault('77')]
    property Number: Integer read FNumber write FNumber;
  end;

  TWithRequired = class
  strict private
    FName: string;
  public
    [CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPRequired]
    property Name: string read FName write FName;
  end;

  TRequiredWithDefault = class
  strict private
    FName: string;
  public
    // Required AND default: default is applied on absence, which counts as provided.
    [CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPRequired, CLPDefault('autoFilled')]
    property Name: string read FName write FName;
  end;

procedure TRequiredAndDefaultsTests.StringDefaultApplied;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithDefaults.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('defaultName', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TRequiredAndDefaultsTests.IntegerDefaultApplied;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithDefaults.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(77, LOpts.Number);
  finally
    LOpts.Free;
  end;
end;

procedure TRequiredAndDefaultsTests.ProvidedValueOverridesDefault;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithDefaults.Create;
  try
    Assert.IsTrue(LParser.Parse('-Name:custom -Number:9', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('custom', LOpts.Name);
    Assert.AreEqual(9, LOpts.Number);
  finally
    LOpts.Free;
  end;
end;

procedure TRequiredAndDefaultsTests.RequiredMissingFails;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithRequired.Create;
  try
    Assert.IsFalse(LParser.Parse('', LOpts));
    Assert.IsTrue(LParser.ErrorInfo.IsError);
  finally
    LOpts.Free;
  end;
end;

procedure TRequiredAndDefaultsTests.RequiredProvidedSucceeds;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithRequired.Create;
  try
    Assert.IsTrue(LParser.Parse('-Name:there', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('there', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TRequiredAndDefaultsTests.RequiredErrorKindIsMissingNamed;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithRequired.Create;
  try
    LParser.Parse('', LOpts);
    Assert.AreEqual(Integer(ekMissingNamed), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TRequiredAndDefaultsTests.RequiredWithDefaultAcceptedWhenAbsent;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRequiredWithDefault.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('autoFilled', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

end.

unit Delphi.CommandLineParser.EnvironmentTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TEnvOptions = class
  strict private
    FFromEnv: string;
    FFromEnvWithDefault: string;
  public
    [CLPLongName('FromEnv'), CLPEnvironment('CLP_TEST_FROMENV'),
     CLPDescription('From env', '<string>'), CLPDefault('')]
    property FromEnv: string read FFromEnv write FFromEnv;

    [CLPLongName('FromEnvWithDefault'), CLPEnvironment('CLP_TEST_FROMENV_D'),
     CLPDescription('From env or default', '<string>'), CLPDefault('default-value')]
    property FromEnvWithDefault: string read FFromEnvWithDefault write FFromEnvWithDefault;
  end;

  [TestFixture]
  TEnvironmentTests = class(TObject)
  private
    procedure SetEnvVar(const AName, AValue: string);
    procedure UnsetEnvVar(const AName: string);
  public
    [Test] procedure EnvVarUsedWhenSwitchAbsent;
    [Test] procedure CommandLineOverridesEnvVar;
    [Test] procedure EnvVarOverridesDefault;
    [Test] procedure DefaultUsedWhenEnvVarUnset;
    [Test] procedure UnsetEnvVarIsIgnored;
  end;

implementation

uses
  System.SysUtils, Winapi.Windows;

procedure TEnvironmentTests.SetEnvVar(const AName, AValue: string);
begin
  {$IFDEF MSWINDOWS}
  SetEnvironmentVariable(PChar(AName), PChar(AValue));
  {$ENDIF}
end;

procedure TEnvironmentTests.UnsetEnvVar(const AName: string);
begin
  {$IFDEF MSWINDOWS}
  SetEnvironmentVariable(PChar(AName), nil);
  {$ENDIF}
end;

procedure TEnvironmentTests.EnvVarUsedWhenSwitchAbsent;
begin
  SetEnvVar('CLP_TEST_FROMENV', 'env-was-here');
  try
    var LParser := CreateCommandLineParser;
    var LOpts := TEnvOptions.Create;
    try
      Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
      Assert.AreEqual('env-was-here', LOpts.FromEnv);
    finally
      LOpts.Free;
    end;
  finally
    UnsetEnvVar('CLP_TEST_FROMENV');
  end;
end;

procedure TEnvironmentTests.CommandLineOverridesEnvVar;
begin
  SetEnvVar('CLP_TEST_FROMENV', 'env-was-here');
  try
    var LParser := CreateCommandLineParser;
    var LOpts := TEnvOptions.Create;
    try
      Assert.IsTrue(LParser.Parse('-FromEnv:from-cli', LOpts), LParser.ErrorInfo.Text);
      Assert.AreEqual('from-cli', LOpts.FromEnv);
    finally
      LOpts.Free;
    end;
  finally
    UnsetEnvVar('CLP_TEST_FROMENV');
  end;
end;

procedure TEnvironmentTests.EnvVarOverridesDefault;
begin
  SetEnvVar('CLP_TEST_FROMENV_D', 'env-wins');
  try
    var LParser := CreateCommandLineParser;
    var LOpts := TEnvOptions.Create;
    try
      Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
      Assert.AreEqual('env-wins', LOpts.FromEnvWithDefault);
    finally
      LOpts.Free;
    end;
  finally
    UnsetEnvVar('CLP_TEST_FROMENV_D');
  end;
end;

procedure TEnvironmentTests.DefaultUsedWhenEnvVarUnset;
begin
  UnsetEnvVar('CLP_TEST_FROMENV_D');
  var LParser := CreateCommandLineParser;
  var LOpts := TEnvOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('default-value', LOpts.FromEnvWithDefault);
  finally
    LOpts.Free;
  end;
end;

procedure TEnvironmentTests.UnsetEnvVarIsIgnored;
begin
  UnsetEnvVar('CLP_TEST_FROMENV');
  var LParser := CreateCommandLineParser;
  var LOpts := TEnvOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('', LOpts.FromEnv);
  finally
    LOpts.Free;
  end;
end;

end.

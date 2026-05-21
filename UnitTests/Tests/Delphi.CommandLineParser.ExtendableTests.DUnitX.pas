unit Delphi.CommandLineParser.ExtendableTests.DUnitX;

interface

uses
  System.SysUtils, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TExtendableOptions = class
  strict private
    FFlag: Boolean;
    FNormal: string;
  public
    [CLPLongName('Define'), CLPExtendable, CLPDescription('Define flag', '<bool>')]
    property Flag: Boolean read FFlag write FFlag;

    // A non-extendable switch whose name happens to share a prefix.
    [CLPLongName('DefineX'), CLPDescription('Other', '<string>'), CLPDefault('')]
    property Normal: string read FNormal write FNormal;
  end;

  TStandaloneExtendable = class
  strict private
    FFlag: Boolean;
  public
    [CLPLongName('Define'), CLPExtendable, CLPDescription('Define flag', '<bool>')]
    property Flag: Boolean read FFlag write FFlag;
  end;

  TNonExtendableOnly = class
  strict private
    FVerbose: Boolean;
  public
    [CLPLongName('Verbose'), CLPDescription('Verbose flag', '<bool>')]
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

  [TestFixture]
  TExtendableTests = class(TObject)
  public
    [Test] procedure ExactNameStillWorks;
    [Test] procedure ExtensionAccessibleViaGetExtension;
    [Test] procedure ExtensionWithValueWorks;
    [Test] procedure NonExtendableSwitchNotMatchedByPrefix;
    [Test] procedure GetExtensionOnUndefinedSwitchRaises;
    [Test] procedure GetExtensionOnNonExtendableSwitchRaises;
  end;

implementation

procedure TExtendableTests.ExactNameStillWorks;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TStandaloneExtendable.Create;
  try
    Assert.IsTrue(LParser.Parse('-Define', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

procedure TExtendableTests.ExtensionAccessibleViaGetExtension;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TStandaloneExtendable.Create;
  try
    Assert.IsTrue(LParser.Parse('-DefineFOO', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.Flag);
    Assert.AreEqual('FOO', LParser.GetExtension('Define'));
  finally
    LOpts.Free;
  end;
end;

procedure TExtendableTests.ExtensionWithValueWorks;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TStandaloneExtendable.Create;
  try
    Assert.IsTrue(LParser.Parse('-DefineBAR:True', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

procedure TExtendableTests.NonExtendableSwitchNotMatchedByPrefix;
begin
  // 'Verbose' is NOT extendable. '-VerboseXYZ' must therefore be reported
  // as an unknown switch — the prefix-match path must skip non-extendable
  // switches.
  var LParser := CreateCommandLineParser;
  var LOpts := TNonExtendableOnly.Create;
  try
    Assert.IsFalse(LParser.Parse('-VerboseXYZ', LOpts));
    Assert.IsTrue(LParser.ErrorInfo.IsError);
  finally
    LOpts.Free;
  end;
end;

procedure TExtendableTests.GetExtensionOnUndefinedSwitchRaises;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TStandaloneExtendable.Create;
  try
    LParser.Parse('-Define', LOpts);
    Assert.WillRaise(
      procedure
      begin
        LParser.GetExtension('NoSuchSwitch');
      end,
      Exception);
  finally
    LOpts.Free;
  end;
end;

procedure TExtendableTests.GetExtensionOnNonExtendableSwitchRaises;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TExtendableOptions.Create;
  try
    LParser.Parse('-DefineX:foo', LOpts);
    Assert.WillRaise(
      procedure
      begin
        LParser.GetExtension('DefineX');
      end,
      Exception);
  finally
    LOpts.Free;
  end;
end;

end.

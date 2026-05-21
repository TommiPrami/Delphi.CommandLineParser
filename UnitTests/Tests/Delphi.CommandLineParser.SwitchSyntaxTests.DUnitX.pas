unit Delphi.CommandLineParser.SwitchSyntaxTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  [TestFixture]
  TSwitchSyntaxTests = class(TObject)
  public
    [Test] procedure LongFormWithDoubleDash;
    [Test] procedure LongFormWithSingleDash;
    {$IFDEF MSWINDOWS}
    [Test] procedure LongFormWithSlash;
    {$ENDIF}
    [Test] procedure ColonSeparator;
    [Test] procedure EqualsSeparator;
    [Test] procedure ColonChosenWhenBothPresent;
    [Test] procedure MultipleSwitchesOnOneLine;
    [Test] procedure LeadingAndTrailingWhitespaceTolerated;
    [Test] procedure UnknownSwitchFails;
    [Test] procedure SwitchNamesAreCaseInsensitive;
  end;

implementation

uses
  System.SysUtils;

type
  TSyntaxOptions = class
  strict private
    FName: string;
    FValue: Integer;
  public
    [CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPDefault('')]
    property Name: string read FName write FName;
    [CLPLongName('Value'), CLPDescription('Value', '<int>'), CLPDefault('0')]
    property Value: Integer read FValue write FValue;
  end;

procedure TSwitchSyntaxTests.LongFormWithDoubleDash;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSyntaxOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('--Name:hello', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('hello', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TSwitchSyntaxTests.LongFormWithSingleDash;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSyntaxOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Name:hello', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('hello', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TSwitchSyntaxTests.LongFormWithSlash;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSyntaxOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('/Name:hello', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('hello', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;
{$ENDIF}

procedure TSwitchSyntaxTests.ColonSeparator;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSyntaxOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Value:42', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(42, LOpts.Value);
  finally
    LOpts.Free;
  end;
end;

procedure TSwitchSyntaxTests.EqualsSeparator;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSyntaxOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Value=42', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(42, LOpts.Value);
  finally
    LOpts.Free;
  end;
end;

procedure TSwitchSyntaxTests.ColonChosenWhenBothPresent;
begin
  // -Name:value=trailing  --> value should be "value=trailing" (first ':' wins)
  var LParser := CreateCommandLineParser;
  var LOpts := TSyntaxOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Name:abc=def', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('abc=def', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TSwitchSyntaxTests.MultipleSwitchesOnOneLine;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSyntaxOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Name:foo -Value:7', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('foo', LOpts.Name);
    Assert.AreEqual(7, LOpts.Value);
  finally
    LOpts.Free;
  end;
end;

procedure TSwitchSyntaxTests.LeadingAndTrailingWhitespaceTolerated;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSyntaxOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('   -Name:foo    -Value:9   ', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('foo', LOpts.Name);
    Assert.AreEqual(9, LOpts.Value);
  finally
    LOpts.Free;
  end;
end;

procedure TSwitchSyntaxTests.UnknownSwitchFails;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSyntaxOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-DoesNotExist:x', LOpts));
    Assert.IsTrue(LParser.ErrorInfo.IsError);
  finally
    LOpts.Free;
  end;
end;

procedure TSwitchSyntaxTests.SwitchNamesAreCaseInsensitive;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSyntaxOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-NAME:up -value:11', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('up', LOpts.Name);
    Assert.AreEqual(11, LOpts.Value);
  finally
    LOpts.Free;
  end;
end;

end.

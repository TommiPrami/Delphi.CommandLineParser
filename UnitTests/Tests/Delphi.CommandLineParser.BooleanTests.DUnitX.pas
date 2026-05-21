unit Delphi.CommandLineParser.BooleanTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  [TestFixture]
  TBooleanTests = class(TObject)
  public
    [Test] procedure FlagWithoutValueSetsTrue;
    [Test] procedure ExplicitTrue;
    [Test] procedure ExplicitFalse;
    [Test] procedure ExplicitTrueLowercase;
    [Test] procedure ExplicitFalseLowercase;
    [Test] procedure InvalidValueReturnsFalseAndSetsError;
    [Test] procedure AbsentBooleanRemainsFalse;
    [Test] procedure TwoBooleanFlagsBothSetTrue;
    [Test] procedure BooleanCanFollowOtherSwitch;
  end;

implementation

uses
  System.SysUtils;

type
  TBoolOptions = class
  strict private
    FFlag: Boolean;
    FOther: Boolean;
    FAlpha: string;
  public
    [CLPLongName('Flag'), CLPDescription('A flag', '<bool>')]
    property Flag: Boolean read FFlag write FFlag;
    [CLPLongName('Other'), CLPDescription('Another flag', '<bool>')]
    property Other: Boolean read FOther write FOther;
    [CLPLongName('Alpha'), CLPDescription('A string', '<string>'), CLPDefault('')]
    property Alpha: string read FAlpha write FAlpha;
  end;

procedure TBooleanTests.FlagWithoutValueSetsTrue;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TBoolOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Flag', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

procedure TBooleanTests.ExplicitTrue;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TBoolOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Flag:True', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

procedure TBooleanTests.ExplicitFalse;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TBoolOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Flag:False', LOpts), LParser.ErrorInfo.Text);
    Assert.IsFalse(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

procedure TBooleanTests.ExplicitTrueLowercase;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TBoolOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Flag:true', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

procedure TBooleanTests.ExplicitFalseLowercase;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TBoolOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Flag:false', LOpts), LParser.ErrorInfo.Text);
    Assert.IsFalse(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

procedure TBooleanTests.InvalidValueReturnsFalseAndSetsError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TBoolOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Flag:notBoolean', LOpts));
    Assert.IsTrue(LParser.ErrorInfo.IsError);
  finally
    LOpts.Free;
  end;
end;

procedure TBooleanTests.AbsentBooleanRemainsFalse;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TBoolOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.IsFalse(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

procedure TBooleanTests.TwoBooleanFlagsBothSetTrue;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TBoolOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Flag -Other', LOpts), LParser.ErrorInfo.Text);
    Assert.IsTrue(LOpts.Flag);
    Assert.IsTrue(LOpts.Other);
  finally
    LOpts.Free;
  end;
end;

procedure TBooleanTests.BooleanCanFollowOtherSwitch;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TBoolOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Alpha:hello -Flag', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('hello', LOpts.Alpha);
    Assert.IsTrue(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

end.

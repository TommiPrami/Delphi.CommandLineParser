unit Delphi.CommandLineParser.PositionalTests.DUnitX;

interface

uses
  System.SysUtils, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TSingleOptional = class
  strict private
    FFirst: string;
  public
    [CLPPosition(1), CLPDescription('First positional', '<string>'), CLPDefault('')]
    property First: string read FFirst write FFirst;
  end;

  TTwoRequired = class
  strict private
    FFirst: string;
    FSecond: string;
  public
    [CLPPosition(1), CLPRequired, CLPDescription('First', '<string>')]
    property First: string read FFirst write FFirst;
    [CLPPosition(2), CLPRequired, CLPDescription('Second', '<string>')]
    property Second: string read FSecond write FSecond;
  end;

  TRequiredAfterOptional = class
  strict private
    FFirst: string;
    FSecond: string;
  public
    [CLPPosition(1), CLPDescription('First', '<string>'), CLPDefault('')]
    property First: string read FFirst write FFirst;
    [CLPPosition(2), CLPRequired, CLPDescription('Second', '<string>')]
    property Second: string read FSecond write FSecond;
  end;

  TMixedPositionalAndSwitch = class
  strict private
    FFirst: string;
    FFlag: Boolean;
  public
    [CLPPosition(1), CLPDescription('First', '<string>'), CLPDefault('')]
    property First: string read FFirst write FFirst;
    [CLPLongName('Flag'), CLPDescription('Flag', '<bool>')]
    property Flag: Boolean read FFlag write FFlag;
  end;

  TInvalidPositionZero = class
  strict private
    FFirst: string;
  public
    [CLPPosition(0), CLPDescription('First', '<string>'), CLPDefault('')]
    property First: string read FFirst write FFirst;
  end;

  TGappedPositionals = class
  strict private
    FFirst: string;
    FThird: string;
  public
    [CLPPosition(1), CLPDescription('First', '<string>'), CLPDefault('')]
    property First: string read FFirst write FFirst;
    [CLPPosition(3), CLPDescription('Third', '<string>'), CLPDefault('')]
    property Third: string read FThird write FThird;
  end;

  [TestFixture]
  TPositionalTests = class(TObject)
  public
    [Test] procedure SingleOptionalPositional;
    [Test] procedure TwoRequiredPositionals;
    [Test] procedure RequiredPositionalMissingFails;
    [Test] procedure ExtraPositionalArgumentFails;
    [Test] procedure RequiredAfterOptionalRaisesConfigError;
    [Test] procedure PositionalsAndSwitchesMix;
    [Test] procedure PositionMustBePositive;
    [Test] procedure MissingPositionalDefinitionRaisesConfigError;
  end;

implementation

procedure TPositionalTests.SingleOptionalPositional;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TSingleOptional.Create;
  try
    Assert.IsTrue(LParser.Parse('value1', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('value1', LOpts.First);
  finally
    LOpts.Free;
  end;
end;

procedure TPositionalTests.TwoRequiredPositionals;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TTwoRequired.Create;
  try
    Assert.IsTrue(LParser.Parse('one two', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('one', LOpts.First);
    Assert.AreEqual('two', LOpts.Second);
  finally
    LOpts.Free;
  end;
end;

procedure TPositionalTests.RequiredPositionalMissingFails;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TTwoRequired.Create;
  try
    Assert.IsFalse(LParser.Parse('one', LOpts));
    Assert.IsTrue(LParser.ErrorInfo.IsError);
  finally
    LOpts.Free;
  end;
end;

procedure TPositionalTests.ExtraPositionalArgumentFails;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TTwoRequired.Create;
  try
    Assert.IsFalse(LParser.Parse('one two three', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TPositionalTests.RequiredAfterOptionalRaisesConfigError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRequiredAfterOptional.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        LParser.Parse('a b', LOpts);
      end,
      Exception);
  finally
    LOpts.Free;
  end;
end;

procedure TPositionalTests.PositionalsAndSwitchesMix;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TMixedPositionalAndSwitch.Create;
  try
    Assert.IsTrue(LParser.Parse('-Flag positionalValue', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('positionalValue', LOpts.First);
    Assert.IsTrue(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

procedure TPositionalTests.PositionMustBePositive;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TInvalidPositionZero.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        LParser.Parse('', LOpts);
      end,
      Exception);
  finally
    LOpts.Free;
  end;
end;

procedure TPositionalTests.MissingPositionalDefinitionRaisesConfigError;
begin
  // Positions 1 and 3 defined; position 2 missing.
  var LParser := CreateCommandLineParser;
  var LOpts := TGappedPositionals.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        LParser.Parse('', LOpts);
      end,
      Exception);
  finally
    LOpts.Free;
  end;
end;

end.

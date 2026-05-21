unit Delphi.CommandLineParser.PositionRestTests.DUnitX;

interface

uses
  System.SysUtils, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TOnlyRest = class
  strict private
    FRest: string;
  public
    [CLPPositionRest, CLPDescription('Rest', '<list>'), CLPDefault('')]
    property Rest: string read FRest write FRest;
  end;

  TFirstPlusRest = class
  strict private
    FFirst: string;
    FRest: string;
  public
    [CLPPosition(1), CLPRequired, CLPDescription('First', '<string>')]
    property First: string read FFirst write FFirst;
    [CLPPositionRest, CLPDescription('Rest', '<list>'), CLPDefault('')]
    property Rest: string read FRest write FRest;
  end;

  TTwoRest = class
  strict private
    FRestA: string;
    FRestB: string;
  public
    [CLPPositionRest, CLPDescription('RestA', '<list>'), CLPDefault('')]
    property RestA: string read FRestA write FRestA;
    [CLPPositionRest, CLPDescription('RestB', '<list>'), CLPDefault('')]
    property RestB: string read FRestB write FRestB;
  end;

  TNonStringRest = class
  strict private
    FRest: Integer;
  public
    [CLPPositionRest, CLPDescription('Rest', '<int>'), CLPDefault('0')]
    property Rest: Integer read FRest write FRest;
  end;

  [TestFixture]
  TPositionRestTests = class(TObject)
  public
    [Test] procedure SingleRestCollectsAllPositionals;
    [Test] procedure FirstPositionalThenRest;
    [Test] procedure NoPositionalsLeavesRestEmpty;
    [Test] procedure DuplicatePositionRestRaisesConfigError;
    [Test] procedure NonStringPositionRestRaisesConfigError;
  end;

implementation

procedure TPositionRestTests.SingleRestCollectsAllPositionals;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TOnlyRest.Create;
  try
    Assert.IsTrue(LParser.Parse('alpha beta gamma', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('alpha'#13'beta'#13'gamma', LOpts.Rest);
  finally
    LOpts.Free;
  end;
end;

procedure TPositionRestTests.FirstPositionalThenRest;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFirstPlusRest.Create;
  try
    Assert.IsTrue(LParser.Parse('primary extra1 extra2', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('primary', LOpts.First);
    Assert.AreEqual('extra1'#13'extra2', LOpts.Rest);
  finally
    LOpts.Free;
  end;
end;

procedure TPositionRestTests.NoPositionalsLeavesRestEmpty;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TOnlyRest.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('', LOpts.Rest);
  finally
    LOpts.Free;
  end;
end;

procedure TPositionRestTests.DuplicatePositionRestRaisesConfigError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TTwoRest.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        LParser.Parse('', LOpts);
      end,
      ECLPConfigurationError);
  finally
    LOpts.Free;
  end;
end;

procedure TPositionRestTests.NonStringPositionRestRaisesConfigError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TNonStringRest.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        LParser.Parse('', LOpts);
      end,
      ECLPConfigurationError);
  finally
    LOpts.Free;
  end;
end;

end.

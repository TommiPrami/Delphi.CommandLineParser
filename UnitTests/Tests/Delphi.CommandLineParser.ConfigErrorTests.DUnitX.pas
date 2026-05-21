unit Delphi.CommandLineParser.ConfigErrorTests.DUnitX;

interface

uses
  System.SysUtils, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TUnsupportedTypeOpts = class
  strict private
    FValue: Double;
  public
    [CLPLongName('Value'), CLPDescription('Value', '<float>'), CLPDefault('0')]
    property Value: Double read FValue write FValue;
  end;

  TUnsupportedArrayOpts = class
  strict private
    FItems: TArray<Integer>;
  public
    [CLPLongName('Items'), CLPDescription('Items', '<ints>')]
    property Items: TArray<Integer> read FItems write FItems;
  end;

  TBadPositional = class
  strict private
    FFirst: string;
  public
    [CLPPosition(0), CLPDescription('First', '<string>'), CLPDefault('')]
    property First: string read FFirst write FFirst;
  end;

  [TestFixture]
  TConfigErrorTests = class(TObject)
  public
    [Test] procedure UnsupportedScalarTypeRaises;
    [Test] procedure UnsupportedArrayElementTypeRaises;
    [Test] procedure ConfigErrorExceptionIsECLPConfigurationError;
  end;

implementation

procedure TConfigErrorTests.UnsupportedScalarTypeRaises;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TUnsupportedTypeOpts.Create;
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

procedure TConfigErrorTests.UnsupportedArrayElementTypeRaises;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TUnsupportedArrayOpts.Create;
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

procedure TConfigErrorTests.ConfigErrorExceptionIsECLPConfigurationError;
begin
  // Position 0 is an illegal positional configuration — must raise
  // ECLPConfigurationError specifically (not just any Exception).
  var LParser := CreateCommandLineParser;
  var LOpts := TBadPositional.Create;
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

unit Delphi.CommandLineParser.SimpleTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  [TestFixture]
  TCommandLineParserDUnitX = class(TObject)
  public
    [Test]
    procedure CreateAndFreeTest;

    [Test]
    procedure SimpleDefaultTest;

    [Test]
    procedure SimpleNonDefaultTest;

    [Test]
    procedure SimpleNonDefaultEqualSeparatorTest;

    [Test]
    procedure SimpleReguiredNoDefaultTest;
  end;

  TEnumForParametrer = (Enum1, Enum2, Enum3);

  TSimpleCommandLine = class(TObject)
  strict private
    FStringParameter: string;
    FIntegerParameter: Integer;
    FEnumParameter: TEnumForParametrer;
  public
    [CLPLongName('StringParameter'), CLPDescription('String parameter', '<string>'), CLPDefault('default')]
    property StringParameter: string read FStringParameter write FStringParameter;

    [CLPLongName('IntegerParameter'), CLPDescription('Integer parameter', '<Integer>'), CLPDefault('1')]
    property IntegerParameter: Integer read FIntegerParameter write FIntegerParameter;

    [CLPLongName('EnumParameter'), CLPDescription('Enum parameter', '<Enum1, Enum2, Enum3>'), CLPDefault('Enum3')]
    property EnumParameter: TEnumForParametrer read FEnumParameter write FEnumParameter;
  end;

  TRequiredCommandLine = class(TObject)
  strict private
    FStringParameter: string;
    FIntegerParameter: Integer;
    FEnumParameter: TEnumForParametrer;
  public
    [CLPLongName('StringParameter'), CLPDescription('String parameter', '<string>'), CLPRequired]
    property StringParameter: string read FStringParameter write FStringParameter;

    [CLPLongName('IntegerParameter'), CLPDescription('Integer parameter', '<Integer>'), CLPRequired]
    property IntegerParameter: Integer read FIntegerParameter write FIntegerParameter;

    [CLPLongName('EnumParameter'), CLPDescription('Enum parameter', '<Enum1, Enum2, Enum3>'), CLPRequired]
    property EnumParameter: TEnumForParametrer read FEnumParameter write FEnumParameter;
  end;

implementation

{ TCommandLineParserDUnitX }

procedure TCommandLineParserDUnitX.CreateAndFreeTest;
begin
  var LCommandLineParser := CreateCommandLineParser;
  try
    Assert.IsTrue(LCommandLineParser.Options = []);
  finally
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;

  Assert.Pass('If no exceptions, all should be OK');
end;

procedure TCommandLineParserDUnitX.SimpleDefaultTest;
begin
  var LCommandLineParser := CreateCommandLineParser;
  var LOptions := TSimpleCommandLine.Create;
  try
    Assert.IsTrue(LCommandLineParser.Parse('', LOptions), 'Parse command line rerurned False');

    Assert.AreEqual('default', LOptions.StringParameter, 'Default string parameter has wrong value');
    Assert.AreEqual(1, LOptions.IntegerParameter, 'Default Integer parameter has wrong value');
    Assert.AreEqual(Integer(Enum3), Integer(LOptions.EnumParameter), 'Default Enum parameter has wrong value');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;
end;

procedure TCommandLineParserDUnitX.SimpleNonDefaultTest;
const
  STRING_VALUE = 'SomeStringValueThatHasNoSpaces';
begin
  var LCommandLineParser := CreateCommandLineParser;
  var LOptions := TSimpleCommandLine.Create;
  try
    Assert.IsTrue(LCommandLineParser.Parse('-StringParameter:' + STRING_VALUE + ' -IntegerParameter:666 -EnumParameter:Enum2', LOptions), 'Parse command line rerurned False');

    Assert.AreEqual(STRING_VALUE, LOptions.StringParameter, 'Default string parameter has wrong value');
    Assert.AreEqual(666, LOptions.IntegerParameter, 'Default Integer parameter has wrong value');
    Assert.AreEqual(Integer(Enum2), Integer(LOptions.EnumParameter), 'Default Enum parameter has wrong value');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;
end;

procedure TCommandLineParserDUnitX.SimpleNonDefaultEqualSeparatorTest;
const
  STRING_VALUE = 'SomeStringValueThatHasNoSpaces';
begin
  var LCommandLineParser := CreateCommandLineParser;
  var LOptions := TSimpleCommandLine.Create;
  try
    Assert.IsTrue(LCommandLineParser.Parse('-StringParameter=' + STRING_VALUE + ' -IntegerParameter=666 -EnumParameter=Enum2', LOptions), 'Parse command line rerurned False');

    Assert.AreEqual(STRING_VALUE, LOptions.StringParameter, 'Default string parameter has wrong value');
    Assert.AreEqual(666, LOptions.IntegerParameter, 'Default Integer parameter has wrong value');
    Assert.AreEqual(Integer(Enum2), Integer(LOptions.EnumParameter), 'Default Enum parameter has wrong value');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;
end;

procedure TCommandLineParserDUnitX.SimpleReguiredNoDefaultTest;
begin
  var LCommandLineParser := CreateCommandLineParser;
  var LOptions := TRequiredCommandLine.Create;
  try
    Assert.IsFalse(LCommandLineParser.Parse('', LOptions), 'Parse command line rerurned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-StringParameter:str', LOptions), 'Parse command line rerurned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-IntegerParameter:666', LOptions), 'Parse command line rerurned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-EnumParameter:Enum2', LOptions), 'Parse command line rerurned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-StringParameter:str -IntegerParameter:666', LOptions), 'Parse command line rerurned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-StringParameter:str -EnumParameter:Enum2', LOptions), 'Parse command line rerurned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-IntegerParameter:666 -StringParameter:str', LOptions), 'Parse command line rerurned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-IntegerParameter:666 -EnumParameter:Enum2', LOptions), 'Parse command line rerurned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-EnumParameter:Enum2 -StringParameter:str', LOptions), 'Parse command line rerurned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-EnumParameter:Enum2 -IntegerParameter:666', LOptions), 'Parse command line rerurned True even should not');

    Assert.IsTrue(LCommandLineParser.Parse('-EnumParameter:Enum2 -IntegerParameter:666 -StringParameter:str', LOptions), 'Parse command line rerurned True even should not');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TCommandLineParserDUnitX);

end.

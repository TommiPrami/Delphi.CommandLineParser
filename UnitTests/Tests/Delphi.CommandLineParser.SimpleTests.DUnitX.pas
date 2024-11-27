unit Delphi.CommandLineParser.SimpleTests.DUnitX;

interface

uses
  System.Classes, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  [TestFixture]
  TCommandLineParserSimpleTestsDUnitX = class(TObject)
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

    [Test]
    procedure SimpleListParameterTest;
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

  TListCommandLine = class(TObject)
  strict private
    FArrayParameter: TArray<string>;
    // FListParameter: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    [CLPLongName('ArrayParameter'), CLPDescription('Array Parameter', '<list of string items>'), CLPDefault('')]
    property ArrayParameter: TArray<string> read FArrayParameter write FArrayParameter;

    // TODO: Add support for TStrings parameter type.
    // [CLPLongName('ListParameter'), CLPDescription('List Parameter', '<list of string items>'), CLPDefault('')]
    // property ListParameter: TStringList read FListParameter write FListParameter;
  end;

implementation

{ TCommandLineParserSimpleTestsDUnitX }

procedure TCommandLineParserSimpleTestsDUnitX.CreateAndFreeTest;
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

procedure TCommandLineParserSimpleTestsDUnitX.SimpleDefaultTest;
begin
  var LCommandLineParser := CreateCommandLineParser;
  var LOptions := TSimpleCommandLine.Create;
  try
    Assert.IsTrue(LCommandLineParser.Parse('', LOptions), 'Parse command line returned False');

    Assert.AreEqual('default', LOptions.StringParameter, 'Default string parameter has wrong value');
    Assert.AreEqual(1, LOptions.IntegerParameter, 'Default Integer parameter has wrong value');
    Assert.AreEqual(Integer(Enum3), Integer(LOptions.EnumParameter), 'Default Enum parameter has wrong value');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;
end;

procedure TCommandLineParserSimpleTestsDUnitX.SimpleNonDefaultTest;
const
  STRING_VALUE = 'SomeStringValueThatHasNoSpaces';
begin
  var LCommandLineParser := CreateCommandLineParser;
  var LOptions := TSimpleCommandLine.Create;
  try
    Assert.IsTrue(LCommandLineParser.Parse('-StringParameter:' + STRING_VALUE + ' -IntegerParameter:666 -EnumParameter:Enum2', LOptions), 'Parse command line returned False');

    Assert.AreEqual(STRING_VALUE, LOptions.StringParameter, 'Default string parameter has wrong value');
    Assert.AreEqual(666, LOptions.IntegerParameter, 'Default Integer parameter has wrong value');
    Assert.AreEqual(Integer(Enum2), Integer(LOptions.EnumParameter), 'Default Enum parameter has wrong value');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;
end;

procedure TCommandLineParserSimpleTestsDUnitX.SimpleNonDefaultEqualSeparatorTest;
const
  STRING_VALUE = 'SomeStringValueThatHasNoSpaces';
begin
  var LCommandLineParser := CreateCommandLineParser;
  var LOptions := TSimpleCommandLine.Create;
  try
    Assert.IsTrue(LCommandLineParser.Parse('-StringParameter=' + STRING_VALUE + ' -IntegerParameter=666 -EnumParameter=Enum2', LOptions), 'Parse command line returned False');

    Assert.AreEqual(STRING_VALUE, LOptions.StringParameter, 'Default string parameter has wrong value');
    Assert.AreEqual(666, LOptions.IntegerParameter, 'Default Integer parameter has wrong value');
    Assert.AreEqual(Integer(Enum2), Integer(LOptions.EnumParameter), 'Default Enum parameter has wrong value');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;
end;

procedure TCommandLineParserSimpleTestsDUnitX.SimpleReguiredNoDefaultTest;
begin
  var LCommandLineParser := CreateCommandLineParser;
  var LOptions := TRequiredCommandLine.Create;
  try
    Assert.IsFalse(LCommandLineParser.Parse('', LOptions), 'Parse command line returned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-StringParameter:str', LOptions), 'Parse command line returned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-IntegerParameter:666', LOptions), 'Parse command line returned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-EnumParameter:Enum2', LOptions), 'Parse command line returned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-StringParameter:str -IntegerParameter:666', LOptions), 'Parse command line returned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-StringParameter:str -EnumParameter:Enum2', LOptions), 'Parse command line returned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-IntegerParameter:666 -StringParameter:str', LOptions), 'Parse command line returned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-IntegerParameter:666 -EnumParameter:Enum2', LOptions), 'Parse command line returned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-EnumParameter:Enum2 -StringParameter:str', LOptions), 'Parse command line returned True even should not');
    Assert.IsFalse(LCommandLineParser.Parse('-EnumParameter:Enum2 -IntegerParameter:666', LOptions), 'Parse command line returned True even should not');

    Assert.IsTrue(LCommandLineParser.Parse('-EnumParameter:Enum2 -IntegerParameter:666 -StringParameter:str', LOptions), 'Parse command line returned False even should not');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;
end;

procedure TCommandLineParserSimpleTestsDUnitX.SimpleListParameterTest;
begin
  var LCommandLineParser := CreateCommandLineParser;
  var LOptions := TListCommandLine.Create;
  try
    Assert.IsTrue(LCommandLineParser.Parse('', LOptions), 'Parse command line returned False even should not');

    // Single parameter
    Assert.IsTrue(LCommandLineParser.Parse('-ArrayParameter:arrayitem1', LOptions), 'Parse command line returned False even should not');
    Assert.AreEqual(1, Length(LOptions.ArrayParameter), 'Did not have 1 item');
    Assert.AreEqual('arrayitem1', LOptions.ArrayParameter[0], 'item 0 had wrong value');

    // multiple in list
    Assert.IsTrue(LCommandLineParser.Parse('-ArrayParameter:arrayitem1;arrayitem2;arrayitem3', LOptions), 'Parse command line returned False even should not');
    Assert.AreEqual(3, Length(LOptions.ArrayParameter), 'Did not have 3 items');
    Assert.AreEqual('arrayitem1', LOptions.ArrayParameter[0], 'item 0 had wrong value');
    Assert.AreEqual('arrayitem2', LOptions.ArrayParameter[1], 'item 1 had wrong value');
    Assert.AreEqual('arrayitem3', LOptions.ArrayParameter[2], 'item 2 had wrong value');

    // multiple items with spaces Separate quoted params
    Assert.IsTrue(LCommandLineParser.Parse('-ArrayParameter:"arrayitem 1";"arrayitem 2";"arrayitem 3"', LOptions), 'Parse command line returned False even should not: ' + LCommandLineParser.ErrorInfo.Text.QuotedString('"'));
    Assert.AreEqual(3, Length(LOptions.ArrayParameter), 'Did not have 3 items');
    Assert.AreEqual('arrayitem 1', LOptions.ArrayParameter[0], 'item 0 had wrong value');
    Assert.AreEqual('arrayitem 2', LOptions.ArrayParameter[1], 'item 1 had wrong value');
    Assert.AreEqual('arrayitem 3', LOptions.ArrayParameter[2], 'item 2 had wrong value');

    // multiple items with spaces Separate single quoted param
    Assert.IsTrue(LCommandLineParser.Parse('-ArrayParameter:"arrayitem 1;arrayitem 2;arrayitem 3"', LOptions), 'Parse command line returned False even should not: ' + LCommandLineParser.ErrorInfo.Text.QuotedString('"'));
    Assert.AreEqual(3, Length(LOptions.ArrayParameter), 'Did not have 3 items');
    Assert.AreEqual('arrayitem 1', LOptions.ArrayParameter[0], 'item 0 had wrong value');
    Assert.AreEqual('arrayitem 2', LOptions.ArrayParameter[1], 'item 1 had wrong value');
    Assert.AreEqual('arrayitem 3', LOptions.ArrayParameter[2], 'item 2 had wrong value');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;
end;

{ TListCommandLine }

constructor TListCommandLine.Create;
begin
  inherited Create;

  // FListParameter := TStringList.Create;
end;

destructor TListCommandLine.Destroy;
begin
  // FListParameter.Free;

  inherited Destroy;
end;

initialization
  TDUnitX.RegisterTestFixture(TCommandLineParserSimpleTestsDUnitX);

end.

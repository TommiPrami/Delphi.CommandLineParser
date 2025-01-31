﻿unit Delphi.CommandLineParser.SimpleTests.DUnitX;

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
    FBooleanParameter: Boolean;
    // FSingleParameter: Single;
    // FDoubleParameter: Double;
    // FDateParameter: TDate;
    // FTimeParameter: TTime;
    // FDateTimeParameter: TDateTime;
  public
    [CLPLongName('StringParameter'), CLPDescription('String parameter', '<string>'), CLPDefault('default')]
    property StringParameter: string read FStringParameter write FStringParameter;

    [CLPLongName('IntegerParameter'), CLPDescription('Integer parameter', '<Integer>'), CLPDefault('1')]
    property IntegerParameter: Integer read FIntegerParameter write FIntegerParameter;

    [CLPLongName('EnumParameter'), CLPDescription('Enum parameter', '<Enum1, Enum2, Enum3>'), CLPDefault('Enum3')]
    property EnumParameter: TEnumForParametrer read FEnumParameter write FEnumParameter;

    [CLPLongName('BooleanParameter'), CLPDescription('Boolean parameter', '<True/False>')]
    property BooleanParameter: Boolean read FBooleanParameter write FBooleanParameter;

    // [CLPLongName('SingleParameter'), CLPDescription('Single parameter', '<Float>')]
    // property SingleParameter: Single read FSingleParameter write FSingleParameter;

    //[CLPLongName('DoubleParameter'), CLPDescription('Double parameter', '<Float>')]
    // property DoubleParameter: Double read FDoubleParameter write FDoubleParameter;

    //[CLPLongName('DateParameter'), CLPDescription('Date parameter', '<Date>')]
    // property DateParameter: TDate read FDateParameter write FDateParameter;

    //[CLPLongName('TimeParameter'), CLPDescription('Time parameter', '<Time>')]
    // property TimeParameter: TTime read FTimeParameter write FTimeParameter;

    //[CLPLongName('DateTimeParameter'), CLPDescription('DateTime parameter', '<DateTime>')]
    // property DateTimeParameter: TDateTime read FDateTimeParameter write FDateTimeParameter;
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
    // FStringsParameter: TStrings;
  public
    constructor Create;
    destructor Destroy; override;

    [CLPLongName('ArrayParameter'), CLPDescription('Array Parameter', '<list of string items>'), CLPDefault('')]
    property ArrayParameter: TArray<string> read FArrayParameter write FArrayParameter;

    //[CLPLongName('StringsParameter'), CLPDescription('Strings parameter', '<List of strings>')]
    // property StringsParameter: TStrings read FStringsParameter write FStringsParameter;
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
var
  LCommandLineParser: ICommandLineParser;
  LOptions: TSimpleCommandLine;
begin
  LCommandLineParser := CreateCommandLineParser;
  LOptions := TSimpleCommandLine.Create;
  try
    Assert.IsTrue(LCommandLineParser.Parse('', LOptions), 'Parse command line returned False');

    Assert.AreEqual('default', LOptions.StringParameter, 'Default string parameter has wrong value');
    Assert.AreEqual(1, LOptions.IntegerParameter, 'Default Integer parameter has wrong value');
    Assert.AreEqual(Integer(Enum3), Integer(LOptions.EnumParameter), 'Default Enum parameter has wrong value');
    Assert.IsFalse(LOptions.BooleanParameter, 'Default BooleanParameter should be False, got True');
    // Assert.AreEqual(0.00, LOptions.SingleParameter, 0.00, 'Default Single parameter should be 0.00');
    // Assert.AreEqual(0.00, LOptions.DoubleParameter, 0.00, 'Default Double parameter should be 0.00');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;
end;

procedure TCommandLineParserSimpleTestsDUnitX.SimpleNonDefaultTest;
const
  STRING_VALUE = 'SomeStringValueThatHasNoSpaces';
var
  LCommandLineParser: ICommandLineParser;
  LOptions: TSimpleCommandLine;
begin
  LCommandLineParser := CreateCommandLineParser;
  LOptions := TSimpleCommandLine.Create;
  try
    Assert.IsTrue(LCommandLineParser.Parse('-StringParameter:' + STRING_VALUE + ' -IntegerParameter:666 -EnumParameter:Enum2 -BooleanParameter', LOptions), 'Parse command line returned False');

    Assert.AreEqual(STRING_VALUE, LOptions.StringParameter, 'Default string parameter has wrong value');
    Assert.AreEqual(666, LOptions.IntegerParameter, 'Default Integer parameter has wrong value');
    Assert.AreEqual(Integer(Enum2), Integer(LOptions.EnumParameter), 'Default Enum parameter has wrong value');
    Assert.IsTrue(LOptions.BooleanParameter, 'BooleanParameter should be True, got False');
    // Assert.AreEqual(0.00, LOptions.SingleParameter, 0.00, 'Default Single parameter should be 0.00');
    // Assert.AreEqual(0.00, LOptions.DoubleParameter, 0.00, 'Default Double parameter should be 0.00');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;

  LCommandLineParser := CreateCommandLineParser;
  LOptions := TSimpleCommandLine.Create;
  try
    Assert.IsTrue(LCommandLineParser.Parse('-BooleanParameter:False', LOptions), 'Parse command line returned False');
    Assert.IsFalse(LOptions.BooleanParameter, 'BooleanParameter should be False, got True');
  finally
    LOptions.Free;
    LCommandLineParser := nil;
    FreeCommandLineParser;
  end;

  LCommandLineParser := CreateCommandLineParser;
  LOptions := TSimpleCommandLine.Create;
  try
    Assert.IsTrue(LCommandLineParser.Parse('-BooleanParameter:True', LOptions), 'Parse command line returned False');
    Assert.IsTrue(LOptions.BooleanParameter, 'BooleanParameter should be True, got False');
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

  // FStringsParameter := TStringList.Create;
end;

destructor TListCommandLine.Destroy;
begin
  // FStringsParameter.Free;

  inherited Destroy;
end;

initialization
  TDUnitX.RegisterTestFixture(TCommandLineParserSimpleTestsDUnitX);

end.

unit Delphi.CommandLineParser;

{
  Check https://github.com/TommiPrami/Delphi.CommandLineParser/blob/main/README.md for more information
}
interface

uses
  System.RTTI, System.SysUtils, System.TypInfo;

type
  // TODO: add Skip (etc) attribute, to parser just skip the prpperty all to gether, so there could be properties that are helpers fopr App but they are not (never ever) coming from the command line

  ///  <summary>
  ///    Specifies short (one letter) name for the switch.
  ///  </summary>
  CLPNameAttribute = class(TCustomAttribute)
  strict private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  ///  <summary>
  ///    Specifies long name for the switch. If not set, property name is used
  ///    for long name.
  ///   A short form of the long name can also be provided which must match the beginning
  ///   of the long form. In this case, parser will accept shortened versions of the
  ///   long name, but no shorter than the short form.
  ///   An example: if 'longName' = 'autotest' and 'shortForm' = 'auto' then the parser
  ///   will accept 'auto', 'autot', 'autote', 'autotes' and 'autotest', but not 'aut',
  ///   'au' and 'a'.
  ///   Multiple long names (alternate switches) can be provided for one entity.
  ///  </summary>
  CLPLongNameAttribute = class(TCustomAttribute)
  strict private
    FLongName : string;
    FShortForm: string;
  public
    constructor Create(const ALongName: string; const AShortForm: string = '');
    property LongName: string read FLongName;
    property ShortForm: string read FShortForm;
  end;

  ///  <summary>
  ///    Specifies default value which will be used if switch is not found on
  ///    the command line.
  ///  </summary>
  CLPDefaultAttribute = class(TCustomAttribute)
  strict private
    FDefaultValue: string;
  public
    constructor Create(const AValue: string); overload;
    property DefaultValue: string read FDefaultValue;
  end;

  ///  <summary>
  ///    Provides switch description, used for the Usage function.
  ///  </summary>
  CLPDescriptionAttribute = class(TCustomAttribute)
  strict private
    FDescription: string;
    FParamName: string;
  public
    const DefaultValue = 'value';
    constructor Create(const ADescription: string; const AParamName: string = '');
    property Description: string read FDescription;
    property ParamName: string read FParamName;
  end;

  ///  <summary>
  ///    When present, specifies that the switch is required.
  ///  </summary>
  CLPRequiredAttribute = class(TCustomAttribute)
  public
  end;

  ///  <summary>
  ///    When present, adds string parameter check that file must exist.
  ///  </summary>
  CLPFileMustExistAttribute = class(TCustomAttribute)
  public
  end;

  ///  <summary>
  ///    When present, adds string parameter check that file must exist.
  ///  </summary>
  CLPDirectoryMustExistAttribute = class(TCustomAttribute)
  public
  end;

  /// <summary>
  ///   When present, specifies that the switch name can be arbitrarily extended.
  ///   The code can access this extension via GetExtension function.
  /// </summary>
  CLPExtendableAttribute = class(TCustomAttribute)
  public
  end;

  ///  <summary>
  ///    Specifies position of a positional (unnamed) switch. First positional
  ///    switch has position 1.
  ///  </summary>
  CLPPositionAttribute = class(TCustomAttribute)
  strict private
    FPosition: Integer;
  public
    constructor Create(const APosition: Integer);
    property Position: Integer read FPosition;
  end;

  ///  <summary>
  ///    Specifies switch that will receive a #13-delimited list of all
  ///    positional parameters for which switch definitions don't exist.
  ///  </summary>
  CLPPositionRestAttribute = class(TCustomAttribute)
  public
  end;

  TCLPErrorKind = (
    //configuration error, will result in exception
    ekPositionalsBadlyDefined, ekNameNotDefined, ekShortNameTooLong, ekLongFormsDontMatch,
    //user data error, will result in error result
    ekMissingPositional, ekExtraPositional, ekMissingNamed, ekUnknownNamed, ekInvalidData,
    ekWronDataTypeForFileOrDirectoryMustExist, ekFileDoNotExist, ekDirectoryDoNotExist);

  TCLPErrorDetailed = (
    edBooleanWithData,             // SBooleanSwitchCannotAcceptData
    edCLPPositionRestNotString,    // STypeOfACLPPositionRestPropertyMu
    edExtraCLPPositionRest,        // SOnlyOneCLPPositionRestPropertyIs
    edInvalidDataForSwitch,        // SInvalidDataForSwitch
    edLongFormsDontMatch,          // SLongFormsDontMatch
    edMissingNameForProperty,      // SMissingNameForProperty
    edMissingPositionalDefinition, // SMissingPositionalParameterDefini
    edMissingRequiredSwitch,       // SRequiredSwitchWasNotProvided
    edPositionNotPositive,         // SPositionMustBeGreaterOrEqualTo1
    edRequiredAfterOptional,       // SRequiredPositionalParametersMust
    edShortNameTooLong,            // SShortNameMustBeOneLetterLong
    edTooManyPositionalArguments,  // STooManyPositionalArguments
    edUnknownSwitch,               // SUnknownSwitch
    edUnsupportedPropertyType,     // SUnsupportedPropertyType
    edMissingRequiredParameter,    // SRequiredParameterWasNotProvided
    edWrongDataTypeForFileOrDirectoryMustExist, // sWrongDataTypeForFileOrDirectoryMustExist
    edFileDoNotExist,              // SFileDoNotExist
    edDirectoryDoNotExist          // SDirectoryDoNotExist
  );

  TCLPErrorInfo = record
    IsError: Boolean;
    Kind: TCLPErrorKind;
    Detailed: TCLPErrorDetailed;
    Position: Integer;
    SwitchName: string;
    Text: string;
  end;

  TCLPOption = (opIgnoreUnknownSwitches);
  TCLPOptions = set of TCLPOption;

  ICommandLineParser = interface ['{C9B729D4-3706-46DB-A8A2-1E07E04F497B}']
    function GetErrorInfo: TCLPErrorInfo;
    function GetOptions: TCLPOptions;
    procedure SetOptions(const AValue: TCLPOptions);
    function GetExtension(const ASwitch: string): string;
    function Usage(const AWrapAtColumn: Integer = 80): TArray<string>;
    function Parse(const ACommandData: TObject): Boolean; overload;
    function Parse(const ACommandLine: string; const ACommandData: TObject): Boolean; overload;
    property ErrorInfo: TCLPErrorInfo read GetErrorInfo;
    property Options: TCLPOptions read GetOptions write SetOptions;
  end;

  ECLPConfigurationError = class(Exception)
  strict private
    FErrorInfo: TCLPErrorInfo;
  public
    constructor Create(const AErrInfo: TCLPErrorInfo);
  end;

  ///  <summary>
  ///    Returns global parser instance. Not thread-safe.
  ///  </summary>
  function CommandLineParser: ICommandLineParser;

  ///  <summary>
  ///    Create new command line parser instance. Thread-safe.
  ///  </summary>
  function CreateCommandLineParser: ICommandLineParser;

  ///  <summary>
  ///    Default usage/error console output formatter.
  ///  </summary>
  procedure DefaultUsageConsoleOutput(const AParser: ICommandLineParser);

  procedure FreeCommandLineParser;

implementation

uses
  System.Classes, System.Character, System.Generics.Collections, System.Generics.Defaults, System.StrUtils;

resourcestring
  SBooleanSwitchCannotAcceptData    = 'Boolean switch cannot accept data, that can''t be converted to Boolean value use True/False.';
  SDefault                          = ', default: ';
  SInvalidDataForSwitch             = 'Invalid data for switch.';
  SLongFormsDontMatch               = 'Short version of the long name must match beginning of the long name';
  SMissingNameForProperty           = 'Missing name for property.';
  SMissingPositionalParameterDefini = 'Missing positional parameter definition.';
  SOnlyOneCLPPositionRestPropertyIs = 'Only one CLPPositionRest property is allowed.';
  SOptions                          = '[options]';
  SPositionMustBeGreaterOrEqualTo1  = 'Position must be greater or equal to 1.';
  SRequiredParameterWasNotProvided  = 'Required parameter was not provided.';
  SRequiredPositionalParametersMust = 'Required positional parameters must not appear after optional positional parameters.';
  SRequiredSwitchWasNotProvided     = 'Required switch was not provided.';
  SShortNameMustBeOneLetterLong     = 'Short name must be one letter long';
  STooManyPositionalArguments       = 'Too many positional arguments.';
  STypeOfACLPPositionRestPropertyMu = 'Type of a CLPPositionRest property must be string.';
  SUnknownSwitch                    = 'Unknown switch.';
  SUnsupportedPropertyType          = 'Unsupported property %s type.';
  SWrongDataTypeForFileOrDirectoryMustExist = 'Switch Datatype for file or directory check musht be string.';
  SFileDoNotExist                   = 'File must exist';
  SDirectoryDoNotExist              = 'Directory must exist';

type
  // Usage: LEnumNameStr := TEnumConverter.EnumToString(FormMain.BorderStyle);
  TEnumConverter = class
  public
    class function EnumToInt<T>(const AEnumValue: T): Integer;
    class function EnumToString<T>(const AEnumValue: T; const AStripLowercasePrefix: Boolean = False): string;
  end;

  TCLPSwitchType = (stString, stInteger, stBoolean, stEnumeration, stStringDynArray);

  TCLPSwitchOption = (soUnknown, soRequired, soPositional, soPositionRest, soExtendable,
    soFileMustExist, soDirectoryMustExist);
  TCLPSwitchOptions = set of TCLPSwitchOption;

  TCLPLongName = record
    LongForm: string;
    ShortForm: string;
    constructor Create(const ALongForm, AShortForm: string);
  end; { TCLPLongName }

  TCLPLongNames = TArray<TCLPLongName>;

  TSwitchData = class
  strict private
    FDefaultValue: string;
    FDescription: string;
    FInstance: TObject;
    FLongNames: TCLPLongNames;
    FName: string;
    FOptions: TCLPSwitchOptions;
    FParamDesc: string;
    FParamValue: string;
    FPosition: Integer;
    FPropertyName: string;
    FProvided: Boolean;
    FShortLongForm: string;
    FSwitchType: TCLPSwitchType;
  strict protected
    function Quote(const AValue: string): string;
  public
    constructor Create(const AInstance: TObject; const APropertyName, AName: string;
      const ALongNames: TCLPLongNames; const ASwitchType: TCLPSwitchType; const APosition: Integer;
      const AOptions: TCLPSwitchOptions; const ADefaultValue, ADescription, AParamName: string);
    function AppendValue(const AValue, ADelim: string; const ADoQuote: Boolean): Boolean;
    procedure Enable;
    function GetValue: string;
    function GetValueOrDefault: string;
    function SetValue(const AValue: string): Boolean;
    property DefaultValue: string read FDefaultValue;
    property Description: string read FDescription;
    property LongNames: TCLPLongNames read FLongNames;
    property Name: string read FName;
    property Options: TCLPSwitchOptions read FOptions;
    property ParamName: string read FParamDesc;
    property ParamValue: string read FParamValue write FParamValue;
    property Position: Integer read FPosition write FPosition;
    property PropertyName: string read FPropertyName;
    property Provided: Boolean read FProvided;
    property ShortLongForm: string read FShortLongForm;
    property SwitchType: TCLPSwitchType read FSwitchType;
  end;

const
  {$IFDEF MSWINDOWS}
    FSwitchDelims: array [0..2] of string = ('-', '/', '--');
  {$ELSE}
    FSwitchDelims: array [0..1] of string = ('--', '-');
  {$ENDIF}
    FParamDelims : array [0..1] of Char = (':', '=');

type
  TCommandLineParser = class(TInterfacedObject, ICommandLineParser)
  strict private
    FErrorInfo: TCLPErrorInfo;
    FOptions: TCLPOptions;
    FPositionals: TArray<TSwitchData>;
    FSwitchComparer: TStringComparer;
    FSwitchDict: TDictionary<string,TSwitchData>;
    FSwitchList: TObjectList<TSwitchData>;
  strict protected
    procedure AddSwitch(const AInstance: TObject; const APropertyName, AName: string;
      const ALongNames: TCLPLongNames; const ASwitchType: TCLPSwitchType; const APosition: Integer;
      const AOptions: TCLPSwitchOptions; const ADefaultValue, ADescription, AParamName: string);
    function CheckAttributes: Boolean;
    function FindExtendableSwitch(const ASwitchName: string; var AParam: string; var AData: TSwitchData): Boolean;
    function GetCommandLine: string;
    function GetErrorInfo: TCLPErrorInfo; inline;
    function GetOptions: TCLPOptions;
    function GrabNextElement(var s, el: string): Boolean;
    function IsSwitch(const ASwitchRawValue: string; var AParam: string; var AData: TSwitchData): Boolean;
    function MapPropertyType(const AProp: TRttiProperty; const APropertyRttiType: TRttiType): TCLPSwitchType;
    procedure ProcessAttributes(const AInstance: TObject; const AProp: TRttiProperty; const APropertyRttiType: TRttiType);
    function FileSystemCheck(const ASwitchData: TSwitchData): TCLPSwitchOption;
    function ProcessCommandLine(const ACommandData: TObject; const ACommandLine: string): Boolean;
    procedure ProcessDefinitionClass(const ACommandData: TObject);
    function SetError(const AKind: TCLPErrorKind; const ADetail: TCLPErrorDetailed; const AText: string;
      const APosition: Integer = 0; const ASwitchName: string = ''): Boolean;
    procedure SetOptions(const AValue: TCLPOptions);
  protected // used in TUsageFormatter
    property Positionals: TArray<TSwitchData> read FPositionals;
    property SwitchList: TObjectList<TSwitchData> read FSwitchList;
  public
    constructor Create;
    destructor  Destroy; override;
    function GetExtension(const ASwitch: string): string;
    function Parse(const ACommandLine: string; const ACommandData: TObject): Boolean; overload;
    function Parse(const ACommandData: TObject): Boolean; overload;
    function Usage(const AWrapAtColumn: Integer = 80): TArray<string>;
    property ErrorInfo: TCLPErrorInfo read GetErrorInfo;
    property Options: TCLPOptions read GetOptions write SetOptions;
  end;

  TUsageFormatter = class
  private
    function AddParameter(const AName, APrefix, ADelim: string; const AData: TSwitchData): string;
    procedure AlignAndWrap(const ASl: TStringList; const AWrapAtColumn: Integer);
    function Wrap(const AName: string; const AData: TSwitchData): string;
    function LastSpaceBefore(const s: string; const AStartPos: Integer): Integer;
    function GetCommandLinePrototype(const APositionalParams: TArray<TSwitchData>; const ASwitchList: TObjectList<TSwitchData>): string;
    function GetPositionalSwitchName(const ASwitchData: TSwitchData): string;
  public
    procedure Usage(const AParser: TCommandLineParser; const AWrapAtColumn: Integer;
      var AUsageList: TArray<string>);
  end;

var
  GCommandLineParser: ICommandLineParser;

class function TEnumConverter.EnumToInt<T>(const AEnumValue: T): Integer;
begin
  Result := 0;

  Move(AEnumValue, Result, SizeOf(AEnumValue));
end;

class function TEnumConverter.EnumToString<T>(const AEnumValue: T; const AStripLowercasePrefix: Boolean = False): string;
begin
  Result := GetEnumName(TypeInfo(T), EnumToInt(AEnumValue));

  if AStripLowercasePrefix and not Result.IsEmpty then
  begin
    var LIndex: Integer := 0;
    var LResultLength := Length(Result);

    while LIndex <= LResultLength do
    begin
      Inc(LIndex);

      if not Result[LIndex].IsLower then
        Break;
    end;

    Result := Copy(Result, LIndex, LResultLength);
  end;
end;


procedure DefaultUsageConsoleOutput(const AParser: ICommandLineParser);
var
  LUsageLines: TArray<string>;
  LLine: string;
  LIndex: Integer;
begin
  LUsageLines := AParser.Usage;

  WriteLn('Usage:');
  WriteLn('  ' + LUsageLines[0]);

  for LIndex := 1 to High(LUsageLines) do
  begin
    LLine := LUsageLines[LIndex];

    WriteLn('    ' + LLine);
  end;

  var LError := AParser.ErrorInfo.Text;

  if not LError.IsEmpty then
  begin
    WriteLn('');
    WriteLn('    Error: ' + AParser.ErrorInfo.SwitchName.QuotedString('"') + ' - ' + AParser.ErrorInfo.Text.QuotedString('"'));
  end;
end;

function SplitArray(const AStringValue: string): TArray<string>;
begin
  Result := AStringValue.Split([',', ';']);
end;

{ exports }

function CommandLineParser: ICommandLineParser;
begin
  if not Assigned(GCommandLineParser) then
    GCommandLineParser := CreateCommandLineParser;

  Result := GCommandLineParser;
end;

function CreateCommandLineParser: ICommandLineParser;
begin
  Result := TCommandLineParser.Create;
end;

procedure FreeCommandLineParser;
begin
  GCommandLineParser := nil;
end;

{ CLPNameAttribute }

constructor CLPNameAttribute.Create(const AName: string);
begin
  inherited Create;

  FName := AName;
end;

{ CLPLongNameAttribute }

constructor CLPLongNameAttribute.Create(const ALongName, AShortForm: string);
begin
  inherited Create;

  FLongName := ALongName;
  FShortForm := AShortForm;
end;

{ CLPDefaultAttribute }

constructor CLPDefaultAttribute.Create(const AValue: string);
begin
  inherited Create;

  FDefaultValue := AValue;
end;

{ CLPDescriptionAttribute }

constructor CLPDescriptionAttribute.Create(const ADescription: string; const AParamName: string);
begin
  inherited Create;

  FDescription := ADescription;

  if AParamName <> '' then
    FParamName := AParamName
  else
    FParamName := DefaultValue;
end;

{ CLPPositionAttribute }

constructor CLPPositionAttribute.Create(const APosition: Integer);
begin
  inherited Create;

  FPosition := APosition;
end;

{ TCLPLongName }

constructor TCLPLongName.Create(const ALongForm, AShortForm: string);
begin
  LongForm := ALongForm;
  ShortForm := AShortForm;
end;

{ TSwitchData }

constructor TSwitchData.Create(const AInstance: TObject; const APropertyName, AName: string;
  const ALongNames: TCLPLongNames; const ASwitchType: TCLPSwitchType; const APosition: Integer;
  const AOptions: TCLPSwitchOptions; const ADefaultValue, ADescription, AParamName: string);
begin
  inherited Create;

  FInstance := AInstance;
  FPropertyName := APropertyName;
  FName := AName;
  FLongNames := ALongNames;
  FShortLongForm := ShortLongForm;
  FSwitchType := ASwitchType;
  FPosition := APosition;
  FOptions := AOptions;
  FDefaultValue := ADefaultValue;
  FDescription := ADescription;
  FParamDesc := AParamName;
end;

function TSwitchData.AppendValue(const AValue, ADelim: string; const ADoQuote: Boolean): Boolean;
var
  LStringValue: string;
begin
  LStringValue := GetValue;

  if LStringValue <> '' then
    LStringValue := LStringValue + ADelim;

  if ADoQuote then
    LStringValue := LStringValue + Quote(AValue)
  else
    LStringValue := LStringValue + AValue;

  Result := SetValue(LStringValue);
end;

procedure TSwitchData.Enable;
var
  LContext: TRttiContext;
  LRtttiType: TRttiType;
  LProperty: TRttiProperty;
begin
  if SwitchType <> stBoolean then
    raise Exception.Create('TSwitchData.Enable: Not supported');

  LContext := TRttiContext.Create;
  LRtttiType := LContext.GetType(FInstance.ClassType);
  LProperty := LRtttiType.GetProperty(FPropertyName);

  LProperty.SetValue(FInstance, True);
  FProvided := True;
end;

function TSwitchData.GetValue: string;
var
  LContext: TRttiContext;
  LRtttiType: TRttiType;
  LProperty: TRttiProperty;
begin
  LContext := TRttiContext.Create;
  LRtttiType := LContext.GetType(FInstance.ClassType);
  LProperty := LRtttiType.GetProperty(FPropertyName);;

  if SwitchType = stEnumeration then
  begin
    // TODO: Untested
    var LEnumValue: Integer := GetEnumValue(LProperty.PropertyType.Handle, FPropertyName);

    if LEnumValue <> -1 then
    begin
      var LValue := TValue.FromOrdinal(LProperty.PropertyType.Handle, LEnumValue);

      Result := LProperty.GetValue(FInstance).AsString
    end
    else
      Result := '';
  end
  else if SwitchType = stStringDynArray then
  begin
    var LArray: TArray<string> := LProperty.GetValue(FInstance).AsType<TArray<string>>;

    Result := Result.Join(';', LArray);
  end
  else
    Result := LProperty.GetValue(FInstance).AsString;
end;

function TSwitchData.GetValueOrDefault: string;
begin
  Result := GetValue;

  if Result.IsEmpty then
    Result := DefaultValue;
end;

function TSwitchData.Quote(const AValue: string): string;
begin
  if (Pos(' ', AValue) > 0) or (Pos('"', AValue) > 0) then
    Result := '"' + StringReplace(AValue, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := AValue;
end;

function TSwitchData.SetValue(const AValue: string): Boolean;
var
  LCode: Integer;
  LIntegerValue: Integer;
  LContext: TRttiContext;
  LRtttiType: TRttiType;
  LProperty: TRttiProperty;
begin
  Result := True;

  LContext := TRttiContext.Create;
  LRtttiType := LContext.GetType(FInstance.ClassType);
  LProperty := LRtttiType.GetProperty(FPropertyName);

  case SwitchType of
    stString: LProperty.SetValue(FInstance, AValue);
    stInteger:
      begin
        Val(AValue, LIntegerValue, LCode);

        if LCode <> 0 then
          Exit(False);

        LProperty.SetValue(FInstance, LIntegerValue);
      end;
    stBoolean:
      begin
        if TryStrToBool(AValue, Result) then
          LProperty.SetValue(FInstance, Result);
      end;
    stEnumeration:
      begin
        if LProperty.IsWritable then
        begin
          var LEnumValue: Integer := GetEnumValue(LProperty.PropertyType.Handle, AValue);

          if LEnumValue <> -1 then
          begin
            var LValue := TValue.FromOrdinal(LProperty.PropertyType.Handle, LEnumValue);

            LProperty.SetValue(FInstance, LValue)
          end
          else
            raise Exception.Create('TSwitchData.SetValue: Unsupported value "' + AValue.QuotedString('"') +
              ' for ' + FPropertyName);
        end;
      end;
    stStringDynArray:
      begin
        var LStringArray := SplitArray(AValue);

        var LValue := TValue.From<TArray<string>>(LStringArray);

        LProperty.SetValue(FInstance, LValue);
      end;
    else
      raise Exception.Create('TSwitchData.SetValue: Unknown or unsupported SwitchType: ' + TEnumConverter.EnumToString(SwitchType));
  end;

  FProvided := True;
end;

{ TCommandLineParser }

constructor TCommandLineParser.Create;
begin
  inherited Create;

  FSwitchList := TObjectList<TSwitchData>.Create;
  FSwitchComparer := TIStringComparer.Ordinal; //don't destroy, Ordinal returns a global singleton
  FSwitchDict := TDictionary<string,TSwitchData>.Create(FSwitchComparer);
end;

destructor TCommandLineParser.Destroy;
begin
  FreeAndNil(FSwitchDict);
  FreeAndNil(FSwitchList);

  inherited Destroy;
end;

procedure TCommandLineParser.AddSwitch(const AInstance: TObject; const APropertyName, AName: string;
  const ALongNames: TCLPLongNames; const ASwitchType: TCLPSwitchType; const APosition: Integer;
  const AOptions: TCLPSwitchOptions; const ADefaultValue, ADescription, AParamName: string);
var
  LSwitchData: TSwitchData;
  LIndex: Integer;
  LLongName: TCLPLongName;
begin
  LSwitchData := TSwitchData.Create(AInstance, APropertyName, AName, ALongNames, ASwitchType,
    APosition, AOptions, ADefaultValue, ADescription, AParamName);

  FSwitchList.Add(LSwitchData);

  if AName <> '' then
    FSwitchDict.Add(AName, LSwitchData);

  for LLongName in ALongNames do
  begin
    FSwitchDict.Add(LLongName.LongForm, LSwitchData);

    if LLongName.ShortForm <> '' then
      for LIndex := Length(LLongName.ShortForm) to Length(LLongName.LongForm) - 1 do
        FSwitchDict.AddOrSetValue(Copy(LLongName.LongForm, 1, LIndex), LSwitchData);
  end;
end;

///  <summary>
///    Verifies attribute consistency.
///
///   For positional attributes there must be no 'holes' (i.e. positional attributes must
///   be numbere 1,2,...N) and there must be no 'required' attributes after 'optional'
///   attributes.
///
///   There must be at most one 'Rest' positional attribute.
///
///   Each switch attribute must have a long or short name. (That is actually enforced in
///   in the current implementation as long name is set to property name by default,
///   but the test is still left in for future proofing.)
///
///   Short names (when provided) must be one letter long.
///
///   At the same time creates an array of references to positional attributes,
///   FPositionals.
///  </summary>
function TCommandLineParser.CheckAttributes: Boolean;
var
  LSwitchData: TSwitchData;
  LHasOptional: Boolean;
  LHighPos: Integer;
  LIndex: Integer;
  LLongName: TCLPLongName;
  LPositionRest: TSwitchData;
begin
  Result := True;

  LHighPos := 0;
  LHasOptional := False;
  LPositionRest := nil;

  for LSwitchData in FSwitchList do
    if soPositional in LSwitchData.Options then
    begin
      if soPositionRest in LSwitchData.Options then
      begin
        if Assigned(LPositionRest) then
          Exit(SetError(ekPositionalsBadlyDefined, edExtraCLPPositionRest, SOnlyOneCLPPositionRestPropertyIs, 0, LSwitchData.PropertyName))
        else if LSwitchData.SwitchType <> stString then
          Exit(SetError(ekPositionalsBadlyDefined, edCLPPositionRestNotString, STypeOfACLPPositionRestPropertyMu, 0, LSwitchData.PropertyName))
        else
          LPositionRest := LSwitchData;
      end
      else
      begin
        if LSwitchData.Position <= 0 then
          Exit(SetError(ekPositionalsBadlyDefined, edPositionNotPositive, SPositionMustBeGreaterOrEqualTo1, LSwitchData.Position))
        else if LSwitchData.Position > LHighPos then
          LHighPos := LSwitchData.Position;
      end;

      if not (soRequired in LSwitchData.Options) then
        LHasOptional := False
      else if LHasOptional then
        Exit(SetError(ekPositionalsBadlyDefined, edRequiredAfterOptional, SRequiredPositionalParametersMust, LSwitchData.Position));
    end;

  if Assigned(LPositionRest) then
  begin
    Inc(LHighPos);
    LPositionRest.Position := LHighPos;
  end;

  if LHighPos = 0 then
    Exit(True);

  SetLength(FPositionals, LHighPos);
  for LIndex := Low(FPositionals) to High(FPositionals) do
    FPositionals[LIndex] := nil;

  for LSwitchData in FSwitchList do
    if soPositional in LSwitchData.Options then
      FPositionals[LSwitchData.Position - 1] := LSwitchData;

  for LIndex := Low(FPositionals) to High(FPositionals) do
    if FPositionals[LIndex] = nil then
      Exit(SetError(ekPositionalsBadlyDefined, edMissingPositionalDefinition, SMissingPositionalParameterDefini, LIndex + 1));

  for LSwitchData in FSwitchList do
    if not (soPositional in LSwitchData.Options) then
      if (LSwitchData.Name = '') and (Length(LSwitchData.LongNames) = 0) then
        Exit(SetError(ekNameNotDefined, edMissingNameForProperty, SMissingNameForProperty, 0, LSwitchData.PropertyName))
      else if (LSwitchData.Name <> '') and (Length(LSwitchData.Name) <> 1) then
        Exit(SetError(ekShortNameTooLong, edShortNameTooLong, SShortNameMustBeOneLetterLong, 0, LSwitchData.Name))
      else for LLongName in LSwitchData.LongNames do
        if (LLongName.ShortForm <> '') and (not StartsText(LLongName.ShortForm, LLongName.LongForm)) then
          Exit(SetError(ekLongFormsDontMatch, edLongFormsDontMatch, SLongFormsDontMatch, 0, LLongName.LongForm));
end;

function TCommandLineParser.FileSystemCheck(const ASwitchData: TSwitchData): TCLPSwitchOption;
var
  LStringValue: string;
  LStringArray: TArray<string>;
begin
  Result := soUnknown;

  LStringValue := ASwitchData.GetValueOrDefault;
  LStringArray := SplitArray(LStringValue);

  if soFileMustExist in ASwitchData.Options then
  begin
    for var LFileName in LStringArray do
      if not FileExists(LFileName) then
        Exit(soFileMustExist);
  end
  else if soDirectoryMustExist in ASwitchData.Options then
  begin
    for var LDirectory in LStringArray do
      if not DirectoryExists(LDirectory) then
        Exit(soDirectoryMustExist);
  end;
end;

function TCommandLineParser.FindExtendableSwitch(const ASwitchName: string; var AParam: string; var AData: TSwitchData): Boolean;
var
  kv: TPair<string, TSwitchData>;
begin
  Result := False;

  for kv in FSwitchDict do
  begin
    if ASwitchName.StartsWith(kv.Key, True) then
    begin
      AData := kv.Value;
      AParam := ASwitchName;
      Delete(AParam, 1, Length(kv.Key));

      Exit(True);
    end;
  end;
end;

function TCommandLineParser.GetCommandLine: string;
var
  LIndex: Integer;
begin
  Result := '';

  for LIndex := 1 to ParamCount do
  begin
    if LIndex > 1 then
      Result := Result + ' ';

    if Pos(' ', ParamStr(LIndex)) > 0 then
      Result := Result + '"' + ParamStr(LIndex) + '"'
    else
      Result := Result + ParamStr(LIndex);
  end;
end;

function TCommandLineParser.GetErrorInfo: TCLPErrorInfo;
begin
  Result := FErrorInfo;
end;

function TCommandLineParser.GetExtension(const ASwitch: string): string;
var
  LSwitchData: TSwitchData;
begin
  if not FSwitchDict.TryGetValue(ASwitch, LSwitchData) then
    raise Exception.CreateFmt('Switch %s is not defined', [ASwitch]);

  if not (soExtendable in LSwitchData.Options) then
    raise Exception.CreateFmt('Switch %s is not extendable', [ASwitch]);

  Result := LSwitchData.ParamValue;
end;

function TCommandLineParser.GetOptions: TCLPOptions;
begin
  Result := FOptions;
end;

function TCommandLineParser.GrabNextElement(var s, el: string): Boolean;
var
  LPosition: Integer;
begin
  el := '';
  s := TrimLeft(s);

  if s = '' then
    Exit(False);

  if s[1] = '"' then
  begin
    repeat
      LPosition := PosEx('"', s, 2);

      if LPosition <= 0 then //unterminated quote
        LPosition := Length(s);

      el := el + Copy(s, 1, LPosition);
      Delete(s, 1, LPosition);
    until (s = '') or (s[1] <> '"');

    Delete(el, 1, 1);

    if el[Length(el)] = '"' then
      Delete(el, Length(el), 1);

    el := StringReplace(el, '""', '"', [rfReplaceAll]);
  end
  else 
  begin
    LPosition := Pos(' ', s);

    if LPosition <= 0 then //last element
      LPosition := Length(s) + 1;

    el := Copy(s, 1, LPosition - 1);

    Delete(s, 1, LPosition);
  end;

  Result := True;
end; { TCommandLineParser.GrabNextElement }

function TCommandLineParser.IsSwitch(const ASwitchRawValue: string; var AParam: string; var AData: TSwitchData): Boolean;
var
  LDelimPos: Integer;
  LMinPos: Integer;
  LName: string;
  LPd: Char;
  LSd: string;
  LSwitchRawValue: string;
begin
  Result := False;
  AParam := '';

  LSwitchRawValue := ASwitchRawValue;
  for LSd in FSwitchDelims do
    if StartsStr(LSd, LSwitchRawValue) then
    begin
      LSwitchRawValue := ASwitchRawValue;

      Delete(LSwitchRawValue, 1, Length(LSd));

      if LSwitchRawValue <> '' then
        Result := True;

      Break;
    end;

  if Result then //try to extract parameter data
  begin
    LName := LSwitchRawValue;
    LMinPos := 0;

    for LPd in FParamDelims do
    begin
      LDelimPos := Pos(LPd, LName);

      if (LDelimPos > 0) and ((LMinPos = 0) or (LDelimPos < LMinPos)) then
        LMinPos := LDelimPos;
    end;

    if LMinPos > 0 then
    begin
      AParam := LName;
      Delete(AParam, 1, LMinPos);
      LName := Copy(LName, 1, LMinPos - 1);
    end;

    FSwitchDict.TryGetValue(LName, AData);

    if not Assigned(AData) then //try extendable switches
      FindExtendableSwitch(LName, AParam, AData);

    if not Assigned(AData) then //try short name
    begin
      if FSwitchDict.TryGetValue(LSwitchRawValue[1], AData) then
      begin
        AParam := LSwitchRawValue;
        Delete(AParam, 1, 1);

        if (AParam <> '') and (AData.SwitchType = stBoolean) then //misdetection, Boolean switch cannot accept data
          AData := nil;
      end;
    end;
  end;
end;

function TCommandLineParser.MapPropertyType(const AProp: TRttiProperty; const APropertyRttiType: TRttiType): TCLPSwitchType;

  procedure RaiseUnsuppoortedPropertyType(const AProp: TRttiProperty; var AResult: TCLPSwitchType);
  begin
    AResult := stString; // Just something to get rid of COmpiler warnign

    raise Exception.CreateFmt(SUnsupportedPropertyType, [AProp.Name]);
  end;

begin
  case AProp.PropertyType.TypeKind of
    tkInteger, tkInt64:
      Result := stInteger;
    tkEnumeration:
      if AProp.PropertyType.Handle = TypeInfo(Boolean) then
        Result := stBoolean
      else
        Result := stEnumeration;
    tkString, tkLString, tkWString, tkUString:
      Result := stString;
    tkDynArray:
      begin
        if APropertyRttiType.TypeKind = tkChar then
          Result := stStringDynArray
        else
          RaiseUnsuppoortedPropertyType(AProp, Result);
      end
    else
      RaiseUnsuppoortedPropertyType(AProp, Result);
  end;
end;

function TCommandLineParser.Parse(const ACommandLine: string; const ACommandData: TObject): Boolean;
begin
  FSwitchDict.Clear;
  SetLength(FPositionals, 0);
  FSwitchList.Clear;

  ProcessDefinitionClass(ACommandData);

  Result := CheckAttributes;

  if not Result then
    raise ECLPConfigurationError.Create(ErrorInfo)
  else
    Result := ProcessCommandLine(ACommandData, ACommandLine);
end;

function TCommandLineParser.Parse(const ACommandData: TObject): Boolean;
begin
  Result := Parse(GetCommandLine, ACommandData);
end;

procedure TCommandLineParser.ProcessAttributes(const AInstance: TObject; const AProp: TRttiProperty;
  const APropertyRttiType: TRttiType);

  procedure AddLongName(const ALongForm, AShortForm: string; var ALongNames: TCLPLongNames);
  begin
    SetLength(ALongNames, Length(ALongNames) + 1);

    ALongNames[High(ALongNames)] := TCLPLongName.Create(ALongForm, AShortForm);
  end;

var
  LAttribute: TCustomAttribute;
  LDefault: string;
  LDescription: string;
  LLongNames: TCLPLongNames;
  LNName: string;
  LOptions: TCLPSwitchOptions;
  LParamName: string;
  LPosition: Integer;
begin
  LNName := '';
  LDescription := '';
  LParamName := CLPDescriptionAttribute.DefaultValue;
  LOptions := [];
  LPosition := 0;

  SetLength(LLongNames, 0);

  for LAttribute in AProp.GetAttributes do
  begin
    if LAttribute is CLPNameAttribute then
      LNName := CLPNameAttribute(LAttribute).Name
    else if LAttribute is CLPLongNameAttribute then
      AddLongName(CLPLongNameAttribute(LAttribute).LongName, CLPLongNameAttribute(LAttribute).ShortForm, LLongNames)
    else if LAttribute is CLPDefaultAttribute then
      LDefault := CLPDefaultAttribute(LAttribute).DefaultValue
    else if LAttribute is CLPDescriptionAttribute then
    begin
      LDescription := CLPDescriptionAttribute(LAttribute).Description;
      LParamName := CLPDescriptionAttribute(LAttribute).paramName;
    end
    else if LAttribute is CLPRequiredAttribute then
      Include(LOptions, soRequired)
    else if LAttribute is CLPFileMustExistAttribute then
      Include(LOptions, soFileMustExist)
    else if LAttribute is CLPDirectoryMustExistAttribute then
      Include(LOptions, soDirectoryMustExist)
    else if LAttribute is CLPExtendableAttribute then
      Include(LOptions, soExtendable)
    else if LAttribute is CLPPositionAttribute then
    begin
      LPosition := CLPPositionAttribute(LAttribute).Position;
      Include(LOptions, soPositional);
    end
    else if LAttribute is CLPPositionRestAttribute then
    begin
      Include(LOptions, soPositional);
      Include(LOptions, soPositionRest);
    end;
  end; //for attr

  if (Length(LLongNames) = 0) and (not SameText(AProp.Name, Trim(LNName))) then
    AddLongName(AProp.Name, '', LLongNames);

  AddSwitch(AInstance, AProp.Name, Trim(LNName), LLongNames, MapPropertyType(AProp, APropertyRttiType), LPosition,
    LOptions, LDefault, Trim(LDescription), Trim(LParamName));
end;

function TCommandLineParser.ProcessCommandLine(const ACommandData: TObject; const ACommandLine: string): Boolean;
var
  LSwitchData: TSwitchData;
  LCommandLine: string;
  LCurrentRawParameter: string;
  LParamValue: string; // -random:ThisParamValueToTheVariable
  LPosition: Integer;
begin
  Result := True;

  for LSwitchData in FSwitchList do
    if LSwitchData.DefaultValue <> '' then
      LSwitchData.SetValue(LSwitchData.DefaultValue);

  LPosition := 1;
  LCommandLine := ACommandLine;

  while GrabNextElement(LCommandLine, LCurrentRawParameter) do
  begin
    if IsSwitch(LCurrentRawParameter, LParamValue, LSwitchData) then
    begin
      if not Assigned(LSwitchData) then
        if opIgnoreUnknownSwitches in FOptions then
          Continue //while
        else
          Exit(SetError(ekUnknownNamed, edUnknownSwitch, SUnknownSwitch, 0, LCurrentRawParameter));

      if LSwitchData.SwitchType = stBoolean then
      begin
        if (LParamValue = '') or (soExtendable in LSwitchData.Options) then
        begin
          LSwitchData.Enable;

          if LParamValue <> '' then
            LSwitchData.ParamValue := LParamValue;
        end
        else
        begin
          var BoolValue: Boolean;

          if TryStrToBool(LParamValue, BoolValue) then
            LSwitchData.SetValue(LParamValue)
          else
            Exit(SetError(ekInvalidData, edBooleanWithData, SBooleanSwitchCannotAcceptData, 0, LCurrentRawParameter));
        end;
      end
      else if LParamValue <> '' then
        if not LSwitchData.SetValue(LParamValue) then
          Exit(SetError(ekInvalidData, edInvalidDataForSwitch, SInvalidDataForSwitch, 0, LCurrentRawParameter));
    end
    else
    begin
      if (LPosition - 1) > High(FPositionals) then
        Exit(SetError(ekExtraPositional, edTooManyPositionalArguments, STooManyPositionalArguments, 0, LCurrentRawParameter));

      LSwitchData := FPositionals[LPosition - 1];

      if soPositionRest in LSwitchData.Options then
      begin
        if not LSwitchData.AppendValue(LCurrentRawParameter, #13, False) then
          Exit(SetError(ekInvalidData, edInvalidDataForSwitch, SInvalidDataForSwitch, 0, LCurrentRawParameter));
      end
      else
      begin
        if not LSwitchData.SetValue(LCurrentRawParameter) then
          Exit(SetError(ekInvalidData, edInvalidDataForSwitch, SInvalidDataForSwitch, 0, LCurrentRawParameter));

        Inc(LPosition);
      end;
    end;
  end; //while s <> ''

  for LSwitchData in FPositionals do
  begin
    if (soRequired in LSwitchData.Options) and (not LSwitchData.Provided) then
      Exit(SetError(ekMissingPositional, edMissingRequiredParameter, SRequiredParameterWasNotProvided, LSwitchData.Position, LSwitchData.LongNames[0].LongForm))
    else if (soFileMustExist in LSwitchData.Options) or (soDirectoryMustExist in LSwitchData.Options) then
    begin
      if LSwitchData.SwitchType in [stString, stStringDynArray] then
      begin
        case FileSystemCheck(LSwitchData) of
          soFileMustExist: Exit(SetError(ekFileDoNotExist, edFileDoNotExist, SFileDoNotExist, LSwitchData.Position, LSwitchData.LongNames[0].LongForm));
          soDirectoryMustExist: Exit(SetError(ekDirectoryDoNotExist, edDirectoryDoNotExist, SDirectoryDoNotExist, LSwitchData.Position, LSwitchData.LongNames[0].LongForm));
        end;
      end
      else
        Exit(SetError(ekWronDataTypeForFileOrDirectoryMustExist, edWrongDataTypeForFileOrDirectoryMustExist, SWrongDataTypeForFileOrDirectoryMustExist, LSwitchData.Position, LSwitchData.LongNames[0].LongForm));
    end;
  end;

  for LSwitchData in FSwitchlist do
  begin
    if (soRequired in LSwitchData.Options) and (not LSwitchData.Provided) then
      Exit(SetError(ekMissingNamed, edMissingRequiredSwitch, SRequiredSwitchWasNotProvided, 0, LSwitchData.LongNames[0].LongForm))
    else if (soFileMustExist in LSwitchData.Options) or (soDirectoryMustExist in LSwitchData.Options) then
    begin
      if LSwitchData.SwitchType in [stString, stStringDynArray] then
      begin
        case FileSystemCheck(LSwitchData) of
          soFileMustExist: Exit(SetError(ekFileDoNotExist, edFileDoNotExist, SFileDoNotExist, LSwitchData.Position, LSwitchData.LongNames[0].LongForm));
          soDirectoryMustExist: Exit(SetError(ekDirectoryDoNotExist, edDirectoryDoNotExist, SDirectoryDoNotExist, LSwitchData.Position, LSwitchData.LongNames[0].LongForm));
        end;
      end
      else
        Exit(SetError(ekWronDataTypeForFileOrDirectoryMustExist, edWrongDataTypeForFileOrDirectoryMustExist, SWrongDataTypeForFileOrDirectoryMustExist, LSwitchData.Position, LSwitchData.LongNames[0].LongForm));
    end;
  end;
end;

procedure TCommandLineParser.ProcessDefinitionClass(const ACommandData: TObject);
var
  LContext: TRttiContext;
  LProperty: TRttiProperty;
  LRttiType: TRttiType;
begin
  LContext := TRttiContext.Create;
  LRttiType := LContext.GetType(ACommandData.ClassType);

  for LProperty in LRttiType.GetProperties do
    if LProperty.Parent = LRttiType then
      ProcessAttributes(ACommandData, LProperty, LContext.GetType(LProperty.Handle));
end;

function TCommandLineParser.SetError(const AKind: TCLPErrorKind; const ADetail: TCLPErrorDetailed;
  const AText: string; const APosition: Integer; const ASwitchName: string): Boolean;
begin
  FErrorInfo.Kind := AKind;
  FErrorInfo.Detailed := ADetail;
  FErrorInfo.Text := AText;
  FErrorInfo.Position := APosition;
  FErrorInfo.SwitchName := ASwitchName;
  FErrorInfo.IsError := True;

  Result := False;
end;

procedure TCommandLineParser.SetOptions(const AValue: TCLPOptions);
begin
  FOptions := AValue;
end;

function TCommandLineParser.Usage(const AWrapAtColumn: Integer): TArray<string>;
var
  LFormatter: TUsageFormatter;
begin
  LFormatter := TUsageFormatter.Create;
  try
    LFormatter.Usage(Self, AWrapAtColumn, Result);
  finally
    FreeAndNil(LFormatter);
  end;
end;

{ TUsageFormatter }

function TUsageFormatter.AddParameter(const AName, APrefix, ADelim: string; const AData: TSwitchData): string;
begin
  Result := AName + ADelim + AData.ParamName;

  if not APrefix.IsEmpty then
    Result := APrefix + Result;
end;

procedure TUsageFormatter.AlignAndWrap(const ASl: TStringList; const AWrapAtColumn: Integer);
var
  LIndex: Integer;
  LMaxPos: Integer;
  LPosDel: Integer;
  LStringValue: string;
begin
  LMaxPos := 0;

  for LStringValue in ASl do
  begin
    LPosDel := Pos(' ' + FSwitchDelims[0], LStringValue);

    if LPosDel > LMaxPos then
      LMaxPos := LPosDel;
  end;

  LIndex := 0;

  while LIndex < ASl.Count do
  begin
    LStringValue := ASl[LIndex];
    LPosDel := Pos(' ' + FSwitchDelims[0], LStringValue);

    if (LPosDel > 0) and (LPosDel < LMaxPos) then
    begin
      Insert(StringOfChar(' ', LMaxPos - LPosDel), LStringValue, LPosDel);
      ASl[LIndex] := LStringValue;
    end;

    if Length(LStringValue) >= AWrapAtColumn then
    begin
      LPosDel := LastSpaceBefore(LStringValue, AWrapAtColumn);

      if LPosDel > 0 then
      begin
        ASl.Insert(LIndex + 1, StringOfChar(' ', LMaxPos + 2) + Copy(LStringValue, LPosDel + 1, Length(LStringValue) - LPosDel));
        ASl[LIndex] := Copy(LStringValue, 1, LPosDel - 1);
        Inc(LIndex);
      end;
    end;

    Inc(LIndex);
  end;
end;

function TUsageFormatter.GetCommandLinePrototype(const APositionalParams: TArray<TSwitchData>; const ASwitchList: TObjectList<TSwitchData>): string;

  function GetName(const ASwitchData: TSwitchData): string;
  begin
    if (Length(ASwitchData.LongNames) >= 1) and not ASwitchData.LongNames[0].LongForm.IsEmpty then
      Result := ASwitchData.LongNames[0].LongForm
    else
      Result := ASwitchData.PropertyName;
  end;

var
  LSwitchData: TSwitchData;
begin
  Result := '';

  for LSwitchData in APositionalParams do
  begin
    // TODO: Have not tested this, might not be too pretty
    Result := Result + GetPositionalSwitchName(LSwitchData);
    Result := Result + ' ';
  end;

  for LSwitchData in ASwitchList do
  begin
    if not (soRequired in LSwitchData.Options) then
      Result := Result + '[';

    Result := Result + FSwitchDelims[0] + GetName(LSwitchData);

    if not LSwitchData.ParamName.IsEmpty then
      Result := Result + FParamDelims[0] + LSwitchData.ParamName;

    if not (soRequired in LSwitchData.Options) then
      Result := Result + ']';

    Result := Result + ' ';
  end;
end;

function TUsageFormatter.GetPositionalSwitchName(const ASwitchData: TSwitchData): string;
begin
  if ASwitchData.Name <> '' then
    Result := ASwitchData.Name
  else if Length(ASwitchData.LongNames) <> 0 then
    Result := ASwitchData.LongNames[0].LongForm
  else
    Result := IntToStr(ASwitchData.Position);
end;

function TUsageFormatter.LastSpaceBefore(const s: string; const AStartPos: Integer): Integer;
begin
  Result := AStartPos - 1;

  while (Result > 0) and (s[Result] <> ' ') do
    Dec(Result);
end;

procedure TUsageFormatter.Usage(const AParser: TCommandLineParser; const AWrapAtColumn: Integer;
  var AUsageList: TArray<string>);
var
  LAddedOptions: Boolean;
  LCommandLine: string;
  LSwitchData: TSwitchData;
  LHelp: TStringList;
  LLongName: TCLPLongName;
  LName: string;
  LName2: string;
begin
  LHelp := TStringList.Create;
  try
    LCommandLine := ExtractFileName(ParamStr(0));

    for LSwitchData in AParser.Positionals do
    begin
      if not Assigned(LSwitchData) then //error in definition class
        LHelp.Add('*** missing ***')
      else
      begin
        LNAme := GetPositionalSwitchName(LSwitchData);

        LCommandLine := LCommandLine + ' ' + Wrap(LName, LSwitchData);
        LHelp.Add(Format('%s - %s', [Wrap(LName, LSwitchData), LSwitchData.Description]));
      end;
    end; //for data in FPositionals

    LAddedOptions := False;

    for LSwitchData in AParser.SwitchList do
    begin
      if not (soPositional in LSwitchData.Options) then
      begin
        if not LAddedOptions then
        begin
          LCommandLine := LCommandLine + ' ' + SOptions;
          LAddedOptions := True;
        end;

        LName := '';

        if LSwitchData.Name <> '' then
          LName := Wrap(AddParameter(LSwitchData.Name, '-', '', LSwitchData), LSwitchData);

        for LLongName in LSwitchData.LongNames do
        begin
          LName2 := Wrap(AddParameter(LLongName.LongForm, '-', ':', LSwitchData), LSwitchData);

          if LName <> '' then
            LName := LName + ', ';

          LName := LName + LName2;
        end;

        LName := LName + ' - ' + LSwitchData.Description;

        if LSwitchData.DefaultValue <> '' then
          LName := LName + SDefault + LSwitchData.DefaultValue;

        LHelp.Add(LName);
      end;
    end;

    if AWrapAtColumn > 0 then
      AlignAndWrap(LHelp, AWrapAtColumn);

    LHelp.Insert(0, LCommandLine);
    LHelp.Insert(1, '  ' + GetCommandLinePrototype(AParser.Positionals, AParser.SwitchList));
    LHelp.Insert(2, '');

    AUsageList := LHelp.ToStringArray;
  finally
    FreeAndNil(LHelp);
  end;
end;

function TUsageFormatter.Wrap(const AName: string; const AData: TSwitchData): string;
begin
  if not (soRequired in AData.Options) then
    Result := '[' + AName + ']'
  else if soPositional in AData.Options then
    Result := '<' + AName + '>'
  else
    Result := AName;
end;

{ ECLPConfigurationError }

constructor ECLPConfigurationError.Create(const AErrInfo: TCLPErrorInfo);
begin
  inherited Create(AErrInfo.Text);

  FErrorInfo := AErrInfo;
end;

end.

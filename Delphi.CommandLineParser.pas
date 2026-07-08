unit Delphi.CommandLineParser;

{
  Check https://github.com/TommiPrami/Delphi.CommandLineParser/blob/main/README.md for more information
}
interface

uses
  System.RTTI, System.SysUtils, System.TypInfo;

type
  ///  <summary>
  ///    Specifies the short (one-letter) name for the switch.
  ///  </summary>
  CLPNameAttribute = class(TCustomAttribute)
  strict private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  ///  <summary>
  ///    Specifies the long name for the switch. If not set, the property name
  ///    is used as the long name.
  ///
  ///    An abbreviation of the long name can also be provided which must match the
  ///    beginning of the long form. In this case the parser will accept shortened
  ///    versions of the long name, but no shorter than the abbreviation.
  ///    Example: if 'longName' = 'autotest' and 'abbreviation' = 'auto' then the parser
  ///    will accept 'auto', 'autot', 'autote', 'autotes' and 'autotest', but not 'aut',
  ///    'au' or 'a'.
  ///
  ///    Multiple long names (alternate switches) can be provided for one entity.
  ///  </summary>
  CLPLongNameAttribute = class(TCustomAttribute)
  strict private
    FLongName : string;
    FAbbreviation: string;
  public
    constructor Create(const ALongName: string; const AAbbreviation: string = '');
    property LongName: string read FLongName;
    property Abbreviation: string read FAbbreviation;
  end;

  ///  <summary>
  ///    Specifies the default value to use when the switch is not found on the command line.
  ///  </summary>
  CLPDefaultAttribute = class(TCustomAttribute)
  strict private
    FDefaultValue: string;
  public
    constructor Create(const AValue: string); overload;
    property DefaultValue: string read FDefaultValue;
  end;

  ///  <summary>
  ///    Provides a switch description, used by the usage function.
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
  ///    When present, checks that the given file exists.
  ///  </summary>
  CLPFileMustExistAttribute = class(TCustomAttribute)
  public
  end;

  ///  <summary>
  ///    When present, checks that the given directory exists.
  ///  </summary>
  CLPDirectoryMustExistAttribute = class(TCustomAttribute)
  public
  end;

  /// <summary>
  ///   When present, specifies that the switch name can be arbitrarily extended.
  ///   The code can access this extension via the GetExtension function.
  /// </summary>
  CLPExtendableAttribute = class(TCustomAttribute)
  public
  end;

  ///  <summary>
  ///    Specifies the position of a positional (unnamed) switch. The first
  ///    positional switch has position 1.
  ///  </summary>
  CLPPositionAttribute = class(TCustomAttribute)
  strict private
    FPosition: Integer;
  public
    constructor Create(const APosition: Integer);
    property Position: Integer read FPosition;
  end;

  ///  <summary>
  ///    Specifies the switch that will receive a #13-delimited list of all
  ///    positional parameters for which switch definitions don't exist.
  ///  </summary>
  CLPPositionRestAttribute = class(TCustomAttribute)
  public
  end;

  ///  <summary>
  ///    Marks a property to be ignored by the command-line parser. Useful for
  ///    helper properties on the definition class that should never be exposed
  ///    on the command line.
  ///  </summary>
  CLPSkipAttribute = class(TCustomAttribute)
  public
  end;

  ///  <summary>
  ///    Pre-fills the switch from an environment variable if the variable is
  ///    set and the switch is not provided on the command line. Precedence
  ///    (lowest to highest): CLPDefault, CLPEnvironment, command-line value.
  ///  </summary>
  CLPEnvironmentAttribute = class(TCustomAttribute)
  strict private
    FVariableName: string;
  public
    constructor Create(const AVariableName: string);
    property VariableName: string read FVariableName;
  end;

  ///  <summary>
  ///    Marks an enum identifier as illegal as command-line input. Useful for
  ///    sentinel values such as 'Uninitialized' that exist for internal state
  ///    but should never be selectable by the user. Stack the attribute to
  ///    mark multiple identifiers.
  ///  </summary>
  CLPIllegalValueAttribute = class(TCustomAttribute)
  strict private
    FValue: string;
  public
    constructor Create(const AValue: string);
    property Value: string read FValue;
  end;

  ///  <summary>
  ///    Restricts a numeric switch (Integer or Float/Date/Time/DateTime) to a
  ///    closed range. Bounds are inclusive. Use the constructor overload that
  ///    matches the underlying property type.
  ///  </summary>
  CLPRangeAttribute = class(TCustomAttribute)
  strict private
    FMinInt: Int64;
    FMaxInt: Int64;
    FMinFloat: Double;
    FMaxFloat: Double;
    FIsFloat: Boolean;
  public
    constructor Create(const AMin, AMax: Int64); overload;
    constructor Create(const AMin, AMax: Double); overload;
    property MinInt: Int64 read FMinInt;
    property MaxInt: Int64 read FMaxInt;
    property MinFloat: Double read FMinFloat;
    property MaxFloat: Double read FMaxFloat;
    property IsFloat: Boolean read FIsFloat;
  end;

  TCLPErrorKind = (
    // configuration error, will result in exception
    ekPositionalsBadlyDefined, ekNameNotDefined, ekShortNameTooLong, ekLongFormsDontMatch,
    ekInvalidDefault,
    // user data error, will result in error result
    ekMissingPositional, ekExtraPositional, ekMissingNamed, ekUnknownNamed, ekInvalidData,
    ekWrongDataTypeForFileOrDirectoryMustExist, ekFileDoesNotExist, ekDirectoryDoesNotExist,
    ekResponseFile);

  TCLPErrorDetailed = (
    edBooleanWithData,             // SBooleanSwitchCannotAcceptData
    edCLPPositionRestNotString,    // SPositionRestMustBeString
    edExtraCLPPositionRest,        // SOnlyOnePositionRestAllowed
    edInvalidDataForSwitch,        // SInvalidDataForSwitch
    edLongFormsDontMatch,          // SLongFormsDontMatch
    edMissingNameForProperty,      // SMissingNameForProperty
    edMissingPositionalDefinition, // SMissingPositionalDefinition
    edMissingRequiredSwitch,       // SRequiredSwitchWasNotProvided
    edPositionNotPositive,         // SPositionMustBeGreaterOrEqualTo1
    edRequiredAfterOptional,       // SRequiredAfterOptionalForbidden
    edShortNameTooLong,            // SShortNameMustBeOneLetterLong
    edTooManyPositionalArguments,  // STooManyPositionalArguments
    edUnknownSwitch,               // SUnknownSwitch
    edUnsupportedPropertyType,     // SUnsupportedPropertyType
    edMissingRequiredParameter,    // SRequiredParameterWasNotProvided
    edWrongDataTypeForFileOrDirectoryMustExist, // SWrongDataTypeForFileOrDirectoryMustExist
    edFileDoesNotExist,              // SFileDoesNotExist
    edDirectoryDoesNotExist,         // SDirectoryDoesNotExist
    edResponseFileMustBeSole,        // SResponseFileMustBeSole
    edResponseFileNotFound,          // SResponseFileNotFound
    edInvalidDefaultValue            // SInvalidDefaultValue
  );

  TCLPErrorInfo = record
    IsError: Boolean;
    Kind: TCLPErrorKind;
    Detailed: TCLPErrorDetailed;
    Position: Integer;
    SwitchName: string;
    Text: string;
  end;

  TCLPOption = (
    opIgnoreUnknownSwitches,
    /// <summary>
    ///   Recognize -h, -?, --help (and on Windows also /h, /?, /help) on the
    ///   command line. When seen, Parse stops processing further switches,
    ///   skips required-switch validation, returns True, and sets HelpRequested
    ///   to True. The caller can then render Usage() and exit. Nothing is
    ///   built in if this option is not set.
    ///
    ///   PRECEDENCE / COLLISION: these help tokens are intercepted BEFORE
    ///   normal switch lookup. If your definition class declares a switch whose
    ///   short name is 'h' (or a long name 'help'), that switch becomes
    ///   unreachable via '-h' / '--help' while this option is enabled — the
    ///   token is treated as a help request instead. Either don't enable this
    ///   option, or avoid naming switches 'h'/'help', if that conflict matters.
    /// </summary>
    opEnableBuiltInHelp);
  TCLPOptions = set of TCLPOption;

  ICommandLineParser = interface ['{C9B729D4-3706-46DB-A8A2-1E07E04F497B}']
    function GetErrorInfo: TCLPErrorInfo;
    function GetHelpRequested: Boolean;
    function GetOptions: TCLPOptions;
    procedure SetOptions(const AValue: TCLPOptions);
    function GetExtension(const ASwitch: string): string;
    function Usage(const AWrapAtColumn: Integer = 80): TArray<string>;
    function Parse(const ACommandData: TObject): Boolean; overload;
    function Parse(const ACommandLine: string; const ACommandData: TObject): Boolean; overload;
    property ErrorInfo: TCLPErrorInfo read GetErrorInfo;
    property HelpRequested: Boolean read GetHelpRequested;
    property Options: TCLPOptions read GetOptions write SetOptions;
  end;

  ECLPConfigurationError = class(Exception)
  strict private
    FErrorInfo: TCLPErrorInfo;
  public
    constructor Create(const AErrInfo: TCLPErrorInfo);
    property ErrorInfo: TCLPErrorInfo read FErrorInfo;
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

  // ---------------------------------------------------------------------------
  // Low-level, state-free text helpers used by the usage formatter. They are
  // exposed so they can be unit-tested in isolation; they have no dependency on
  // the parser or its switch metadata.
  // ---------------------------------------------------------------------------

  ///  <summary>
  ///    Returns the 1-based index of the last space character at or before
  ///    AStartPos, or 0 if there is none. Used to pick a word-break point.
  ///  </summary>
  function CLPLastSpaceBefore(const AValue: string; const AStartPos: Integer): Integer;

  ///  <summary>
  ///    Word-wraps a single logical line so that no physical line exceeds
  ///    AWrapAtColumn characters (when a break point exists). Continuation
  ///    lines are prefixed with AHangingIndent spaces. A word longer than the
  ///    available width is left intact on its own line rather than being split.
  ///    AWrapAtColumn <= 0 disables wrapping (returns the line unchanged).
  ///  </summary>
  function CLPWordWrapLine(const ALine: string; const AWrapAtColumn, AHangingIndent: Integer): TArray<string>;

  ///  <summary>
  ///    Lays out a two-column block. Every left cell is padded to the width of
  ///    the widest left cell, then ASeparator and the right cell are appended.
  ///    Each resulting row is word-wrapped at AWrapAtColumn with continuation
  ///    lines hanging-indented under the right column. When a right cell is
  ///    empty, only the (padded) left cell is emitted. ALeft and ARight must
  ///    have the same length.
  ///  </summary>
  function CLPFormatTwoColumnBlock(const ALeft, ARight: TArray<string>; const ASeparator: string;
    const AWrapAtColumn: Integer): TArray<string>;

  ///  <summary>
  ///    Packs ATokens onto as few lines as possible without exceeding
  ///    AWrapAtColumn, breaking only BETWEEN tokens (never inside one) and
  ///    indenting continuation lines by AContinuationIndent spaces. A token
  ///    wider than the column is emitted intact on its own line. AWrapAtColumn
  ///    &lt;= 0 places every token on a single line.
  ///  </summary>
  function CLPWrapTokens(const ATokens: TArray<string>; const AWrapAtColumn, AContinuationIndent: Integer): TArray<string>;

implementation

uses
  System.Character, System.Classes, System.DateUtils, System.Generics.Collections, System.Generics.Defaults, System.StrUtils;

resourcestring
  SBooleanSwitchCannotAcceptData     = 'Boolean switch cannot accept this data. Use True or False.';
  SDefault                           = ', default: ';
  SInvalidDataForSwitch              = 'Invalid data for switch.';
  SLongFormsDontMatch                = 'Short version of the long name must match the beginning of the long name.';
  SMissingNameForProperty            = 'Missing name for property.';
  SMissingPositionalDefinition       = 'Missing positional parameter definition.';
  SOnlyOnePositionRestAllowed        = 'Only one CLPPositionRest property is allowed.';
  SOptions                           = '[options]';
  SPositionMustBeGreaterOrEqualTo1   = 'Position must be greater than or equal to 1.';
  SRequiredParameterWasNotProvided   = 'Required parameter was not provided.';
  SRequiredAfterOptionalForbidden    = 'Required positional parameters must not appear after optional positional parameters.';
  SRequiredSwitchWasNotProvided      = 'Required switch was not provided.';
  SShortNameMustBeOneLetterLong      = 'Short name must be one letter long.';
  STooManyPositionalArguments        = 'Too many positional arguments.';
  SPositionRestMustBeString          = 'Type of a CLPPositionRest property must be a string.';
  SUnknownSwitch                     = 'Unknown switch.';
  SUnsupportedPropertyType           = 'Unsupported type for property %s.';
  SWrongDataTypeForFileOrDirectoryMustExist = 'Switch data type for a file or directory check must be a string.';
  SFileDoesNotExist                  = 'File must exist.';
  SDirectoryDoesNotExist             = 'Directory must exist.';
  SResponseFileMustBeSole            = 'A response file (@file) must be the only command-line argument; no other switches or @ tokens are allowed.';
  SResponseFileNotFound              = 'Response file does not exist.';
  SInvalidDefaultValue               = 'Default value is not valid for this switch.';

type
  // Usage: LEnumNameStr := TEnumConverter.EnumToString(FormMain.BorderStyle);
  TEnumConverter = class
  public
    class function EnumToInt<T>(const AEnumValue: T): Integer;
    class function EnumToString<T>(const AEnumValue: T; const AStripLowercasePrefix: Boolean = False): string;
  end;

  TCLPSwitchType = (stString, stInteger, stFloat, stBoolean, stEnumeration, stDate, stTime, stDateTime, stStringArray);

  TCLPSwitchOption = (soRequired, soPositional, soPositionRest, soExtendable, soFileMustExist, soDirectoryMustExist);
  TCLPSwitchOptions = set of TCLPSwitchOption;

  // Result of TCommandLineParser.FileSystemCheck.
  TFSCheckResult = (fscOk, fscFileMissing, fscDirectoryMissing);

  TCLPLongName = record
    LongForm: string;
    Abbreviation: string;
    constructor Create(const ALongForm, AAbbreviation: string);
  end; { TCLPLongName }

  TCLPLongNames = TArray<TCLPLongName>;

  TSwitchData = class
  strict private
    FDefaultValue: string;
    FDescription: string;
    FEnvironmentVariable: string;
    FHasIntRange: Boolean;
    FHasFloatRange: Boolean;
    FIllegalValues: TArray<string>;
    FInstance: TObject;
    FLongNames: TCLPLongNames;
    FMinInt: Int64;
    FMaxInt: Int64;
    FMinFloat: Double;
    FMaxFloat: Double;
    FName: string;
    FOptions: TCLPSwitchOptions;
    FParamName: string;
    FParamValue: string;
    FPosition: Integer;
    FPropertyName: string;
    FRttiContext: TRttiContext;
    FHasValue: Boolean;
    FSwitchType: TCLPSwitchType;
  strict protected
    function GetRttiProperty: TRttiProperty;
    function Quote(const AValue: string): string;
  public
    constructor Create(const AInstance: TObject; const APropertyName, AName: string; const ALongNames: TCLPLongNames;
      const ASwitchType: TCLPSwitchType; const APosition: Integer; const AOptions: TCLPSwitchOptions;
      const ADefaultValue, ADescription, AParamName, AEnvironmentVariable: string);
    function AppendValue(const AValue, ADelim: string; const ADoQuote: Boolean): Boolean;
    function DisplayName: string;
    function GetValue: string;
    function GetValueOrDefault: string;
    function HasIntRange: Boolean;
    function HasFloatRange: Boolean;
    function IsIllegalValue(const AValue: string): Boolean;
    function SetValue(const AValue: string): Boolean;
    procedure AddIllegalValue(const AValue: string);
    procedure SetBooleanTrue;
    procedure SetIntRange(const AMin, AMax: Int64);
    procedure SetFloatRange(const AMin, AMax: Double);
    property DefaultValue: string read FDefaultValue;
    property Description: string read FDescription;
    property EnvironmentVariable: string read FEnvironmentVariable;
    property LongNames: TCLPLongNames read FLongNames;
    property Name: string read FName;
    property Options: TCLPSwitchOptions read FOptions;
    property ParamName: string read FParamName;
    property ParamValue: string read FParamValue write FParamValue;
    property Position: Integer read FPosition write FPosition;
    property PropertyName: string read FPropertyName;
    /// <summary>
    ///   True once the switch has received a value from ANY source: a default,
    ///   an environment variable, or the command line. (It is NOT limited to
    ///   "the user typed it on the command line".) The required-switch check
    ///   uses this, which is why a required switch that also has a default is
    ///   considered satisfied even when absent from the command line.
    /// </summary>
    property HasValue: Boolean read FHasValue;
    property SwitchType: TCLPSwitchType read FSwitchType;
  end;

const
  {$IFDEF MSWINDOWS}
    SWITCH_DELIMITERS: array [0..2] of string = ('--', '-', '/');
  {$ELSE}
    SWITCH_DELIMITERS: array [0..1] of string = ('--', '-');
  {$ENDIF}
    PARAM_DELIMITERS : array [0..1] of Char = (':', '=');

  // Canonical prefixes used when RENDERING usage. Intentionally independent of
  // SWITCH_DELIMITERS (which governs PARSING and whose order changed when '--' was
  // promoted to first-tried). Short = single dash, long = double dash.
  USAGE_SHORT_PREFIX = '-';
  USAGE_PARAM_DELIMITER = ':';   // matches a real, parseable separator
  USAGE_DESC_DELIMITER = ' - '; // between the names column and the description (legacy two-column layout)

  // Stacked usage detail layout (switch label / description / default) plus the
  // wrapped prototype. The numeric values are indent widths, in spaces.
  USAGE_DEFAULT_LABEL      = 'default: ';
  USAGE_DESC_PREFIX        = '- ';
  USAGE_PROTO_CONT_INDENT  = 2; // prototype continuation lines
  USAGE_SWITCH_INDENT      = 2; // the switch label line
  USAGE_DESC_INDENT        = 4; // spaces before the '- ' description marker
  USAGE_DETAIL_CONT_INDENT = 6; // description continuation + default line (= USAGE_DESC_INDENT + Length('- '))

type
  TCommandLineParser = class(TInterfacedObject, ICommandLineParser)
  strict private
    FErrorInfo: TCLPErrorInfo;
    FHelpRequested: Boolean;
    FOptions: TCLPOptions;
    FPositionals: TArray<TSwitchData>;
    FSwitchComparer: TStringComparer;
    FSwitchDict: TDictionary<string, TSwitchData>;
    FSwitchList: TObjectList<TSwitchData>;
  strict protected
    function CheckAttributes: Boolean;
    function ExpandResponseFile(var ACommandLine: string): Boolean;
    function FileSystemCheck(const ASwitchData: TSwitchData; var AFileOrDirectoryNotFound: string): TFSCheckResult;
    function FindExtendableSwitch(const ASwitchName: string; var AParam: string; var AData: TSwitchData): Boolean;
    function BuildCommandLineFromParams: string;
    function GetErrorInfo: TCLPErrorInfo; inline;
    function GetHelpRequested: Boolean;
    function GetOptions: TCLPOptions;
    function GrabNextElement(var ARemaining, AToken: string): Boolean;
    function IsBuiltInHelpToken(const ARawToken: string): Boolean;
    function IsSwitch(const ASwitchRawValue: string; var AParam: string; var AHasParamDelim: Boolean; var AData: TSwitchData): Boolean;
    function MapPropertyType(const AProp: TRttiProperty): TCLPSwitchType;
    function ProcessCommandLine(const ACommandData: TObject; const ACommandLine: string): Boolean;
    function SetError(const AKind: TCLPErrorKind; const ADetail: TCLPErrorDetailed; const AText: string; const APosition: Integer = 0;
      const ASwitchName: string = ''): Boolean;
    procedure AddSwitch(const AInstance: TObject; const APropertyName, AName: string; const ALongNames: TCLPLongNames;
      const ASwitchType: TCLPSwitchType; const APosition: Integer; const AOptions: TCLPSwitchOptions;
      const ADefaultValue, ADescription, AParamName, AEnvironmentVariable: string);
    procedure ProcessAttributes(const AInstance: TObject; const AProp: TRttiProperty);
    procedure ProcessDefinitionClass(const ACommandData: TObject);
    procedure SetOptions(const AValue: TCLPOptions);
  protected // used in TUsageFormatter
    property Positionals: TArray<TSwitchData> read FPositionals;
    property SwitchList: TObjectList<TSwitchData> read FSwitchList;
  public
    constructor Create;
    destructor  Destroy; override;
    function GetExtension(const ASwitch: string): string;
    function Parse(const ACommandData: TObject): Boolean; overload;
    function Parse(const ACommandLine: string; const ACommandData: TObject): Boolean; overload;
    function Usage(const AWrapAtColumn: Integer = 80): TArray<string>;
    property ErrorInfo: TCLPErrorInfo read GetErrorInfo;
    property HelpRequested: Boolean read GetHelpRequested;
    property Options: TCLPOptions read GetOptions write SetOptions;
  end;

  TUsageFormatter = class
  strict private
    /// Returns the param suffix for a switch label, e.g. ':<value>'. Empty for
    /// Boolean switches, which are bare flags and take no argument.
    function ParamSuffix(const AData: TSwitchData): string;
    /// One alternate label, e.g. '-v' / '--verbose' / '--count:<int>'.
    function SwitchLabel(const APrefix, AName: string; const AData: TSwitchData): string;
    /// The full names column for a switch, e.g. '-c:<int>, --count:<int>'.
    function NamesColumn(const AData: TSwitchData): string;
    /// The single name used in the compact prototype line.
    function PrimaryName(const AData: TSwitchData): string;
    /// The display name of a positional switch.
    function PositionalName(const AData: TSwitchData): string;
    /// Wraps AText in '[ ]' (optional) or '< >' (required positional).
    function Decorate(const AText: string; const AData: TSwitchData): string;
    function BuildPrototypeTokens(const APositionals: TArray<TSwitchData>; const ASwitchList: TObjectList<TSwitchData>): TArray<string>;
  public
    procedure Usage(const AParser: TCommandLineParser; var AUsageList: TArray<string>; const AWrapAtColumn: Integer = 80);
  end;

var
  GDelphiCommandLineParser: ICommandLineParser;

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
    var LIndex: Integer := 1;
    var LResultLength := Length(Result);

    while (LIndex <= LResultLength) and Result[LIndex].IsLower do
      Inc(LIndex);

    if LIndex > 1 then
      Result := Copy(Result, LIndex, LResultLength - LIndex + 1);
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

  for LIndex := 0 to High(LUsageLines) do
  begin
    LLine := LUsageLines[LIndex];

    WriteLn(LLine);
  end;

  var LError := AParser.ErrorInfo.Text;

  if not LError.IsEmpty then
  begin
    WriteLn('');
    WriteLn('  * Error in switch:');
    WriteLn('    - ' + AParser.ErrorInfo.SwitchName + ' - ' + AParser.ErrorInfo.Text);
  end;
end;

function SplitArray(const AStringValue: string): TArray<string>;
begin
  Result := AStringValue.Split([',', ';']);
end;

{ exports }

function CommandLineParser: ICommandLineParser;
begin
  if not Assigned(GDelphiCommandLineParser) then
    GDelphiCommandLineParser := CreateCommandLineParser;

  Result := GDelphiCommandLineParser;
end;

function CreateCommandLineParser: ICommandLineParser;
begin
  Result := TCommandLineParser.Create;
end;

{ CLPNameAttribute }

constructor CLPNameAttribute.Create(const AName: string);
begin
  inherited Create;

  FName := AName;
end;

{ CLPLongNameAttribute }

constructor CLPLongNameAttribute.Create(const ALongName, AAbbreviation: string);
begin
  inherited Create;

  FLongName := ALongName;
  FAbbreviation := AAbbreviation;
end;

{ CLPEnvironmentAttribute }

constructor CLPEnvironmentAttribute.Create(const AVariableName: string);
begin
  inherited Create;

  FVariableName := AVariableName;
end;

{ CLPIllegalValueAttribute }

constructor CLPIllegalValueAttribute.Create(const AValue: string);
begin
  inherited Create;

  FValue := AValue;
end;

{ CLPRangeAttribute }

constructor CLPRangeAttribute.Create(const AMin, AMax: Int64);
begin
  inherited Create;

  FMinInt := AMin;
  FMaxInt := AMax;
  FIsFloat := False;
end;

constructor CLPRangeAttribute.Create(const AMin, AMax: Double);
begin
  inherited Create;

  FMinFloat := AMin;
  FMaxFloat := AMax;
  FIsFloat := True;
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

constructor TCLPLongName.Create(const ALongForm, AAbbreviation: string);
begin
  LongForm := ALongForm;
  Abbreviation := AAbbreviation;
end;

{ TSwitchData }

constructor TSwitchData.Create(const AInstance: TObject; const APropertyName, AName: string;
  const ALongNames: TCLPLongNames; const ASwitchType: TCLPSwitchType; const APosition: Integer;
  const AOptions: TCLPSwitchOptions; const ADefaultValue, ADescription, AParamName, AEnvironmentVariable: string);
begin
  inherited Create;

  // Hold the RTTI context for the whole lifetime of this object. TRttiContext is
  // a record whose interface token keeps the shared RTTI pool - and every
  // TRttiType / TRttiProperty it hands out - alive. If the context lived only as
  // a local inside a lookup helper, the pool would be released the moment that
  // helper returned, leaving any returned TRttiProperty dangling.
  FRttiContext := TRttiContext.Create;

  FInstance := AInstance;
  FPropertyName := APropertyName;
  FName := AName;
  FLongNames := ALongNames;
  FSwitchType := ASwitchType;
  FPosition := APosition;
  FOptions := AOptions;
  FDefaultValue := ADefaultValue;
  FDescription := ADescription;
  FParamName := AParamName;
  FEnvironmentVariable := AEnvironmentVariable;
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

function TSwitchData.DisplayName: string;
begin
  if Length(FLongNames) > 0 then
    Result := FLongNames[0].LongForm
  else if FName <> '' then
    Result := FName
  else if FPropertyName <> '' then
    Result := FPropertyName
  else
    Result := IntToStr(FPosition);
end;

function TSwitchData.GetRttiProperty: TRttiProperty;
begin
  // Use the long-lived FRttiContext (created in the constructor) so the returned
  // TRttiProperty stays valid for as long as this TSwitchData lives. Creating a
  // local context here instead would free the RTTI pool the moment this function
  // returned, leaving the caller with a dangling TRttiProperty - a use-after-free
  // that typically surfaces as an AV around the next call that touches it.
  Result := FRttiContext.GetType(FInstance.ClassType).GetProperty(FPropertyName);
end;

procedure TSwitchData.SetBooleanTrue;
var
  LProperty: TRttiProperty;
begin
  if SwitchType <> stBoolean then
    raise Exception.Create('TSwitchData.SetBooleanTrue: only supported for Boolean switches');

  LProperty := GetRttiProperty;

  LProperty.SetValue(FInstance, True);
  FHasValue := True;
end;

function TSwitchData.GetValue: string;
var
  LProperty: TRttiProperty;
begin
  LProperty := GetRttiProperty;

  case SwitchType of
    stEnumeration:
      begin
        var LValue := LProperty.GetValue(FInstance);
        Result := GetEnumName(LProperty.PropertyType.Handle, LValue.AsOrdinal);
      end;
    stStringArray:
      begin
        var LArray: TArray<string> := LProperty.GetValue(FInstance).AsType<TArray<string>>;
        Result := string.Join(';', LArray);
      end;
    stFloat:
      Result := FloatToStr(LProperty.GetValue(FInstance).AsExtended, TFormatSettings.Invariant);
    stDate:
      // Date-only: no time component or 'Z' suffix.
      Result := FormatDateTime('yyyy-mm-dd', LProperty.GetValue(FInstance).AsExtended);
    stTime:
      Result := FormatDateTime('hh:nn:ss', LProperty.GetValue(FInstance).AsExtended);
    stDateTime:
      Result := DateToISO8601(LProperty.GetValue(FInstance).AsExtended, True);
  else
    Result := LProperty.GetValue(FInstance).AsString;
  end;
end;

function TSwitchData.GetValueOrDefault: string;
begin
  Result := GetValue;

  if Result.IsEmpty then
    Result := DefaultValue;
end;

function TSwitchData.HasIntRange: Boolean;
begin
  Result := FHasIntRange;
end;

function TSwitchData.HasFloatRange: Boolean;
begin
  Result := FHasFloatRange;
end;

procedure TSwitchData.SetIntRange(const AMin, AMax: Int64);
begin
  FHasIntRange := True;
  FMinInt := AMin;
  FMaxInt := AMax;
end;

procedure TSwitchData.SetFloatRange(const AMin, AMax: Double);
begin
  FHasFloatRange := True;
  FMinFloat := AMin;
  FMaxFloat := AMax;
end;

procedure TSwitchData.AddIllegalValue(const AValue: string);
begin
  SetLength(FIllegalValues, Length(FIllegalValues) + 1);
  FIllegalValues[High(FIllegalValues)] := AValue;
end;

function TSwitchData.IsIllegalValue(const AValue: string): Boolean;
var
  LIllegal: string;
begin
  for LIllegal in FIllegalValues do
    if SameText(LIllegal, AValue) then
      Exit(True);

  Result := False;
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
  LInt64Value: Int64;
  LProperty: TRttiProperty;
begin
  Result := True;

  LProperty := GetRttiProperty;

  case SwitchType of
    stString: LProperty.SetValue(FInstance, AValue);
    stInteger:
      begin
        if not TryStrToInt64(AValue, LInt64Value) then
          Exit(False);

        if FHasIntRange and ((LInt64Value < FMinInt) or (LInt64Value > FMaxInt)) then
          Exit(False);

        // tkInt64 properties take the full 64-bit value. A 32-bit Integer (or
        // smaller) property must reject anything that doesn't fit, rather than
        // silently wrapping.
        if LProperty.PropertyType.TypeKind = tkInt64 then
          LProperty.SetValue(FInstance, LInt64Value)
        else
        begin
          if (LInt64Value < Low(Integer)) or (LInt64Value > High(Integer)) then
            Exit(False);

          LProperty.SetValue(FInstance, Integer(LInt64Value));
        end;
      end;
    stFloat:
      begin
        var LFloatValue: Double;

        if not TryStrToFloat(AValue, LFloatValue, TFormatSettings.Invariant) then
          Exit(False);

        if FHasFloatRange and ((LFloatValue < FMinFloat) or (LFloatValue > FMaxFloat)) then
          Exit(False);

        LProperty.SetValue(FInstance, LFloatValue);
      end;
    stDate:
      begin
        var LDateValue: TDateTime;

        if TryISO8601ToDate(AValue, LDateValue, True) then
          LProperty.SetValue(FInstance, TValue.From<TDate>(DateOf(LDateValue)))
        else if TryStrToDate(AValue, LDateValue, TFormatSettings.Invariant) then
          LProperty.SetValue(FInstance, TValue.From<TDate>(DateOf(LDateValue)))
        else
          Exit(False);
      end;
    stTime:
      begin
        var LTimeValue: TDateTime;

        if TryStrToTime(AValue, LTimeValue, TFormatSettings.Invariant) then
          LProperty.SetValue(FInstance, TValue.From<TTime>(TimeOf(LTimeValue)))
        else
          Exit(False);
      end;
    stDateTime:
      begin
        var LDateTimeValue: TDateTime;

        if TryISO8601ToDate(AValue, LDateTimeValue, True) then
          LProperty.SetValue(FInstance, TValue.From<TDateTime>(LDateTimeValue))
        else if TryStrToDateTime(AValue, LDateTimeValue, TFormatSettings.Invariant) then
          LProperty.SetValue(FInstance, TValue.From<TDateTime>(LDateTimeValue))
        else
          Exit(False);
      end;
    stBoolean:
      begin
        var LBoolValue: Boolean;

        if TryStrToBool(AValue, LBoolValue) then
          LProperty.SetValue(FInstance, LBoolValue)
        else
          Exit(False);
      end;
    stEnumeration:
      begin
        if not LProperty.IsWritable then
          Exit(False);

        if IsIllegalValue(AValue) then
          Exit(False);

        var LEnumValue: Integer := GetEnumValue(LProperty.PropertyType.Handle, AValue);

        if LEnumValue = -1 then
          Exit(False);

        var LValue := TValue.FromOrdinal(LProperty.PropertyType.Handle, LEnumValue);

        LProperty.SetValue(FInstance, LValue);
      end;
    stStringArray:
      begin
        var LStringArray := SplitArray(AValue);

        var LValue := TValue.From<TArray<string>>(LStringArray);

        LProperty.SetValue(FInstance, LValue);
      end;
    else
      raise Exception.Create('TSwitchData.SetValue: Unknown or unsupported SwitchType: ' + TEnumConverter.EnumToString(SwitchType));
  end;

  FHasValue := True;
end;

{ TCommandLineParser }

constructor TCommandLineParser.Create;
begin
  inherited Create;

  FSwitchList := TObjectList<TSwitchData>.Create;
  FSwitchComparer := TIStringComparer.Ordinal; // Don't destroy, Ordinal returns a global singleton
  FSwitchDict := TDictionary<string, TSwitchData>.Create(FSwitchComparer);
end;

destructor TCommandLineParser.Destroy;
begin
  FreeAndNil(FSwitchDict);
  FreeAndNil(FSwitchList);

  inherited Destroy;
end;

procedure TCommandLineParser.AddSwitch(const AInstance: TObject; const APropertyName, AName: string; const ALongNames: TCLPLongNames;
  const ASwitchType: TCLPSwitchType; const APosition: Integer; const AOptions: TCLPSwitchOptions;
  const ADefaultValue, ADescription, AParamName, AEnvironmentVariable: string);
var
  LSwitchData: TSwitchData;
  LIndex: Integer;
  LLongName: TCLPLongName;
begin
  LSwitchData := TSwitchData.Create(AInstance, APropertyName, AName, ALongNames, ASwitchType, APosition, AOptions, ADefaultValue,
    ADescription, AParamName, AEnvironmentVariable);

  FSwitchList.Add(LSwitchData);

  if AName <> '' then
    FSwitchDict.Add(AName, LSwitchData);

  for LLongName in ALongNames do
  begin
    FSwitchDict.Add(LLongName.LongForm, LSwitchData);

    if LLongName.Abbreviation <> '' then
      for LIndex := Length(LLongName.Abbreviation) to Length(LLongName.LongForm) - 1 do
        FSwitchDict.AddOrSetValue(Copy(LLongName.LongForm, 1, LIndex), LSwitchData);
  end;
end;

///  <summary>
///    Verifies attribute consistency.
///
///   For positional attributes there must be no 'holes' (i.e. positional attributes must
///   be numbered 1,2,...N) and there must be no 'required' attributes after 'optional'
///   attributes.
///
///   There must be at most one 'Rest' positional attribute.
///
///   Each switch attribute must have a long or short name. (That is actually enforced
///   in the current implementation as long name is set to property name by default,
///   but the test is still left in for future-proofing.)
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
          Exit(SetError(ekPositionalsBadlyDefined, edExtraCLPPositionRest, SOnlyOnePositionRestAllowed, 0, LSwitchData.PropertyName))
        else if LSwitchData.SwitchType <> stString then
          Exit(SetError(ekPositionalsBadlyDefined, edCLPPositionRestNotString, SPositionRestMustBeString, 0, LSwitchData.PropertyName))
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
        LHasOptional := True
      else if LHasOptional then
        Exit(SetError(ekPositionalsBadlyDefined, edRequiredAfterOptional, SRequiredAfterOptionalForbidden, LSwitchData.Position));
    end;

  if Assigned(LPositionRest) then
  begin
    Inc(LHighPos);
    LPositionRest.Position := LHighPos;
  end;

  if LHighPos > 0 then
  begin
    SetLength(FPositionals, LHighPos);

    for LIndex := Low(FPositionals) to High(FPositionals) do
      FPositionals[LIndex] := nil;

    for LSwitchData in FSwitchList do
      if soPositional in LSwitchData.Options then
        FPositionals[LSwitchData.Position - 1] := LSwitchData;

    for LIndex := Low(FPositionals) to High(FPositionals) do
      if FPositionals[LIndex] = nil then
        Exit(SetError(ekPositionalsBadlyDefined, edMissingPositionalDefinition, SMissingPositionalDefinition, LIndex + 1));
  end;

  for LSwitchData in FSwitchList do
    if not (soPositional in LSwitchData.Options) then
      if (LSwitchData.Name = '') and (Length(LSwitchData.LongNames) = 0) then
        Exit(SetError(ekNameNotDefined, edMissingNameForProperty, SMissingNameForProperty, 0, LSwitchData.PropertyName))
      else if (LSwitchData.Name <> '') and (Length(LSwitchData.Name) <> 1) then
        Exit(SetError(ekShortNameTooLong, edShortNameTooLong, SShortNameMustBeOneLetterLong, 0, LSwitchData.Name))
      else for LLongName in LSwitchData.LongNames do
        if (LLongName.Abbreviation <> '') and (not StartsText(LLongName.Abbreviation, LLongName.LongForm)) then
          Exit(SetError(ekLongFormsDontMatch, edLongFormsDontMatch, SLongFormsDontMatch, 0, LLongName.LongForm));
end;

///  <summary>
///    Detects and expands a response-file token (@filename) on the command line.
///
///    Rules:
///      * If no token starts with '@', the command line is left unchanged
///        and the function returns True.
///      * If exactly one token is present AND it starts with '@', the file is
///        loaded and its contents replace ACommandLine. Newlines in the file
///        are treated as whitespace so the existing tokenizer handles it.
///      * Any other shape (multiple tokens with @, two @-tokens, etc.) is an
///        error — response files must be the sole command-line argument.
///      * Recursion is not allowed: if the loaded content itself contains an
///        '@' token, that's also an error.
///  </summary>
function TCommandLineParser.ExpandResponseFile(var ACommandLine: string): Boolean;
var
  LCopy: string;
  LToken: string;
  LTotalTokens: Integer;
  LResponseFileToken: string;
  LResponseFileCount: Integer;
  LFileName: string;
  LFileLines: TStringList;
begin
  LResponseFileToken := '';
  LResponseFileCount := 0;
  LTotalTokens := 0;

  // First pass: tokenize a copy of the command line and count tokens / @-tokens.
  LCopy := ACommandLine;

  while GrabNextElement(LCopy, LToken) do
  begin
    Inc(LTotalTokens);

    if LToken.StartsWith('@') then
    begin
      LResponseFileToken := LToken;
      Inc(LResponseFileCount);
    end;
  end;

  if LResponseFileCount = 0 then
    Exit(True); // Nothing to expand.

  if (LResponseFileCount > 1) or (LTotalTokens > 1) then
    Exit(SetError(ekResponseFile, edResponseFileMustBeSole, SResponseFileMustBeSole, 0, LResponseFileToken));

  // Exactly one token, which is "@<filename>". Load the file.
  LFileName := LResponseFileToken;
  Delete(LFileName, 1, 1);

  if not FileExists(LFileName) then
    Exit(SetError(ekResponseFile, edResponseFileNotFound, SResponseFileNotFound, 0, LFileName));

  LFileLines := TStringList.Create;
  try
    // Load as UTF-8 by default. A BOM, if present, still wins; but for the
    // common no-BOM case this avoids falling back to the system ANSI codepage
    // and mangling non-ASCII values.
    LFileLines.LoadFromFile(LFileName, TEncoding.UTF8);
    // Join all lines with single spaces. GrabNextElement re-tokenizes this
    // exactly like a regular command line, with full quoting support.
    ACommandLine := string.Join(' ', LFileLines.ToStringArray);
  finally
    LFileLines.Free;
  end;

  // Recursion guard: the expanded content must not itself contain @-tokens.
  LCopy := ACommandLine;

  while GrabNextElement(LCopy, LToken) do
    if LToken.StartsWith('@') then
      Exit(SetError(ekResponseFile, edResponseFileMustBeSole, SResponseFileMustBeSole, 0, LToken));

  Result := True;
end;

function TCommandLineParser.FileSystemCheck(const ASwitchData: TSwitchData; var AFileOrDirectoryNotFound: string): TFSCheckResult;
var
  LStringValue: string;
  LStringArray: TArray<string>;
begin
  Result := fscOk;

  // Nothing to check: the switch wasn't provided and there is no default to validate.
  if (not ASwitchData.HasValue) and (ASwitchData.DefaultValue = '') then
    Exit;

  LStringValue := ASwitchData.GetValueOrDefault;
  LStringArray := SplitArray(LStringValue);

  if soFileMustExist in ASwitchData.Options then
  begin
    for var LFileName in LStringArray do
      if not FileExists(LFileName) then
      begin
        AFileOrDirectoryNotFound := LFileName;
        Exit(fscFileMissing);
      end;
  end
  else if soDirectoryMustExist in ASwitchData.Options then
  begin
    for var LDirectory in LStringArray do
      if not DirectoryExists(LDirectory) then
      begin
        AFileOrDirectoryNotFound := LDirectory;
        Exit(fscDirectoryMissing);
      end;
  end;
end;

function TCommandLineParser.FindExtendableSwitch(const ASwitchName: string; var AParam: string; var AData: TSwitchData): Boolean;
var
  LSwitchEntry: TPair<string, TSwitchData>;
begin
  Result := False;

  for LSwitchEntry in FSwitchDict do
  begin
    if not (soExtendable in LSwitchEntry.Value.Options) then
      Continue;

    if ASwitchName.StartsWith(LSwitchEntry.Key, True) then
    begin
      AData := LSwitchEntry.Value;
      AParam := ASwitchName;
      Delete(AParam, 1, Length(LSwitchEntry.Key));

      Exit(True);
    end;
  end;
end;

function TCommandLineParser.BuildCommandLineFromParams: string;
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

function TCommandLineParser.GetHelpRequested: Boolean;
begin
  Result := FHelpRequested;
end;

function TCommandLineParser.IsBuiltInHelpToken(const ARawToken: string): Boolean;
const
  CHelpTokens: array [0..{$IFDEF MSWINDOWS}5{$ELSE}2{$ENDIF}] of string = (
    '-h', '-?', '--help'
    {$IFDEF MSWINDOWS}, '/h', '/?', '/help'{$ENDIF});
var
  LToken: string;
begin
  for LToken in CHelpTokens do
    if SameText(ARawToken, LToken) then
      Exit(True);

  Result := False;
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

function TCommandLineParser.GrabNextElement(var ARemaining, AToken: string): Boolean;
var
  LIndex: Integer;
  LInQuote: Boolean;
begin
  AToken := '';
  ARemaining := TrimLeft(ARemaining);

  if ARemaining = '' then
    Exit(False);

  LInQuote := False;
  LIndex := 1;

  while LIndex <= Length(ARemaining) do
  begin
    if ARemaining[LIndex] = '"' then
    begin
      if LInQuote and (LIndex < Length(ARemaining)) and (ARemaining[LIndex + 1] = '"') then
      begin
        // Escaped quote inside a quoted region: emit a single quote and skip both.
        AToken := AToken + '"';
        Inc(LIndex, 2);
        Continue;
      end;

      LInQuote := not LInQuote;
    end
    else if (ARemaining[LIndex] = ' ') and not LInQuote then
      Break
    else
      AToken := AToken + ARemaining[LIndex];

    Inc(LIndex);
  end;

  Delete(ARemaining, 1, LIndex);

  Result := True;
end;

function TCommandLineParser.IsSwitch(const ASwitchRawValue: string; var AParam: string; var AHasParamDelim: Boolean; var AData: TSwitchData): Boolean;
var
  LDelimPos: Integer;
  LFirstDelimPos: Integer;
  LName: string;
  LParamDelim: Char;
  LSwitchDelim: string;
  LSwitchBody: string;
begin
  Result := False;
  AParam := '';
  AHasParamDelim := False;

  LSwitchBody := ASwitchRawValue;
  for LSwitchDelim in SWITCH_DELIMITERS do
    if StartsStr(LSwitchDelim, LSwitchBody) then
    begin
      LSwitchBody := ASwitchRawValue;

      Delete(LSwitchBody, 1, Length(LSwitchDelim));

      if LSwitchBody <> '' then
        Result := True;

      Break;
    end;

  if Result then //try to extract parameter data
  begin
    LName := LSwitchBody;
    LFirstDelimPos := 0;

    for LParamDelim in PARAM_DELIMITERS do
    begin
      LDelimPos := Pos(LParamDelim, LName);

      if (LDelimPos > 0) and ((LFirstDelimPos = 0) or (LDelimPos < LFirstDelimPos)) then
        LFirstDelimPos := LDelimPos;
    end;

    if LFirstDelimPos > 0 then
    begin
      AHasParamDelim := True;
      AParam := LName;
      Delete(AParam, 1, LFirstDelimPos);
      LName := Copy(LName, 1, LFirstDelimPos - 1);
    end;

    FSwitchDict.TryGetValue(LName, AData);

    if not Assigned(AData) then //try extendable switches
      FindExtendableSwitch(LName, AParam, AData);

    if not Assigned(AData) then //try short name
    begin
      if FSwitchDict.TryGetValue(LSwitchBody[1], AData) then
      begin
        AParam := LSwitchBody;
        Delete(AParam, 1, 1);

        if (AParam <> '') and (AData.SwitchType = stBoolean) then // misdetection, Boolean switch cannot accept data
          AData := nil;
      end;
    end;
  end;
end;

function TCommandLineParser.MapPropertyType(const AProp: TRttiProperty): TCLPSwitchType;

  procedure RaiseUnsupportedPropertyType(const AProp: TRttiProperty; var AResult: TCLPSwitchType); noreturn;
  begin
    AResult := stString; // Dummy assignment to silence the "Result not set" compiler warning.

    raise Exception.CreateFmt(SUnsupportedPropertyType, [AProp.Name]) at ReturnAddress;
  end;

begin
  case AProp.PropertyType.TypeKind of
    tkInteger, tkInt64:
      Result := stInteger;
    tkFloat:
      if AProp.PropertyType.Handle = TypeInfo(TDate) then
        Result := stDate
      else if AProp.PropertyType.Handle = TypeInfo(TTime) then
        Result := stTime
      else if AProp.PropertyType.Handle = TypeInfo(TDateTime) then
        Result := stDateTime
      else
        Result := stFloat;
    tkEnumeration:
      if AProp.PropertyType.Handle = TypeInfo(Boolean) then
        Result := stBoolean
      else
        Result := stEnumeration;
    tkString, tkLString, tkWString, tkUString:
      Result := stString;
    tkDynArray:
      begin
        var LArrayType := AProp.PropertyType as TRttiDynamicArrayType;

        if LArrayType.ElementType.TypeKind in [tkUString, tkLString, tkWString, tkString] then
          Result := stStringArray
        else
          RaiseUnsupportedPropertyType(AProp, Result);
      end
    else
      RaiseUnsupportedPropertyType(AProp, Result);
  end;
end;

function TCommandLineParser.Parse(const ACommandLine: string; const ACommandData: TObject): Boolean;
var
  LCommandLine: string;
begin
  FErrorInfo := Default(TCLPErrorInfo);
  FHelpRequested := False;
  FSwitchDict.Clear;
  SetLength(FPositionals, 0);
  FSwitchList.Clear;

  ProcessDefinitionClass(ACommandData);

  Result := CheckAttributes;

  if not Result then
    raise ECLPConfigurationError.Create(ErrorInfo) at ReturnAddress;

  LCommandLine := ACommandLine;

  if not ExpandResponseFile(LCommandLine) then
    Exit(False);

  Result := ProcessCommandLine(ACommandData, LCommandLine);
end;

function TCommandLineParser.Parse(const ACommandData: TObject): Boolean;
begin
  Result := Parse(BuildCommandLineFromParams, ACommandData);
end;

procedure TCommandLineParser.ProcessAttributes(const AInstance: TObject; const AProp: TRttiProperty);

  procedure AddLongName(const ALongForm, AAbbreviation: string; var ALongNames: TCLPLongNames);
  begin
    SetLength(ALongNames, Length(ALongNames) + 1);

    ALongNames[High(ALongNames)] := TCLPLongName.Create(ALongForm, AAbbreviation);
  end;

var
  LAttribute: TCustomAttribute;
  LDefault: string;
  LDescription: string;
  LEnvironmentVariable: string;
  LIllegalValues: TArray<string>;
  LLongNames: TCLPLongNames;
  LShortName: string;
  LOptions: TCLPSwitchOptions;
  LParamName: string;
  LPosition: Integer;
  LRangeAttr: CLPRangeAttribute;
begin
  LRangeAttr := nil;
  SetLength(LIllegalValues, 0);

  LShortName := '';
  LDescription := '';
  LEnvironmentVariable := '';
  LParamName := CLPDescriptionAttribute.DefaultValue;
  LOptions := [];
  LPosition := 0;

  SetLength(LLongNames, 0);

  for LAttribute in AProp.GetAttributes do
  begin
    // A CLPSkip attribute means "ignore this property entirely" — bail out
    // immediately without registering a switch.
    if LAttribute is CLPSkipAttribute then
      Exit
    else if LAttribute is CLPNameAttribute then
      LShortName := CLPNameAttribute(LAttribute).Name
    else if LAttribute is CLPLongNameAttribute then
      AddLongName(CLPLongNameAttribute(LAttribute).LongName, CLPLongNameAttribute(LAttribute).Abbreviation, LLongNames)
    else if LAttribute is CLPDefaultAttribute then
      LDefault := CLPDefaultAttribute(LAttribute).DefaultValue
    else if LAttribute is CLPDescriptionAttribute then
    begin
      LDescription := CLPDescriptionAttribute(LAttribute).Description;
      LParamName := CLPDescriptionAttribute(LAttribute).ParamName;
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
    end
    else if LAttribute is CLPEnvironmentAttribute then
      LEnvironmentVariable := CLPEnvironmentAttribute(LAttribute).VariableName
    else if LAttribute is CLPRangeAttribute then
      LRangeAttr := CLPRangeAttribute(LAttribute)
    else if LAttribute is CLPIllegalValueAttribute then
    begin
      SetLength(LIllegalValues, Length(LIllegalValues) + 1);
      LIllegalValues[High(LIllegalValues)] := CLPIllegalValueAttribute(LAttribute).Value;
    end;
  end;

  if (Length(LLongNames) = 0) and (not SameText(AProp.Name, Trim(LShortName))) then
    AddLongName(AProp.Name, '', LLongNames);

  AddSwitch(AInstance, AProp.Name, Trim(LShortName), LLongNames, MapPropertyType(AProp), LPosition,
    LOptions, LDefault, Trim(LDescription), Trim(LParamName), Trim(LEnvironmentVariable));

  // Range bounds, if any, apply to the just-added switch (last in FSwitchList).
  if Assigned(LRangeAttr) then
  begin
    if LRangeAttr.IsFloat then
      FSwitchList.Last.SetFloatRange(LRangeAttr.MinFloat, LRangeAttr.MaxFloat)
    else
      FSwitchList.Last.SetIntRange(LRangeAttr.MinInt, LRangeAttr.MaxInt);
  end;

  // Illegal-value list (CLPIllegalValue attributes) applies to the same switch.
  for var LIllegal in LIllegalValues do
    FSwitchList.Last.AddIllegalValue(LIllegal);
end;

function TCommandLineParser.ProcessCommandLine(const ACommandData: TObject; const ACommandLine: string): Boolean;
var
  LCommandLine: string;
  LCurrentRawParameter: string;
  LFileSystemObjectNotFound: string;
  LHasParamDelim: Boolean;
  LParamValue: string; // Value extracted from after the parameter delimiter, e.g. 'foo' in '-name:foo'.
  LPosition: Integer;
  LSwitchData: TSwitchData;
begin
  Result := True;

  // A CLPDefault that cannot be applied is a developer/configuration mistake
  // (wrong type, out of CLPRange, bad enum identifier). Surface it as a
  // configuration error rather than silently leaving the property unset.
  for LSwitchData in FSwitchList do
    if LSwitchData.DefaultValue <> '' then
      if not LSwitchData.SetValue(LSwitchData.DefaultValue) then
      begin
        SetError(ekInvalidDefault, edInvalidDefaultValue, SInvalidDefaultValue, 0, LSwitchData.DisplayName);
        raise ECLPConfigurationError.Create(FErrorInfo) at ReturnAddress;
      end;

  // Env-var fallback: applied AFTER defaults but BEFORE command-line tokens,
  // so env vars override defaults and the command line overrides env vars.
  // An environment value is runtime input (like the command line), so a value
  // that fails to parse is a normal data error — not silently ignored.
  for LSwitchData in FSwitchList do
    if LSwitchData.EnvironmentVariable <> '' then
    begin
      var LEnvValue := GetEnvironmentVariable(LSwitchData.EnvironmentVariable);

      if (LEnvValue <> '') and (not LSwitchData.SetValue(LEnvValue)) then
        Exit(SetError(ekInvalidData, edInvalidDataForSwitch, SInvalidDataForSwitch, 0, LSwitchData.DisplayName));
    end;

  LPosition := 1;
  LCommandLine := ACommandLine;

  while GrabNextElement(LCommandLine, LCurrentRawParameter) do
  begin
    if (opEnableBuiltInHelp in FOptions) and IsBuiltInHelpToken(LCurrentRawParameter) then
    begin
      FHelpRequested := True;
      Exit(True);
    end;

    if IsSwitch(LCurrentRawParameter, LParamValue, LHasParamDelim, LSwitchData) then
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
          LSwitchData.SetBooleanTrue;

          if LParamValue <> '' then
            LSwitchData.ParamValue := LParamValue;
        end
        else
        begin
          var LBoolValue: Boolean;

          if TryStrToBool(LParamValue, LBoolValue) then
            LSwitchData.SetValue(LParamValue)
          else
            Exit(SetError(ekInvalidData, edBooleanWithData, SBooleanSwitchCannotAcceptData, 0, LCurrentRawParameter));
        end;
      end
      // A non-Boolean switch with an explicit '=' or ':' delimiter is treated
      // as "provided" even when the value is empty — SetValue is called so the
      // property is set to '' (for strings) or a parse error fires (for
      // numeric/enum types).
      else if (LParamValue <> '') or LHasParamDelim then
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

  // Single post-parse validation pass. FSwitchList contains every switch
  // (positional and named), so one loop is enough — the error kind is picked
  // based on whether the switch is positional.
  for LSwitchData in FSwitchList do
  begin
    if (soRequired in LSwitchData.Options) and (not LSwitchData.HasValue) then
    begin
      if soPositional in LSwitchData.Options then
        Exit(SetError(ekMissingPositional, edMissingRequiredParameter, SRequiredParameterWasNotProvided,
          LSwitchData.Position, LSwitchData.DisplayName))
      else
        Exit(SetError(ekMissingNamed, edMissingRequiredSwitch, SRequiredSwitchWasNotProvided,
          0, LSwitchData.DisplayName));
    end
    else if (soFileMustExist in LSwitchData.Options) or (soDirectoryMustExist in LSwitchData.Options) then
    begin
      if LSwitchData.SwitchType in [stString, stStringArray] then
      begin
        case FileSystemCheck(LSwitchData, LFileSystemObjectNotFound) of
          fscFileMissing: Exit(SetError(ekFileDoesNotExist, edFileDoesNotExist, SFileDoesNotExist + ': '
            + LFileSystemObjectNotFound.QuotedString('"'),
            LSwitchData.Position, LSwitchData.DisplayName));
          fscDirectoryMissing: Exit(SetError(ekDirectoryDoesNotExist, edDirectoryDoesNotExist, SDirectoryDoesNotExist + ': '
            + LFileSystemObjectNotFound.QuotedString('"'), LSwitchData.Position, LSwitchData.DisplayName));
        end;
      end
      else
        Exit(SetError(ekWrongDataTypeForFileOrDirectoryMustExist, edWrongDataTypeForFileOrDirectoryMustExist,
          SWrongDataTypeForFileOrDirectoryMustExist, LSwitchData.Position, LSwitchData.DisplayName));
    end;
  end;
end;

procedure TCommandLineParser.ProcessDefinitionClass(const ACommandData: TObject);
var
  LContext: TRttiContext;
  LProperty: TRttiProperty;
  LRttiType: TRttiType;
  LClassType: TClass;
begin
  LContext := TRttiContext.Create;
  LClassType := ACommandData.ClassType;

  while Assigned(LClassType) do
  begin
    LRttiType := LContext.GetType(LClassType);

    for LProperty in LRttiType.GetProperties do
      if LProperty.Parent = LRttiType then
        ProcessAttributes(ACommandData, LProperty);

    LClassType := LClassType.ClassParent;
  end;
end;

function TCommandLineParser.SetError(const AKind: TCLPErrorKind; const ADetail: TCLPErrorDetailed; const AText: string;
  const APosition: Integer; const ASwitchName: string): Boolean;
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
    LFormatter.Usage(Self, Result, AWrapAtColumn);
  finally
    FreeAndNil(LFormatter);
  end;
end;

{ Usage text helpers (pure, state-free, exposed for unit testing) }

function CLPLastSpaceBefore(const AValue: string; const AStartPos: Integer): Integer;
begin
  Result := AStartPos - 1;

  if Result > Length(AValue) then
    Result := Length(AValue);

  while (Result > 0) and (AValue[Result] <> ' ') do
    Dec(Result);
end;

function CLPWordWrapLine(const ALine: string; const AWrapAtColumn, AHangingIndent: Integer): TArray<string>;
var
  LResult: TList<string>;
  LRemaining: string;
  LIndentStr: string;
  LBreak: Integer;
begin
  LResult := TList<string>.Create;
  try
    LIndentStr := StringOfChar(' ', AHangingIndent);
    LRemaining := ALine;

    while (AWrapAtColumn > 0) and (Length(LRemaining) > AWrapAtColumn) do
    begin
      LBreak := CLPLastSpaceBefore(LRemaining, AWrapAtColumn + 1);

      // No usable break point inside the wrappable region — either a single
      // word longer than the column, or the only space is within the hanging
      // indent / left column. Emit the rest intact rather than splitting a word
      // or looping forever.
      if LBreak <= AHangingIndent then
        Break;

      LResult.Add(Copy(LRemaining, 1, LBreak - 1));
      LRemaining := LIndentStr + TrimLeft(Copy(LRemaining, LBreak + 1, Length(LRemaining)));
    end;

    LResult.Add(LRemaining);
    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

function CLPFormatTwoColumnBlock(const ALeft, ARight: TArray<string>; const ASeparator: string;
  const AWrapAtColumn: Integer): TArray<string>;
var
  LResult: TList<string>;
  LMaxLeft: Integer;
  LIndex: Integer;
  LPaddedLeft: string;
begin
  LResult := TList<string>.Create;
  try
    LMaxLeft := 0;

    for LIndex := 0 to High(ALeft) do
      if Length(ALeft[LIndex]) > LMaxLeft then
        LMaxLeft := Length(ALeft[LIndex]);

    for LIndex := 0 to High(ALeft) do
    begin
      LPaddedLeft := ALeft[LIndex] + StringOfChar(' ', LMaxLeft - Length(ALeft[LIndex]));

      if (LIndex <= High(ARight)) and (ARight[LIndex] <> '') then
        LResult.AddRange(CLPWordWrapLine(LPaddedLeft + ASeparator + ARight[LIndex], AWrapAtColumn, LMaxLeft + Length(ASeparator)))
      else
        LResult.Add(TrimRight(LPaddedLeft));
    end;

    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

function CLPWrapTokens(const ATokens: TArray<string>; const AWrapAtColumn, AContinuationIndent: Integer): TArray<string>;
var
  LResult: TList<string>;
  LIndent: string;
  LCurrent: string;
  LToken: string;
begin
  LResult := TList<string>.Create;
  try
    LIndent := StringOfChar(' ', AContinuationIndent);
    LCurrent := '';

    for LToken in ATokens do
    begin
      if LToken = '' then
        Continue;

      if LCurrent = '' then
        // First token of the first line: no indent. Continuation lines get the
        // indent applied when a wrap happens, below.
        LCurrent := LToken
      else if (AWrapAtColumn > 0) and (Length(LCurrent) + 1 + Length(LToken) > AWrapAtColumn) then
      begin
        // The token would overflow the column. Tokens are never split, so the
        // current line is flushed and the token starts the next (indented) line,
        // even if the token itself is wider than the column.
        LResult.Add(LCurrent);
        LCurrent := LIndent + LToken;
      end
      else
        LCurrent := LCurrent + ' ' + LToken;
    end;

    if LCurrent <> '' then
      LResult.Add(LCurrent);

    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

{ TUsageFormatter }

function TUsageFormatter.ParamSuffix(const AData: TSwitchData): string;
begin
  // Boolean switches are bare flags; showing ':<value>' would wrongly imply
  // they take an argument.
  if (AData.SwitchType = stBoolean) or AData.ParamName.IsEmpty then
    Result := ''
  else
    Result := USAGE_PARAM_DELIMITER + AData.ParamName;
end;

function TUsageFormatter.SwitchLabel(const APrefix, AName: string; const AData: TSwitchData): string;
begin
  Result := APrefix + AName + ParamSuffix(AData);
end;

function TUsageFormatter.NamesColumn(const AData: TSwitchData): string;
var
  LLongName: TCLPLongName;
begin
  Result := '';

  if AData.Name <> '' then
    Result := SwitchLabel(USAGE_SHORT_PREFIX, AData.Name, AData);

  for LLongName in AData.LongNames do
  begin
    if Result <> '' then
      Result := Result + ', ';

    Result := Result + SwitchLabel(USAGE_SHORT_PREFIX, LLongName.LongForm, AData);
  end;

  if Result = '' then // no explicit name at all
    Result := SwitchLabel(USAGE_SHORT_PREFIX, AData.PropertyName, AData);
end;

function TUsageFormatter.PrimaryName(const AData: TSwitchData): string;
begin
  if (Length(AData.LongNames) >= 1) and not AData.LongNames[0].LongForm.IsEmpty then
    Result := USAGE_SHORT_PREFIX + AData.LongNames[0].LongForm
  else if AData.Name <> '' then
    Result := USAGE_SHORT_PREFIX + AData.Name
  else
    Result := USAGE_SHORT_PREFIX + AData.PropertyName;

  Result := Result + ParamSuffix(AData);
end;

function TUsageFormatter.PositionalName(const AData: TSwitchData): string;
begin
  if AData.Name <> '' then
    Result := AData.Name
  else if Length(AData.LongNames) <> 0 then
    Result := AData.LongNames[0].LongForm
  else
    Result := IntToStr(AData.Position);
end;

function TUsageFormatter.Decorate(const AText: string; const AData: TSwitchData): string;
begin
  if not (soRequired in AData.Options) then
    Result := '[' + AText + ']'
  else if soPositional in AData.Options then
    Result := '<' + AText + '>'
  else
    Result := AText;
end;

function TUsageFormatter.BuildPrototypeTokens(const APositionals: TArray<TSwitchData>; const ASwitchList: TObjectList<TSwitchData>): TArray<string>;
var
  LSwitchData: TSwitchData;
  LResult: TList<string>;
begin
  LResult := TList<string>.Create;
  try
    for LSwitchData in APositionals do
      if Assigned(LSwitchData) then
        LResult.Add(Decorate(PositionalName(LSwitchData), LSwitchData));

    // Only NON-positional switches here — positionals are listed above. (The old
    // implementation listed positionals twice.)
    for LSwitchData in ASwitchList do
      if not (soPositional in LSwitchData.Options) then
        LResult.Add(Decorate(PrimaryName(LSwitchData), LSwitchData));

    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

type
  // One switch's help, captured before layout so we can choose how to render it.
  TUsageDetail = record
    SwitchLabel: string;  // e.g. '[-UserName:<usernane>]'
    Description: string;   // e.g. 'User name for Db connection'
    DefaultValue: string;  // e.g. 'SYSDBA' (empty when no default)
  end;

procedure TUsageFormatter.Usage(const AParser: TCommandLineParser; var AUsageList: TArray<string>; const AWrapAtColumn: Integer = 80);
var
  LSwitchData: TSwitchData;
  LResult: TList<string>;
  LDetails: TList<TUsageDetail>;
  LDetail: TUsageDetail;
  LTokens: TArray<string>;
  LLeft: TArray<string>;
  LRight: TArray<string>;
  LIndex: Integer;
  LLeftWidth: Integer;
  LCompact: Boolean;

  procedure AddDetail(const ALabel, ADescription, ADefault: string);
  var
    LItem: TUsageDetail;
  begin
    LItem.SwitchLabel := ALabel;
    LItem.Description := ADescription;
    LItem.DefaultValue := ADefault;
    LDetails.Add(LItem);
  end;

  // The single-line (compact) right-hand text: description with the default
  // appended, exactly as the original two-column formatter produced it.
  function CompactRight(const ADetail: TUsageDetail): string;
  begin
    Result := ADetail.Description;

    if ADetail.DefaultValue <> '' then
      Result := Result + SDefault + ADetail.DefaultValue;
  end;

  // Stacked layout for one entry: label / '- description' / 'default: value',
  // each part wrapped at AWrapAtColumn.
  procedure AppendStacked(const ADetail: TUsageDetail);
  begin
    LResult.Add(StringOfChar(' ', USAGE_SWITCH_INDENT) + ADetail.SwitchLabel);

    if ADetail.Description <> '' then
      LResult.AddRange(CLPWordWrapLine(
        StringOfChar(' ', USAGE_DESC_INDENT) + USAGE_DESC_PREFIX + ADetail.Description,
        AWrapAtColumn, USAGE_DETAIL_CONT_INDENT));

    if ADetail.DefaultValue <> '' then
      LResult.AddRange(CLPWordWrapLine(
        StringOfChar(' ', USAGE_DETAIL_CONT_INDENT) + USAGE_DEFAULT_LABEL + ADetail.DefaultValue,
        AWrapAtColumn, USAGE_DETAIL_CONT_INDENT));
  end;

begin
  LResult := TList<string>.Create;
  LDetails := TList<TUsageDetail>.Create;
  try
    // [0..] = usage summary: exe name followed by the decorated switch tokens,
    // wrapped at the requested column on token boundaries (never inside a token).
    LTokens := BuildPrototypeTokens(AParser.Positionals, AParser.SwitchList);
    Insert(ExtractFileName(ParamStr(0)), LTokens, 0);
    LResult.AddRange(CLPWrapTokens(LTokens, AWrapAtColumn, USAGE_PROTO_CONT_INDENT));

    // blank separator
    LResult.Add('');

    // Collect one detail entry (label / description / default) per switch:
    // positional switches first, then named switches.
    for LSwitchData in AParser.Positionals do
      if not Assigned(LSwitchData) then // error in definition class
        AddDetail('*** missing ***', '', '')
      else
        AddDetail(Decorate(PositionalName(LSwitchData), LSwitchData),
          LSwitchData.Description, LSwitchData.DefaultValue);

    for LSwitchData in AParser.SwitchList do
      if not (soPositional in LSwitchData.Options) then
        AddDetail(Decorate(NamesColumn(LSwitchData), LSwitchData),
          LSwitchData.Description, LSwitchData.DefaultValue);

    // Choose the layout. Use the compact, aligned two-column layout when EVERY
    // entry fits on one line within the wrap column; otherwise fall back to the
    // stacked layout so long switches/descriptions are not crushed. This keeps
    // short switches with short defaults to a single line instead of three.
    LLeftWidth := 0;
    for LIndex := 0 to LDetails.Count - 1 do
      if Length(LDetails[LIndex].SwitchLabel) > LLeftWidth then
        LLeftWidth := Length(LDetails[LIndex].SwitchLabel);

    LCompact := True;
    if AWrapAtColumn > 0 then
      for LIndex := 0 to LDetails.Count - 1 do
      begin
        LDetail := LDetails[LIndex];
        if (CompactRight(LDetail) <> '') and
           (LLeftWidth + Length(USAGE_DESC_DELIMITER) + Length(CompactRight(LDetail)) > AWrapAtColumn) then
        begin
          LCompact := False;
          Break;
        end;
      end;

    if LCompact then
    begin
      SetLength(LLeft, LDetails.Count);
      SetLength(LRight, LDetails.Count);

      for LIndex := 0 to LDetails.Count - 1 do
      begin
        LLeft[LIndex] := LDetails[LIndex].SwitchLabel;
        LRight[LIndex] := CompactRight(LDetails[LIndex]);
      end;

      LResult.AddRange(CLPFormatTwoColumnBlock(LLeft, LRight, USAGE_DESC_DELIMITER, AWrapAtColumn));
    end
    else
      for LDetail in LDetails do
        AppendStacked(LDetail);

    AUsageList := LResult.ToArray;
  finally
    LDetails.Free;
    LResult.Free;
  end;
end;

{ ECLPConfigurationError }

constructor ECLPConfigurationError.Create(const AErrInfo: TCLPErrorInfo);
begin
  inherited Create(AErrInfo.Text);

  FErrorInfo := AErrInfo;
end;

end.

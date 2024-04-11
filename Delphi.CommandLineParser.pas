unit Delphi.CommandLineParser;

{
  Check https://github.com/TommiPrami/Delphi.CommandLineParser/blob/main/README.md for more information
}
interface

uses
  System.SysUtils, System.TypInfo, System.RTTI;

type
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
    ekMissingPositional, ekExtraPositional, ekMissingNamed, ekUnknownNamed, ekInvalidData);

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
    edMissingRequiredParameter     // SRequiredParameterWasNotProvided
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

implementation

uses
  System.StrUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections;

resourcestring
  SBooleanSwitchCannotAcceptData    = 'Boolean switch cannot accept data.';
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

type
  TCLPSwitchType = (stString, stInteger, stBoolean);

  TCLPSwitchOption = (soRequired, soPositional, soPositionRest, soExtendable);
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
    function GetRTTIProperty: TRttiProperty;
  strict protected
    function Quote(const AValue: string): string;
  public
    constructor Create(const AInstance: TObject; const APropertyName, AName: string;
      const ALongNames: TCLPLongNames; const ASwitchType: TCLPSwitchType; const APosition: Integer;
      const AOptions: TCLPSwitchOptions; const ADefaultValue, ADescription, AParamName: string);
    function AppendValue(const AValue, ADelim: string; const ADoQuote: Boolean): Boolean;
    procedure Enable;
    function GetValue: string;
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
    function FindExtendableSwitch(const AEl: string; var AParam: string; var AData: TSwitchData): Boolean;
    function GetCommandLine: string;
    function GetErrorInfo: TCLPErrorInfo; inline;
    function GetOptions: TCLPOptions;
    function GrabNextElement(var s, el: string): Boolean;
    function IsSwitch(const AEl: string; var AParam: string; var AData: TSwitchData): Boolean;
    function MapPropertyType(const AProp: TRttiProperty): TCLPSwitchType;
    procedure ProcessAttributes(const AInstance: TObject; const AProp: TRttiProperty);
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
    function AddParameter(const AName, ADelim: string; const AData: TSwitchData): string;
    procedure AlignAndWrap(const ASl: TStringList; const AWrapAtColumn: Integer);
    function Wrap(const AName: string; const AData: TSwitchData): string;
    function LastSpaceBefore(const s: string; const AStartPos: Integer): Integer;
  public
    procedure Usage(const AParser: TCommandLineParser; const AWrapAtColumn: Integer;
      var AUsageList: TArray<string>);
  end;

var
  GGpCommandLineParser: ICommandLineParser;

{ exports }

function CommandLineParser: ICommandLineParser;
begin
  if not Assigned(GGpCommandLineParser) then
    GGpCommandLineParser := CreateCommandLineParser;

  Result := GGpCommandLineParser;
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

  if paramName <> '' then
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
  LProperty: TRttiProperty;
begin
  if SwitchType <> stBoolean then
    raise Exception.Create('TSwitchData.Enable: Not supported');

  LProperty := GetRTTIProperty;
  LProperty.SetValue(FInstance, True);
  FProvided := True;
end;

function TSwitchData.GetRTTIProperty: TRttiProperty;
var
  LContext: TRttiContext;
  LRtttiType: TRttiType;
begin
  LContext := TRttiContext.Create;
  LRtttiType := LContext.GetType(FInstance.ClassType);

  Result := LRtttiType.GetProperty(FPropertyName);
end;

function TSwitchData.GetValue: string;
var
  LProperty: TRttiProperty;
begin
  LProperty := GetRTTIProperty;

  Result := LProperty.GetValue(FInstance).AsString;
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
  LProperty: TRttiProperty;
begin
  Result := True;

  LProperty := GetRTTIProperty;

  case SwitchType of
    stString:
      LProperty.SetValue(FInstance, AValue);
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
    else
      raise Exception.Create('TSwitchData.SetValue: Not supported');
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
      FPositionals[LSwitchData.Position-1] := LSwitchData;

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

function TCommandLineParser.FindExtendableSwitch(const AEl: string; var AParam: string; var AData: TSwitchData): Boolean;
var
  kv: TPair<string, TSwitchData>;
begin
  Result := False;

  for kv in FSwitchDict do
  begin
    if AEl.StartsWith(kv.Key, True) then
    begin
      AData := kv.Value;
      AParam := AEl;
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

    el := Copy(s, 1, LPosition-1);

    Delete(s, 1, LPosition);
  end;

  Result := True;
end; { TCommandLineParser.GrabNextElement }

function TCommandLineParser.IsSwitch(const AEl: string; var AParam: string; var AData: TSwitchData): Boolean;
var
  LDelimPos: Integer;
  LMinPos: Integer;
  LName: string;
  LPd: Char;
  LSd: string;
  LTrimEl: string;
begin
  Result := False;
  AParam := '';

  LTrimEl := AEl;
  for LSd in FSwitchDelims do
    if StartsStr(LSd, LTrimEl) then
    begin
      LTrimEl := AEl;

      Delete(LTrimEl, 1, Length(LSd));

      if LTrimEl <> '' then
        Result := True;

      Break;
    end;

  if Result then //try to extract parameter data
  begin
    LName := LTrimEl;
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
      if FSwitchDict.TryGetValue(LTrimEl[1], AData) then
      begin
        AParam := LTrimEl;
        Delete(AParam, 1, 1);

        if (AParam <> '') and (AData.SwitchType = stBoolean) then //misdetection, Boolean switch cannot accept data
          AData := nil;
      end;
    end;
  end;
end;

function TCommandLineParser.MapPropertyType(const AProp: TRttiProperty): TCLPSwitchType;
begin
  case AProp.PropertyType.TypeKind of
    tkInteger, tkInt64:
      Result := stInteger;
    tkEnumeration:
      if AProp.PropertyType.Handle = TypeInfo(Boolean) then
        Result := stBoolean
      else
        raise Exception.CreateFmt(SUnsupportedPropertyType, [AProp.Name]);
    tkString, tkLString, tkWString, tkUString:
      Result := stString;
    else
      raise Exception.CreateFmt(SUnsupportedPropertyType, [AProp.Name]);
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

procedure TCommandLineParser.ProcessAttributes(const AInstance: TObject; const AProp: TRttiProperty);

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

  AddSwitch(AInstance, AProp.Name, Trim(LNName), LLongNames, MapPropertyType(AProp), LPosition,
    LOptions, LDefault, Trim(LDescription), Trim(LParamName));
end;

function TCommandLineParser.ProcessCommandLine(const ACommandData: TObject; const ACommandLine: string): Boolean;
var
  LSwitchData: TSwitchData;
  Lel: string;
  LParam: string;
  LPosition: Integer;
  Ls: string;
begin
  Result := True;

  for LSwitchData in FSwitchList do
    if LSwitchData.DefaultValue <> '' then
      LSwitchData.SetValue(LSwitchData.DefaultValue);

  LPosition := 1;
  Ls := ACommandLine;

  while GrabNextElement(Ls, Lel) do
  begin
    if IsSwitch(Lel, LParam, LSwitchData) then
    begin
      if not Assigned(LSwitchData) then
        if opIgnoreUnknownSwitches in FOptions then
          Continue //while
        else
          Exit(SetError(ekUnknownNamed, edUnknownSwitch, SUnknownSwitch, 0, Lel));

      if LSwitchData.SwitchType = stBoolean then
      begin
        if (LParam = '') or (soExtendable in LSwitchData.Options) then
        begin
          LSwitchData.Enable;

          if LParam <> '' then
            LSwitchData.ParamValue := LParam;
        end
        else
          Exit(SetError(ekInvalidData, edBooleanWithData, SBooleanSwitchCannotAcceptData, 0, Lel));
      end
      else if LParam <> '' then
        if not LSwitchData.SetValue(LParam) then
          Exit(SetError(ekInvalidData, edInvalidDataForSwitch, SInvalidDataForSwitch, 0, Lel));
    end
    else
    begin
      if (LPosition - 1) > High(FPositionals) then
        Exit(SetError(ekExtraPositional, edTooManyPositionalArguments, STooManyPositionalArguments, 0, Lel));

      LSwitchData := FPositionals[LPosition - 1];

      if soPositionRest in LSwitchData.Options then
      begin
        if not LSwitchData.AppendValue(Lel, #13, False) then
          Exit(SetError(ekInvalidData, edInvalidDataForSwitch, SInvalidDataForSwitch, 0, Lel));
      end
      else
      begin
        if not LSwitchData.SetValue(Lel) then
          Exit(SetError(ekInvalidData, edInvalidDataForSwitch, SInvalidDataForSwitch, 0, Lel));

        Inc(LPosition);
      end;
    end;
  end; //while s <> ''

  for LSwitchData in FPositionals do
    if (soRequired in LSwitchData.Options) and (not LSwitchData.Provided) then
      Exit(SetError(ekMissingPositional, edMissingRequiredParameter, SRequiredParameterWasNotProvided, LSwitchData.Position, LSwitchData.LongNames[0].LongForm));

  for LSwitchData in FSwitchlist do
    if (soRequired in LSwitchData.Options) and (not LSwitchData.Provided) then
      Exit(SetError(ekMissingNamed, edMissingRequiredSwitch, SRequiredSwitchWasNotProvided, 0, LSwitchData.LongNames[0].LongForm));
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
      ProcessAttributes(ACommandData, LProperty);
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

function TUsageFormatter.AddParameter(const AName, ADelim: string; const AData: TSwitchData): string;
begin
  if AData.SwitchType = stBoolean then
    Result := AName
  else
    Result := AName + ADelim + AData.ParamName;

  if ADelim = '' then
    Result := '-' + Result
  else
    Result := FSwitchDelims[0] + Result;
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
    LPosDel := Pos(' -', LStringValue);

    if LPosDel > LMaxPos then
      LMaxPos := LPosDel;
  end;

  LIndex := 0;

  while LIndex < ASl.Count do
  begin
    LStringValue := ASl[LIndex];
    LPosDel := Pos(' -', LStringValue);

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
        ASl[LIndex] := Copy(LStringValue, 1, LPosDel-1);
        Inc(LIndex);
      end;
    end;

    Inc(LIndex);
  end;
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
        if LSwitchData.Name <> '' then
          LName := LSwitchData.Name
        else if Length(LSwitchData.LongNames) <> 0 then
          LName := LSwitchData.LongNames[0].LongForm
        else
          LName := IntToStr(LSwitchData.Position);

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
          LName := Wrap(AddParameter(LSwitchData.Name, '', LSwitchData), LSwitchData);

        for LLongName in LSwitchData.LongNames do
        begin
          LName2 := Wrap(AddParameter(LLongName.LongForm, ':', LSwitchData), LSwitchData);

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
    LHelp.Insert(1, '');

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

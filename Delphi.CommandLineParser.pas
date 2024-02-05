unit Delphi.CommandLineParser;

{ Chages to original: https://github.com/gabr42/GpDelphiUnits - GpCommandLineParser.pas
  * Code changes are, at least some, are shared back to the original repository, as pull reguest. Primoz seems to be 
    super busy, so I need to make my own version, to subject it to our code formattuing rules. I bet is better this way.
    If someone is committed to some formatting and coding standard for decades, I bet they are not too happy to have 
    multiple standards in  their own repository.

  - Formatting changed to more standard
  - Added possibility to have default as true boolean parameter, and ability to make it false:
    - -BoolParam:False (1/0, true/false, t/f should be supported)
  - Default parameter stwiths in windows is "-" (not "/" character)
  - Some comments that _I_ think that are not needed, like version history, are removed, and some will be moved in the 
    readme.md etc...
}

interface

uses
  System.SysUtils, System.TypInfo, System.RTTI;

type
  ///	<summary>
  ///	  Specifies short (one letter) name for the switch.
  ///	</summary>
  CLPNameAttribute = class(TCustomAttribute)
  strict private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;

  ///	<summary>
  ///	  Specifies long name for the switch. If not set, property name is used
  ///	  for long name.
  ///   A short form of the long name can also be provided which must match the beginning
  ///   of the long form. In this case, parser will accept shortened versions of the
  ///   long name, but no shorter than the short form.
  ///   An example: if 'longName' = 'autotest' and 'shortForm' = 'auto' then the parser
  ///   will accept 'auto', 'autot', 'autote', 'autotes' and 'autotest', but not 'aut',
  ///   'au' and 'a'.
  ///   Multiple long names (alternate switches) can be provided for one entity.
  ///	</summary>
  CLPLongNameAttribute = class(TCustomAttribute)
  strict private
    FLongName : string;
    FShortForm: string;
  public
    constructor Create(const ALongName: string; const AShortForm: string = '');
    property LongName: string read FLongName;
    property ShortForm: string read FShortForm;
  end;

  ///	<summary>
  ///	  Specifies default value which will be used if switch is not found on
  ///	  the command line.
  ///	</summary>
  CLPDefaultAttribute = class(TCustomAttribute)
  strict private
    FDefaultValue: string;
  public
    constructor Create(const AValue: string); overload;
    property DefaultValue: string read FDefaultValue;
  end;

  ///	<summary>
  ///	  Provides switch description, used for the Usage function.
  ///	</summary>
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

  ///	<summary>
  ///	  When present, specifies that the switch is required.
  ///	</summary>
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

  ///	<summary>
  ///	  Specifies position of a positional (unnamed) switch. First positional
  ///	  switch has position 1.
  ///	</summary>
  CLPPositionAttribute = class(TCustomAttribute)
  strict private
    FPosition: Integer;
  public
    constructor Create(const APosition: Integer);
    property Position: Integer read FPosition;
  end;

  ///	<summary>
  ///	  Specifies switch that will receive a #13-delimited list of all
  ///	  positional parameters for which switch definitions don't exist.
  ///	</summary>
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

  IGpCommandLineParser = interface ['{C9B729D4-3706-46DB-A8A2-1E07E04F497B}']
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

  ///	<summary>
  ///	  Returns global parser instance. Not thread-safe.
  ///	</summary>
  function CommandLineParser: IGpCommandLineParser;

  ///	<summary>
  ///	  Create new command line parser instance. Thread-safe.
  ///	</summary>
  function CreateCommandLineParser: IGpCommandLineParser;

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
    FParamDelims : array [0..1] of char = (':', '=');

type
  TGpCommandLineParser = class(TInterfacedObject, IGpCommandLineParser)
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
  protected // used in TGpUsageFormatter
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

  TGpUsageFormatter = class
  private
    function AddParameter(const AName, ADelim: string; const AData: TSwitchData): string;
    procedure AlignAndWrap(const ASl: TStringList; const AWrapAtColumn: Integer);
    function Wrap(const AName: string; const AData: TSwitchData): string;
    function LastSpaceBefore(const s: string; const AStartPos: Integer): Integer;
  public
    procedure Usage(const AParser: TGpCommandLineParser; const AWrapAtColumn: Integer;
      var AUsageList: TArray<string>);
  end;

var
  GGpCommandLineParser: IGpCommandLineParser;

{ exports }

function CommandLineParser: IGpCommandLineParser;
begin
  if not assigned(GGpCommandLineParser) then
    GGpCommandLineParser := CreateCommandLineParser;

  Result := GGpCommandLineParser;
end;

function CreateCommandLineParser: IGpCommandLineParser;
begin
  Result := TGpCommandLineParser.Create;
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

  FPosition := position;
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
  s: string;
begin
  s := GetValue;

  if s <> '' then
    s := s + ADelim;

  if ADoQuote then
    s := s + Quote(AValue)
  else
    s := s + AValue;

  Result := SetValue(s);
end;

procedure TSwitchData.Enable;
var
  LContext: TRttiContext;
  prop: TRttiProperty;
  typ: TRttiType;
begin
  if SwitchType <> stBoolean then
    raise Exception.Create('TSwitchData.Enable: Not supported');

  LContext := TRttiContext.Create;
  typ := LContext.GetType(FInstance.ClassType);
  prop := typ.GetProperty(FPropertyName);
  prop.SetValue(FInstance, True);
  FProvided := True;
end;

function TSwitchData.GetValue: string;
var
  LContext: TRttiContext;
  prop: TRttiProperty;
  typ: TRttiType;
begin
  LContext := TRttiContext.Create;
  typ := LContext.GetType(FInstance.ClassType);
  prop := typ.GetProperty(FPropertyName);
  Result := prop.GetValue(FInstance).AsString;
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
  c: Integer;
  LContext: TRttiContext;
  iValue: Integer;
  prop: TRttiProperty;
  typ: TRttiType;
begin
  Result := True;
  LContext := TRttiContext.Create;
  typ := LContext.GetType(FInstance.ClassType);
  prop := typ.GetProperty(FPropertyName);

  case SwitchType of
    stString:
      prop.SetValue(FInstance, AValue);
    stInteger:
      begin
        Val(AValue, iValue, c);

        if c <> 0 then
          Exit(False);

        prop.SetValue(FInstance, iValue);
      end;
    stBoolean:
      begin
        if TryStrToBool(AValue, Result) then
          prop.SetValue(FInstance, Result);
      end;
    else
      raise Exception.Create('TSwitchData.SetValue: Not supported');
  end;

  FProvided := True;
end;

{ TGpCommandLineParser }

constructor TGpCommandLineParser.Create;
begin
  inherited Create;

  FSwitchList := TObjectList<TSwitchData>.Create;
  FSwitchComparer := TIStringComparer.Ordinal; //don't destroy, Ordinal returns a global singleton
  FSwitchDict := TDictionary<string,TSwitchData>.Create(FSwitchComparer);
end;

destructor TGpCommandLineParser.Destroy;
begin
  FreeAndNil(FSwitchDict);
  FreeAndNil(FSwitchList);

  inherited Destroy;
end;

procedure TGpCommandLineParser.AddSwitch(const AInstance: TObject; const APropertyName, AName: string;
  const ALongNames: TCLPLongNames; const ASwitchType: TCLPSwitchType; const APosition: Integer;
  const AOptions: TCLPSwitchOptions; const ADefaultValue, ADescription, AParamName: string);
var
  data: TSwitchData;
  LIndex: Integer;
  longName: TCLPLongName;
begin
  data := TSwitchData.Create(AInstance, APropertyName, AName, ALongNames, ASwitchType,
    APosition, AOptions, ADefaultValue, ADescription, AParamName);

  FSwitchList.Add(data);

  if AName <> '' then
    FSwitchDict.Add(AName, data);

  for longName in ALongNames do
  begin
    FSwitchDict.Add(longName.LongForm, data);

    if longName.ShortForm <> '' then
      for LIndex := Length(longName.ShortForm) to Length(longName.LongForm) - 1 do
        FSwitchDict.AddOrSetValue(Copy(longName.LongForm, 1, LIndex), data);
  end;
end;

///	<summary>
///	  Verifies attribute consistency.
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
///	</summary>
function TGpCommandLineParser.CheckAttributes: Boolean;
var
  data: TSwitchData;
  hasOptional: Boolean;
  highPos: Integer;
  LIndex: Integer;
  longName: TCLPLongName;
  positionRest: TSwitchData;
begin
  Result := True;

  highPos := 0;
  hasOptional := False;
  positionRest := nil;

  for data in FSwitchList do
    if soPositional in data.Options then
    begin
      if soPositionRest in data.Options then
      begin
        if assigned(positionRest) then
          Exit(SetError(ekPositionalsBadlyDefined, edExtraCLPPositionRest, SOnlyOneCLPPositionRestPropertyIs, 0, data.PropertyName))
        else if data.SwitchType <> stString then
          Exit(SetError(ekPositionalsBadlyDefined, edCLPPositionRestNotString, STypeOfACLPPositionRestPropertyMu, 0, data.PropertyName))
        else
          positionRest := data;
      end
      else
      begin
        if data.Position <= 0 then
          Exit(SetError(ekPositionalsBadlyDefined, edPositionNotPositive, SPositionMustBeGreaterOrEqualTo1, data.Position))
        else if data.Position > highPos then
          highPos := data.Position;
      end;

      if not (soRequired in data.Options) then
        hasOptional := False
      else if hasOptional then
        Exit(SetError(ekPositionalsBadlyDefined, edRequiredAfterOptional, SRequiredPositionalParametersMust, data.Position));
    end;

  if assigned(positionRest) then
  begin
    Inc(highPos);
    positionRest.Position := highPos;
  end;

  if highPos = 0 then
    Exit(True);

  SetLength(FPositionals, highPos);
  for LIndex := Low(FPositionals) to High(FPositionals) do
    FPositionals[LIndex] := nil;

  for data in FSwitchList do
    if soPositional in data.Options then
      FPositionals[data.Position-1] := data;

  for LIndex := Low(FPositionals) to High(FPositionals) do
    if FPositionals[LIndex] = nil then
      Exit(SetError(ekPositionalsBadlyDefined, edMissingPositionalDefinition, SMissingPositionalParameterDefini, LIndex + 1));

  for data in FSwitchList do
    if not (soPositional in data.Options) then
      if (data.Name = '') and (Length(data.LongNames) = 0) then
        Exit(SetError(ekNameNotDefined, edMissingNameForProperty, SMissingNameForProperty, 0, data.PropertyName))
      else if (data.Name <> '') and (Length(data.Name) <> 1) then
        Exit(SetError(ekShortNameTooLong, edShortNameTooLong, SShortNameMustBeOneLetterLong, 0, data.Name))
      else for longName in data.LongNames do
        if (longName.ShortForm <> '') and (not StartsText(longName.ShortForm, longName.LongForm)) then
          Exit(SetError(ekLongFormsDontMatch, edLongFormsDontMatch, SLongFormsDontMatch, 0, longName.LongForm));
end;

function TGpCommandLineParser.FindExtendableSwitch(const AEl: string; var AParam: string; var AData: TSwitchData): Boolean;
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

function TGpCommandLineParser.GetCommandLine: string;
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

function TGpCommandLineParser.GetErrorInfo: TCLPErrorInfo;
begin
  Result := FErrorInfo;
end;

function TGpCommandLineParser.GetExtension(const ASwitch: string): string;
var
  LData: TSwitchData;
begin
  if not FSwitchDict.TryGetValue(ASwitch, LData) then
    raise Exception.CreateFmt('Switch %s is not defined', [ASwitch]);

  if not (soExtendable in LData.Options) then
    raise Exception.CreateFmt('Switch %s is not extendable', [ASwitch]);

  Result := LData.ParamValue;
end;

function TGpCommandLineParser.GetOptions: TCLPOptions;
begin
  Result := FOptions;
end;

function TGpCommandLineParser.GrabNextElement(var s, el: string): Boolean;
var
  p: Integer;
begin
  el := '';
  s := TrimLeft(s);

  if s = '' then
    Exit(False);

  if s[1] = '"' then
  begin
    repeat
      p := PosEx('"', s, 2);

      if p <= 0 then //unterminated quote
        p := Length(s);

      el := el + Copy(s, 1, p);
      Delete(s, 1, p);
    until (s = '') or (s[1] <> '"');

    Delete(el, 1, 1);

    if el[Length(el)] = '"' then
      Delete(el, Length(el), 1);

    el := StringReplace(el, '""', '"', [rfReplaceAll]);
  end
  else begin
    p := Pos(' ', s);

    if p <= 0 then //last element
      p := Length(s) + 1;

    el := Copy(s, 1, p-1);

    Delete(s, 1, p);
  end;

  Result := True;
end; { TGpCommandLineParser.GrabNextElement }

function TGpCommandLineParser.IsSwitch(const AEl: string; var AParam: string; var AData: TSwitchData): Boolean;
var
  delimPos: Integer;
  minPos  : Integer;
  name    : string;
  pd      : char;
  sd      : string;
  trimEl  : string;
begin
  Result := False;
  AParam := '';

  trimEl := AEl;
  for sd in FSwitchDelims do
    if StartsStr(sd, trimEl) then
    begin
      trimEl := AEl;

      Delete(trimEl, 1, Length(sd));

      if trimEl <> '' then
        Result := True;

      Break;
    end;

  if Result then //try to extract parameter data
  begin
    Name := trimEl;
    minPos := 0;

    for pd in FParamDelims do
    begin
      delimPos := Pos(pd, name);

      if (delimPos > 0) and ((minPos = 0) or (delimPos < minPos)) then
        minPos := delimPos;
    end;

    if minPos > 0 then
    begin
      AParam := name;
      Delete(AParam, 1, minPos);
      name := Copy(name, 1, minPos - 1);
    end;

    FSwitchDict.TryGetValue(name, AData);

    if not assigned(AData) then //try extendable switches
      FindExtendableSwitch(name, AParam, AData);

    if not assigned(AData) then //try short name
    begin
      if FSwitchDict.TryGetValue(trimEl[1], AData) then
      begin
        AParam := trimEl;
        Delete(AParam, 1, 1);

        if (AParam <> '') and (AData.SwitchType = stBoolean) then //misdetection, Boolean switch cannot accept data
          AData := nil;
      end;
    end;
  end;
end;

function TGpCommandLineParser.MapPropertyType(const AProp: TRttiProperty): TCLPSwitchType;
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

function TGpCommandLineParser.Parse(const ACommandLine: string; const ACommandData: TObject): Boolean;
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

function TGpCommandLineParser.Parse(const ACommandData: TObject): Boolean;
begin
  Result := Parse(GetCommandLine, ACommandData);
end;

procedure TGpCommandLineParser.ProcessAttributes(const AInstance: TObject; const AProp: TRttiProperty);
var
  attr       : TCustomAttribute;
  default    : string;
  description: string;
  longNames  : TCLPLongNames;
  name       : string;
  options    : TCLPSwitchOptions;
  paramName  : string;
  position   : Integer;

  procedure AddLongName(const longForm, shortForm: string);
  begin
    SetLength(longNames, Length(longNames) + 1);
    longNames[High(longNames)] := TCLPLongName.Create(longForm, shortForm);
  end;

begin
  name := '';
  description := '';
  paramName := CLPDescriptionAttribute.DefaultValue;
  options := [];
  position := 0;

  SetLength(longNames, 0);

  for attr in AProp.GetAttributes do
  begin
    if attr is CLPNameAttribute then
      name := CLPNameAttribute(attr).Name
    else if attr is CLPLongNameAttribute then
    begin
      AddLongName(CLPLongNameAttribute(attr).LongName, CLPLongNameAttribute(attr).ShortForm);
    end
    else if attr is CLPDefaultAttribute then
      default := CLPDefaultAttribute(attr).DefaultValue
    else if attr is CLPDescriptionAttribute then
    begin
      description := CLPDescriptionAttribute(attr).Description;
      paramName := CLPDescriptionAttribute(attr).paramName;
    end
    else if attr is CLPRequiredAttribute then
      Include(options, soRequired)
    else if attr is CLPExtendableAttribute then
      Include(options, soExtendable)
    else if attr is CLPPositionAttribute then begin
      position := CLPPositionAttribute(attr).Position;
      Include(options, soPositional);
    end
    else if attr is CLPPositionRestAttribute then begin
      Include(options, soPositional);
      Include(options, soPositionRest);
    end;
  end; //for attr

  if (Length(longNames) = 0) and (not SameText(AProp.Name, Trim(Name))) then
    AddLongName(AProp.Name, '');

  AddSwitch(AInstance, AProp.Name, Trim(name), LongNames, MapPropertyType(AProp), Position,
    Options, Default, Trim(Description), Trim(ParamName));
end;

function TGpCommandLineParser.ProcessCommandLine(const ACommandData: TObject; const ACommandLine: string): Boolean;
var
  data: TSwitchData;
  el: string;
  param: string;
  position: Integer;
  s: string;
begin
  Result := True;

  for data in FSwitchList do
    if data.DefaultValue <> '' then
      data.SetValue(data.DefaultValue);

  position := 1;
  s := ACommandLine;

  while GrabNextElement(s, el) do
  begin
    if IsSwitch(el, param, data) then
    begin
      if not assigned(data) then
        if opIgnoreUnknownSwitches in FOptions then
          continue //while
        else
          Exit(SetError(ekUnknownNamed, edUnknownSwitch, SUnknownSwitch, 0, el));

      if data.SwitchType = stBoolean then
      begin
        if (param = '') or (soExtendable in data.Options) then
        begin
          data.Enable;

          if param <> '' then
            data.ParamValue := param;
        end
        else
          Exit(SetError(ekInvalidData, edBooleanWithData, SBooleanSwitchCannotAcceptData, 0, el));
      end
      else if param <> '' then
        if not data.SetValue(param) then
          Exit(SetError(ekInvalidData, edInvalidDataForSwitch, SInvalidDataForSwitch, 0, el));
    end
    else
    begin
      if (Position - 1) > High(FPositionals) then
        Exit(SetError(ekExtraPositional, edTooManyPositionalArguments, STooManyPositionalArguments, 0, el));

      data := FPositionals[position-1];

      if soPositionRest in data.Options then
      begin
        if not data.AppendValue(el, #13, False) then
          Exit(SetError(ekInvalidData, edInvalidDataForSwitch, SInvalidDataForSwitch, 0, el));
      end
      else
      begin
        if not data.SetValue(el) then
          Exit(SetError(ekInvalidData, edInvalidDataForSwitch, SInvalidDataForSwitch, 0, el));

        Inc(position);
      end;
    end;
  end; //while s <> ''

  for data in FPositionals do
    if (soRequired in data.Options) and (not data.Provided) then
      Exit(SetError(ekMissingPositional, edMissingRequiredParameter, SRequiredParameterWasNotProvided, data.Position, data.LongNames[0].LongForm));

  for data in FSwitchlist do
    if (soRequired in data.Options) and (not data.Provided) then
      Exit(SetError(ekMissingNamed, edMissingRequiredSwitch, SRequiredSwitchWasNotProvided, 0, data.LongNames[0].LongForm));
end;

procedure TGpCommandLineParser.ProcessDefinitionClass(const ACommandData: TObject);
var
  LContext: TRttiContext;
  prop: TRttiProperty;
  typ: TRttiType;
begin
  LContext := TRttiContext.Create;
  typ := LContext.GetType(ACommandData.ClassType);

  for prop in typ.GetProperties do
    if prop.Parent = typ then
      ProcessAttributes(ACommandData, prop);
end;

function TGpCommandLineParser.SetError(const AKind: TCLPErrorKind; const ADetail: TCLPErrorDetailed;
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

procedure TGpCommandLineParser.SetOptions(const AValue: TCLPOptions);
begin
  FOptions := AValue;
end;

function TGpCommandLineParser.Usage(const AWrapAtColumn: Integer): TArray<string>;
var
  formatter: TGpUsageFormatter;
begin
  formatter := TGpUsageFormatter.Create;
  try
    formatter.Usage(Self, AWrapAtColumn, Result);
  finally
    FreeAndNil(formatter);
  end;
end;

{ TGpUsageFormatter }

function TGpUsageFormatter.AddParameter(const AName, ADelim: string; const AData: TSwitchData): string;
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

procedure TGpUsageFormatter.AlignAndWrap(const ASl: TStringList; const AWrapAtColumn: Integer);
var
  LIndex: Integer;
  maxPos: Integer;
  posDel: Integer;
  s: string;
begin
  maxPos := 0;

  for s in ASl do
  begin
    posDel := Pos(' -', s);

    if posDel > maxPos then
      maxPos := posDel;
  end;

  LIndex := 0;

  while LIndex < ASl.Count do
  begin
    s := ASl[LIndex];
    posDel := Pos(' -', s);

    if (posDel > 0) and (posDel < maxPos) then
    begin
      Insert(StringOfChar(' ', maxPos - posDel), s, posDel);
      ASl[LIndex] := s;
    end;

    if Length(s) >= AWrapAtColumn then
    begin
      posDel := LastSpaceBefore(s, AWrapAtColumn);

      if posDel > 0 then
      begin
        ASl.Insert(LIndex + 1, StringOfChar(' ', maxPos + 2) + Copy(s, posDel + 1, Length(s) - posDel));
        ASl[LIndex] := Copy(s, 1, posDel-1);
        Inc(LIndex);
      end;
    end;

    Inc(LIndex);
  end;
end;

function TGpUsageFormatter.LastSpaceBefore(const s: string; const AStartPos: Integer): Integer;
begin
  Result := AStartPos - 1;

  while (Result > 0) and (s[Result] <> ' ') do
    Dec(Result);
end;

procedure TGpUsageFormatter.Usage(const AParser: TGpCommandLineParser; const AWrapAtColumn: Integer;
  var AUsageList: TArray<string>);
var
  addedOptions: Boolean;
  cmdLine: string;
  data: TSwitchData;
  help: TStringList;
  longName: TCLPLongName;
  name: string;
  name2: string;
begin
  help := TStringList.Create;
  try
    cmdLine := ExtractFileName(ParamStr(0));

    for data in AParser.Positionals do
    begin
      if not assigned(data) then //error in definition class
        help.Add('*** missing ***')
      else
      begin
        if data.Name <> '' then
          name := data.Name
        else if Length(data.LongNames) <> 0 then
          name := data.LongNames[0].LongForm
        else
          name := IntToStr(data.Position);

        cmdLine := cmdLine + ' ' + Wrap(name, data);
        help.Add(Format('%s - %s', [Wrap(name, data), data.Description]));
      end;
    end; //for data in FPositionals

    addedOptions := False;

    for data in AParser.SwitchList do
    begin
      if not (soPositional in data.Options) then
      begin
        if not addedOptions then
        begin
          cmdLine := cmdLine + ' ' + SOptions;
          addedOptions := True;
        end;

        name := '';

        if data.Name <> '' then
          name := Wrap(AddParameter(data.Name, '', data), data);

        for longName in data.LongNames do
        begin
          name2 := Wrap(AddParameter(longName.LongForm, ':', data), data);

          if name <> '' then
            name := name + ', ';

          name := name + name2;
        end;

        name := name + ' - ' + data.Description;

        if data.DefaultValue <> '' then
          name := name + SDefault + data.DefaultValue;

        help.Add(name);
      end;
    end;

    if AWrapAtColumn > 0 then
      AlignAndWrap(help, AWrapAtColumn);

    help.Insert(0, cmdLine);
    help.Insert(1, '');

    AUsageList := help.ToStringArray;
  finally
    FreeAndNil(help);
  end;
end;

function TGpUsageFormatter.Wrap(const AName: string; const AData: TSwitchData): string;
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

unit Delphi.CommandLineParser.UsageTests.DUnitX;

{
  WHY THIS UNIT EXISTS
  --------------------
  Usage rendering used to be effectively untested, which is how a parsing
  change (promoting '--' to the first switch delimiter) silently degraded the
  help output.

  The rendering contract is locked in two layers:

    1. The pure, state-free text helpers (CLPLastSpaceBefore, CLPWordWrapLine,
       CLPFormatTwoColumnBlock, CLPWrapTokens) are tested directly with
       hand-built inputs, so the tricky wrapping logic is verified independently
       of the parser and of ParamStr(0).

    2. Integration tests drive the full Usage() output for real definition
       classes and assert the properties that actually matter for the current,
       STACKED layout:
         <exe> <switch> <switch> ...        (prototype, wrapped on token
           <switch> ...                      boundaries, continuation indent 2)

           <label>                           (each switch, indent 2)
             - <description>                 (indent 4, wrapped, cont indent 6)
               default: <value>              (own line, indent 6, only if set)

  Note: Usage()'s first line embeds the executable name (ParamStr(0)), which is
  not deterministic under the test runner, so the integration tests assert on
  CONTENT and on indentation rather than exact equality.
}

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  [TestFixture]
  TUsageHelperTests = class(TObject)
  public
    // CLPLastSpaceBefore
    [Test] procedure LastSpaceBefore_FindsSpaceBeforePosition;
    [Test] procedure LastSpaceBefore_ReturnsZeroWhenNoSpace;
    [Test] procedure LastSpaceBefore_ClampsWhenStartBeyondLength;

    // CLPWordWrapLine
    [Test] procedure WordWrap_ShortLineUnchanged;
    [Test] procedure WordWrap_BreaksAtSpace;
    [Test] procedure WordWrap_AppliesHangingIndent;
    [Test] procedure WordWrap_LongWordNotSplit;
    [Test] procedure WordWrap_DisabledWhenColumnZero;

    // CLPFormatTwoColumnBlock
    [Test] procedure TwoColumn_PadsLeftToWidestCell;
    [Test] procedure TwoColumn_DescriptionsAlignAcrossRows;
    [Test] procedure TwoColumn_EmptyRightEmitsLeftOnly;

    // CLPWrapTokens
    [Test] procedure WrapTokens_PacksUntilColumn;
    [Test] procedure WrapTokens_ContinuationIsIndented;
    [Test] procedure WrapTokens_LongTokenKeptIntactOnOwnLine;
    [Test] procedure WrapTokens_DisabledWhenColumnZero;
  end;

  [TestFixture]
  TUsageIntegrationTests = class(TObject)
  public
    [Test] procedure LongNameUsesSingleDash;
    [Test] procedure ShortNameUsesSingleDash;
    [Test] procedure BooleanFlagHasNoValueSuffix;
    [Test] procedure NonBooleanShowsValueSuffix;
    [Test] procedure RequiredSwitchNotBracketedOptionalIs;
    // Compact (single-line) layout when everything fits the column.
    [Test] procedure ShortSwitchesUseCompactSingleLine;
    [Test] procedure CompactModeAppendsDefaultInline;
    // Stacked layout (forced with a narrow column).
    [Test] procedure NarrowColumnSwitchesToStacked;
    [Test] procedure SwitchLabelIsIndentedTwoSpaces;
    [Test] procedure DescriptionIsOnIndentedMarkerLine;
    [Test] procedure DefaultIsRenderedOnItsOwnLine;
    [Test] procedure DescriptionLineHasNoDefaultSuffix;
    [Test] procedure SkippedPropertyAbsent;
    [Test] procedure PositionalNotListedTwiceInPrototype;
    [Test] procedure PrototypeWrapsOnTokenBoundaries;
  end;

implementation

uses
  System.SysUtils, System.StrUtils;

{ ---- helpers ------------------------------------------------------------- }

// Index (0-based) of the first line in AUsage that contains ANeedle, or -1.
function IndexOfLineContaining(const AUsage: TArray<string>; const ANeedle: string): Integer;
begin
  for var I := 0 to High(AUsage) do
    if ContainsStr(AUsage[I], ANeedle) then
      Exit(I);
  Result := -1;
end;

// The prototype block is everything before the first blank separator line; the
// detail block is everything after it. These let tests target the right region
// (a switch token appears in BOTH the prototype and its detail label).
function DetailStart(const AUsage: TArray<string>): Integer;
begin
  for var I := 0 to High(AUsage) do
    if AUsage[I] = '' then
      Exit(I + 1);
  Result := Length(AUsage);
end;

function IndexOfDetailLine(const AUsage: TArray<string>; const ANeedle: string): Integer;
begin
  for var I := DetailStart(AUsage) to High(AUsage) do
    if ContainsStr(AUsage[I], ANeedle) then
      Exit(I);
  Result := -1;
end;

function PrototypeText(const AUsage: TArray<string>): string;
begin
  Result := '';
  for var I := 0 to High(AUsage) do
  begin
    if AUsage[I] = '' then
      Break;
    Result := Result + AUsage[I] + ' ';
  end;
end;

{ ---- definition classes used by the integration tests -------------------- }

type
  TFlagsAndValues = class
  strict private
    FVerbose: Boolean;
    FCount: Integer;
  public
    [CLPName('v'), CLPLongName('Verbose'), CLPDescription('Verbose output', '<bool>')]
    property Verbose: Boolean read FVerbose write FVerbose;
    [CLPLongName('Count'), CLPDescription('Iteration count', '<n>'), CLPDefault('1')]
    property Count: Integer read FCount write FCount;
  end;

  TRequiredAndOptional = class
  strict private
    FInput: string;
    FOutput: string;
  public
    [CLPLongName('Input'), CLPDescription('Input file', '<file>'), CLPRequired]
    property Input: string read FInput write FInput;
    [CLPLongName('Output'), CLPDescription('Output file', '<file>'), CLPDefault('')]
    property Output: string read FOutput write FOutput;
  end;

  TWithSkip = class
  strict private
    FName: string;
    FHelper: string;
  public
    [CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPDefault('')]
    property Name: string read FName write FName;
    [CLPSkip]
    property Helper: string read FHelper write FHelper;
  end;

  TPositionalAndSwitch = class
  strict private
    FSource: string;
    FForce: Boolean;
  public
    [CLPPosition(1), CLPRequired, CLPDescription('Source', '<src>')]
    property Source: string read FSource write FSource;
    [CLPLongName('Force'), CLPDescription('Overwrite', '<bool>')]
    property Force: Boolean read FForce write FForce;
  end;

  TManySwitches = class
  strict private
    FA, FB, FC, FD, FE, FF: string;
  public
    [CLPLongName('AlphaOne'), CLPDescription('a', '<v>')] property A: string read FA write FA;
    [CLPLongName('BetaTwo'), CLPDescription('b', '<v>')] property B: string read FB write FB;
    [CLPLongName('GammaThree'), CLPDescription('c', '<v>')] property C: string read FC write FC;
    [CLPLongName('DeltaFour'), CLPDescription('d', '<v>')] property D: string read FD write FD;
    [CLPLongName('EpsilonFive'), CLPDescription('e', '<v>')] property E: string read FE write FE;
    [CLPLongName('ZetaSix'), CLPDescription('f', '<v>')] property F: string read FF write FF;
  end;

{ ===== TUsageHelperTests ================================================== }

procedure TUsageHelperTests.LastSpaceBefore_FindsSpaceBeforePosition;
begin
  Assert.AreEqual(4, CLPLastSpaceBefore('abc def', 6));
end;

procedure TUsageHelperTests.LastSpaceBefore_ReturnsZeroWhenNoSpace;
begin
  Assert.AreEqual(0, CLPLastSpaceBefore('abcdef', 5));
end;

procedure TUsageHelperTests.LastSpaceBefore_ClampsWhenStartBeyondLength;
begin
  Assert.AreEqual(4, CLPLastSpaceBefore('abc def', 999));
end;

procedure TUsageHelperTests.WordWrap_ShortLineUnchanged;
begin
  var LLines := CLPWordWrapLine('short line', 80, 0);
  Assert.AreEqual(1, Length(LLines));
  Assert.AreEqual('short line', LLines[0]);
end;

procedure TUsageHelperTests.WordWrap_BreaksAtSpace;
begin
  var LLines := CLPWordWrapLine('aaaa bbbb', 5, 0);
  Assert.AreEqual(2, Length(LLines));
  Assert.AreEqual('aaaa', LLines[0]);
  Assert.AreEqual('bbbb', LLines[1]);
end;

procedure TUsageHelperTests.WordWrap_AppliesHangingIndent;
begin
  var LLines := CLPWordWrapLine('aaaa bbbb', 5, 3);
  Assert.AreEqual(2, Length(LLines));
  Assert.AreEqual('aaaa', LLines[0]);
  Assert.AreEqual('   bbbb', LLines[1]);
end;

procedure TUsageHelperTests.WordWrap_LongWordNotSplit;
begin
  var LLines := CLPWordWrapLine('superlongword', 5, 0);
  Assert.AreEqual(1, Length(LLines));
  Assert.AreEqual('superlongword', LLines[0]);
end;

procedure TUsageHelperTests.WordWrap_DisabledWhenColumnZero;
begin
  var LLines := CLPWordWrapLine('aaaa bbbb cccc', 0, 0);
  Assert.AreEqual(1, Length(LLines));
  Assert.AreEqual('aaaa bbbb cccc', LLines[0]);
end;

procedure TUsageHelperTests.TwoColumn_PadsLeftToWidestCell;
begin
  var LLeft: TArray<string> := ['a', 'bbb'];
  var LRight: TArray<string> := ['one', 'two'];
  var LLines := CLPFormatTwoColumnBlock(LLeft, LRight, ' - ', 0);

  Assert.AreEqual(2, Length(LLines));
  Assert.AreEqual('a   - one', LLines[0]);
  Assert.AreEqual('bbb - two', LLines[1]);
end;

procedure TUsageHelperTests.TwoColumn_DescriptionsAlignAcrossRows;
begin
  var LLeft: TArray<string> := ['x', 'longerleft'];
  var LRight: TArray<string> := ['desc one', 'desc two'];
  var LLines := CLPFormatTwoColumnBlock(LLeft, LRight, ' - ', 0);

  Assert.AreEqual(Pos(' - ', LLines[0]), Pos(' - ', LLines[1]),
    'description separator must be column-aligned');
end;

procedure TUsageHelperTests.TwoColumn_EmptyRightEmitsLeftOnly;
begin
  var LLeft: TArray<string> := ['onlyleft'];
  var LRight: TArray<string> := [''];
  var LLines := CLPFormatTwoColumnBlock(LLeft, LRight, ' - ', 0);

  Assert.AreEqual(1, Length(LLines));
  Assert.AreEqual('onlyleft', LLines[0]);
end;

procedure TUsageHelperTests.WrapTokens_PacksUntilColumn;
begin
  // col 9: 'aaaa'(4) + ' ' + 'bbbb'(4) = 9 fits; adding 'cccc' would reach 14.
  var LTokens: TArray<string> := ['aaaa', 'bbbb', 'cccc'];
  var LLines := CLPWrapTokens(LTokens, 9, 2);

  Assert.AreEqual(2, Length(LLines));
  Assert.AreEqual('aaaa bbbb', LLines[0]);
  Assert.AreEqual('  cccc', LLines[1]);
end;

procedure TUsageHelperTests.WrapTokens_ContinuationIsIndented;
begin
  var LTokens: TArray<string> := ['one', 'two', 'three'];
  var LLines := CLPWrapTokens(LTokens, 5, 2);

  // Every line after the first must start with the 2-space continuation indent.
  Assert.IsTrue(Length(LLines) >= 2, 'should have wrapped');
  for var I := 1 to High(LLines) do
    Assert.IsTrue(StartsStr('  ', LLines[I]), 'continuation line must be indented');
end;

procedure TUsageHelperTests.WrapTokens_LongTokenKeptIntactOnOwnLine;
begin
  // A token wider than the column must never be split: it lands intact on its
  // own line.
  var LTokens: TArray<string> := ['ab', 'superlongtoken', 'cd'];
  var LLines := CLPWrapTokens(LTokens, 6, 2);

  Assert.AreEqual(3, Length(LLines));
  Assert.AreEqual('ab', LLines[0]);
  Assert.AreEqual('  superlongtoken', LLines[1]);
  Assert.AreEqual('  cd', LLines[2]);
end;

procedure TUsageHelperTests.WrapTokens_DisabledWhenColumnZero;
begin
  var LTokens: TArray<string> := ['a', 'b', 'c'];
  var LLines := CLPWrapTokens(LTokens, 0, 2);

  Assert.AreEqual(1, Length(LLines));
  Assert.AreEqual('a b c', LLines[0]);
end;

{ ===== TUsageIntegrationTests ============================================= }

procedure TUsageIntegrationTests.LongNameUsesSingleDash;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage;

    var LIdx := IndexOfDetailLine(LUsage, '-Count');
    Assert.IsTrue(LIdx >= 0, 'Count label row should exist');
    Assert.IsTrue(ContainsStr(LUsage[LIdx], '-Count:<n>'), 'long name present');
    Assert.IsFalse(ContainsStr(LUsage[LIdx], '--Count'),
      'long name should render with a single dash');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.ShortNameUsesSingleDash;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage;

    var LIdx := IndexOfDetailLine(LUsage, 'Verbose');
    Assert.IsTrue(LIdx >= 0, 'Verbose label row should exist');
    Assert.IsTrue(ContainsStr(LUsage[LIdx], '-v'), 'short name should be present');
    Assert.IsFalse(ContainsStr(LUsage[LIdx], '--v'), 'short name must use a single dash');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.BooleanFlagHasNoValueSuffix;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage;

    var LIdx := IndexOfDetailLine(LUsage, 'Verbose');
    Assert.IsTrue(LIdx >= 0, 'Verbose label row should exist');
    // A boolean flag is a bare switch; it must not advertise a ':<value>' arg.
    Assert.IsFalse(ContainsStr(LUsage[LIdx], ':'),
      'boolean flag should not show a value suffix');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.NonBooleanShowsValueSuffix;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage;

    var LIdx := IndexOfDetailLine(LUsage, '-Count');
    Assert.IsTrue(ContainsStr(LUsage[LIdx], '-Count:<n>'),
      'non-boolean switch should show its value placeholder');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.RequiredSwitchNotBracketedOptionalIs;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRequiredAndOptional.Create;
  try
    LParser.Parse('-Input:x', LOpts);
    var LProto := PrototypeText(LParser.Usage);

    Assert.IsTrue(ContainsStr(LProto, '-Input:<file>'), 'required switch present');
    Assert.IsFalse(ContainsStr(LProto, '[-Input'), 'required switch must NOT be bracketed');
    Assert.IsTrue(ContainsStr(LProto, '[-Output:<file>]'), 'optional switch must be bracketed');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.ShortSwitchesUseCompactSingleLine;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    // Wide column: every entry fits on one line, so the compact (original)
    // two-column layout is used - label, description and default share a line.
    var LUsage := LParser.Usage(80);

    var LIdx := IndexOfDetailLine(LUsage, '-Count:<n>');
    Assert.IsTrue(LIdx >= 0, 'Count row should exist');
    Assert.IsTrue(ContainsStr(LUsage[LIdx], 'Iteration count'),
      'description should be on the same line as the label');
    Assert.IsTrue(ContainsStr(LUsage[LIdx], 'default: 1'),
      'default should be on the same line as the label');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.CompactModeAppendsDefaultInline;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage(80);

    // Compact mode appends the default inline (', default: ...'); it must NOT
    // emit a separate, indented default line.
    Assert.AreEqual(-1, IndexOfDetailLine(LUsage, '      default:'),
      'compact mode must not emit a standalone default line');
    Assert.IsTrue(IndexOfDetailLine(LUsage, ', default: 1') >= 0,
      'compact mode should append the default inline');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.NarrowColumnSwitchesToStacked;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);

    // Wide column -> compact: the default stays on the label line.
    var LWide := LParser.Usage(80);
    var LWideIdx := IndexOfDetailLine(LWide, '-Count:<n>');
    Assert.IsTrue(ContainsStr(LWide[LWideIdx], 'default: 1'),
      'wide column should keep the default on the label line');

    // Narrow column -> stacked: the label line no longer carries the default,
    // which moves to its own indented line.
    var LNarrow := LParser.Usage(30);
    var LNarrowIdx := IndexOfDetailLine(LNarrow, '-Count:<n>');
    Assert.IsFalse(ContainsStr(LNarrow[LNarrowIdx], 'default:'),
      'narrow column should move the default off the label line');
    Assert.IsTrue(IndexOfDetailLine(LNarrow, '      default: 1') >= 0,
      'narrow column should render the default on its own indented line');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.SwitchLabelIsIndentedTwoSpaces;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    // Narrow column forces the stacked layout.
    var LUsage := LParser.Usage(30);

    var LIdx := IndexOfDetailLine(LUsage, '-Count:<n>');
    Assert.IsTrue(LIdx >= 0, 'Count label row should exist');
    // The label sits on its own line, indented exactly two spaces. Count is
    // optional, so it is bracketed (required switches are not).
    Assert.AreEqual('  [-Count:<n>]', LUsage[LIdx]);
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.DescriptionIsOnIndentedMarkerLine;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage(30); // narrow -> stacked

    var LIdx := IndexOfDetailLine(LUsage, 'Iteration count');
    Assert.IsTrue(LIdx >= 0, 'description line should exist');
    // Description hangs under the label on its own '- ' marker line, indent 4.
    Assert.AreEqual('    - Iteration count', LUsage[LIdx]);
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.DefaultIsRenderedOnItsOwnLine;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage(30); // narrow -> stacked

    var LIdx := IndexOfDetailLine(LUsage, 'default:');
    Assert.IsTrue(LIdx >= 0, 'a default value should appear on its own line');
    // Count's default is 1, indented to the continuation column (6).
    Assert.AreEqual('      default: 1', LUsage[LIdx]);
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.DescriptionLineHasNoDefaultSuffix;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage(30); // narrow -> stacked

    // In the stacked layout the default has its own line, so the description
    // line must not carry the old ', default: ...' suffix.
    var LIdx := IndexOfDetailLine(LUsage, 'Iteration count');
    Assert.IsTrue(LIdx >= 0, 'description line should exist');
    Assert.IsFalse(ContainsStr(LUsage[LIdx], 'default:'),
      'description line must not include the default value');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.SkippedPropertyAbsent;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithSkip.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage;
    Assert.AreEqual(-1, IndexOfLineContaining(LUsage, 'Helper'),
      'a CLPSkip property must never appear in usage');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.PositionalNotListedTwiceInPrototype;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TPositionalAndSwitch.Create;
  try
    LParser.Parse('src', LOpts);
    var LProto := PrototypeText(LParser.Usage);

    var LFirst := Pos('Source', LProto);
    Assert.IsTrue(LFirst > 0, 'Source should appear in the prototype');
    Assert.AreEqual(0, PosEx('Source', LProto, LFirst + 1),
      'positional must not be listed twice in the prototype');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.PrototypeWrapsOnTokenBoundaries;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TManySwitches.Create;
  try
    LParser.Parse('', LOpts);
    // Force a narrow column so the prototype must span several lines.
    var LUsage := LParser.Usage(40);

    var LProtoLineCount := DetailStart(LUsage) - 1; // lines before the blank
    Assert.IsTrue(LProtoLineCount > 1, 'prototype should wrap onto multiple lines');

    // Continuation lines are indented two spaces; no token is ever split, so
    // every '[' opens a complete '[...]' bracket on the same line.
    for var I := 1 to LProtoLineCount - 1 do
      Assert.IsTrue(StartsStr('  ', LUsage[I]),
        'prototype continuation line must be indented two spaces');
  finally
    LOpts.Free;
  end;
end;

end.

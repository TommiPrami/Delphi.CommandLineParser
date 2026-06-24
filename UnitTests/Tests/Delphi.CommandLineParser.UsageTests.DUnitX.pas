unit Delphi.CommandLineParser.UsageTests.DUnitX;

{
  WHY THIS UNIT EXISTS
  --------------------
  Usage rendering used to be effectively untested, which is how a parsing
  change (promoting '--' to the first switch delimiter) silently degraded the
  help output: the prototype line and the detail lines disagreed on the prefix,
  and column alignment stopped working because the aligner keyed off the parse
  delimiter rather than a stable layout marker.

  This unit locks the rendering contract in two layers:

    1. The pure, state-free text helpers (CLPLastSpaceBefore, CLPWordWrapLine,
       CLPFormatTwoColumnBlock) are tested directly with hand-built inputs, so
       the tricky wrapping/alignment logic is verified independently of the
       parser and of ParamStr(0).

    2. A handful of integration tests drive the full Usage() output for real
       definition classes and assert the properties that actually matter:
       consistent '--'/'-' prefixes, boolean flags WITHOUT a ':<value>' suffix,
       required vs optional bracketing, and that descriptions line up in a
       single column.

  Note: Usage()'s first line embeds the executable name (ParamStr(0)), which is
  not deterministic under the test runner, so the integration tests assert on
  CONTENT (substrings / column positions) rather than exact equality.
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
  end;

  [TestFixture]
  TUsageIntegrationTests = class(TObject)
  public
    [Test] procedure LongNamesUseDoubleDash;
    [Test] procedure ShortNameUsesSingleDash;
    [Test] procedure BooleanFlagHasNoValueSuffix;
    [Test] procedure NonBooleanShowsValueSuffix;
    [Test] procedure RequiredSwitchNotBracketedOptionalIs;
    [Test] procedure DescriptionsShareAColumn;
    [Test] procedure SkippedPropertyAbsent;
    [Test] procedure PositionalNotListedTwiceInPrototype;
  end;

implementation

uses
  System.SysUtils, System.StrUtils;

{ ---- helpers ------------------------------------------------------------- }

// Returns the index (0-based) of the line in AUsage that contains ANeedle,
// or -1. Used by the integration tests to locate a specific detail row.
function IndexOfLineContaining(const AUsage: TArray<string>; const ANeedle: string): Integer;
begin
  for var I := 0 to High(AUsage) do
    if ContainsStr(AUsage[I], ANeedle) then
      Exit(I);
  Result := -1;
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

{ ===== TUsageHelperTests ================================================== }

procedure TUsageHelperTests.LastSpaceBefore_FindsSpaceBeforePosition;
begin
  // 'abc def' -> space is at index 4. Looking from position 6 backwards.
  Assert.AreEqual(4, CLPLastSpaceBefore('abc def', 6));
end;

procedure TUsageHelperTests.LastSpaceBefore_ReturnsZeroWhenNoSpace;
begin
  Assert.AreEqual(0, CLPLastSpaceBefore('abcdef', 5));
end;

procedure TUsageHelperTests.LastSpaceBefore_ClampsWhenStartBeyondLength;
begin
  // Start position past the end must not read out of bounds; it clamps to the
  // string length and still finds the last space.
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
  // Wrap 'aaaa bbbb' at column 5 -> 'aaaa' then 'bbbb'.
  var LLines := CLPWordWrapLine('aaaa bbbb', 5, 0);
  Assert.AreEqual(2, Length(LLines));
  Assert.AreEqual('aaaa', LLines[0]);
  Assert.AreEqual('bbbb', LLines[1]);
end;

procedure TUsageHelperTests.WordWrap_AppliesHangingIndent;
begin
  // Continuation line must be prefixed with the hanging indent.
  var LLines := CLPWordWrapLine('aaaa bbbb', 5, 3);
  Assert.AreEqual(2, Length(LLines));
  Assert.AreEqual('aaaa', LLines[0]);
  Assert.AreEqual('   bbbb', LLines[1]);
end;

procedure TUsageHelperTests.WordWrap_LongWordNotSplit;
begin
  // A single token longer than the column cannot be broken: it stays intact.
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
  // Left cells 'a' and 'bbb'; with no wrapping the short one is padded so the
  // separator starts at the same column.
  var LLeft: TArray<string> := ['a', 'bbb'];
  var LRight: TArray<string> := ['one', 'two'];
  var LLines := CLPFormatTwoColumnBlock(LLeft, LRight, ' - ', 0);

  Assert.AreEqual(2, Length(LLines));
  Assert.AreEqual('a   - one', LLines[0]); // 'a' + 2 pad spaces -> width 3
  Assert.AreEqual('bbb - two', LLines[1]);
end;

procedure TUsageHelperTests.TwoColumn_DescriptionsAlignAcrossRows;
begin
  var LLeft: TArray<string> := ['x', 'longerleft'];
  var LRight: TArray<string> := ['desc one', 'desc two'];
  var LLines := CLPFormatTwoColumnBlock(LLeft, LRight, ' - ', 0);

  // The ' - ' separator must start at the same index on every row.
  Assert.AreEqual(Pos(' - ', LLines[0]), Pos(' - ', LLines[1]),
    'description separator must be column-aligned');
end;

procedure TUsageHelperTests.TwoColumn_EmptyRightEmitsLeftOnly;
begin
  var LLeft: TArray<string> := ['onlyleft'];
  var LRight: TArray<string> := [''];
  var LLines := CLPFormatTwoColumnBlock(LLeft, LRight, ' - ', 0);

  Assert.AreEqual(1, Length(LLines));
  Assert.AreEqual('onlyleft', LLines[0]); // no trailing separator
end;

{ ===== TUsageIntegrationTests ============================================= }

procedure TUsageIntegrationTests.LongNamesUseDoubleDash;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage;
    var LIdx := IndexOfLineContaining(LUsage, 'Iteration count');
    Assert.IsTrue(LIdx >= 0, 'Count row should exist');
    Assert.IsTrue(ContainsStr(LUsage[LIdx], '--Count'),
      'long name should render with a double dash');
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
    var LIdx := IndexOfLineContaining(LUsage, 'Verbose output');
    Assert.IsTrue(LIdx >= 0, 'Verbose row should exist');
    // Short form '-v' present, and NOT mangled into a double dash '--v'.
    Assert.IsTrue(ContainsStr(LUsage[LIdx], '-v'), 'short name should be present');
    Assert.IsFalse(ContainsStr(LUsage[LIdx], '--v,'), 'short name must use a single dash');
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
    var LIdx := IndexOfLineContaining(LUsage, 'Verbose output');
    // A boolean flag must NOT advertise a ':<bool>' argument.
    Assert.IsFalse(ContainsStr(LUsage[LIdx], '--Verbose:'),
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
    var LIdx := IndexOfLineContaining(LUsage, 'Iteration count');
    Assert.IsTrue(ContainsStr(LUsage[LIdx], '--Count:<n>'),
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
    var LUsage := LParser.Usage;

    // The prototype line is index 0 (exe + prototype).
    var LProto := LUsage[0];
    Assert.IsTrue(ContainsStr(LProto, '--Input:<file>'), 'required switch present');
    Assert.IsFalse(ContainsStr(LProto, '[--Input'), 'required switch must NOT be bracketed');
    Assert.IsTrue(ContainsStr(LProto, '[--Output:<file>]'), 'optional switch must be bracketed');
  finally
    LOpts.Free;
  end;
end;

procedure TUsageIntegrationTests.DescriptionsShareAColumn;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFlagsAndValues.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage;

    var LVerbose := IndexOfLineContaining(LUsage, 'Verbose output');
    var LCount := IndexOfLineContaining(LUsage, 'Iteration count');
    Assert.IsTrue((LVerbose >= 0) and (LCount >= 0), 'both rows should exist');

    // Both descriptions follow the aligned ' - ' separator at the same column.
    Assert.AreEqual(Pos(' - ', LUsage[LVerbose]), Pos(' - ', LUsage[LCount]),
      'descriptions must be aligned in a single column');
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
    var LProto := LParser.Usage[0]; // prototype/summary line

    // The positional 'Source' must appear exactly once in the prototype line
    // (the old formatter listed positionals both as <Source> and as a switch).
    var LFirst := Pos('Source', LProto);
    Assert.IsTrue(LFirst > 0, 'Source should appear in the prototype');
    Assert.AreEqual(0, PosEx('Source', LProto, LFirst + 1),
      'positional must not be listed twice in the prototype');
  finally
    LOpts.Free;
  end;
end;

end.

unit Delphi.CommandLineParser.QuotingTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  [TestFixture]
  TQuotingTests = class(TObject)
  public
    [Test] procedure QuotedValueWithSpaces;
    [Test] procedure QuotedValueAttachedToSwitch;
    [Test] procedure EscapedQuoteInsideQuotedValue;
    [Test] procedure TwoQuotedTokensWithSpaceBetween;
    [Test] procedure UnterminatedQuoteAbsorbsRest;
    [Test] procedure EmptyQuotedValue;
    [Test] procedure ArrayWithSpacesInsideQuotes;
    [Test] procedure ArrayWithMultipleSeparateQuotedItems;
  end;

implementation

uses
  System.SysUtils;

type
  TQuoteOptions = class
  strict private
    FAlpha: string;
    FBeta: string;
    FItems: TArray<string>;
  public
    [CLPLongName('Alpha'), CLPDescription('Alpha', '<string>'), CLPDefault('')]
    property Alpha: string read FAlpha write FAlpha;
    [CLPLongName('Beta'), CLPDescription('Beta', '<string>'), CLPDefault('')]
    property Beta: string read FBeta write FBeta;
    [CLPLongName('Items'), CLPDescription('Items', '<list>'), CLPDefault('')]
    property Items: TArray<string> read FItems write FItems;
  end;

procedure TQuotingTests.QuotedValueWithSpaces;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TQuoteOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Alpha:"hello world"', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('hello world', LOpts.Alpha);
  finally
    LOpts.Free;
  end;
end;

procedure TQuotingTests.QuotedValueAttachedToSwitch;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TQuoteOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Alpha:"a b c" -Beta:"x y z"', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('a b c', LOpts.Alpha);
    Assert.AreEqual('x y z', LOpts.Beta);
  finally
    LOpts.Free;
  end;
end;

procedure TQuotingTests.EscapedQuoteInsideQuotedValue;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TQuoteOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Alpha:"he said ""hi"""', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('he said "hi"', LOpts.Alpha);
  finally
    LOpts.Free;
  end;
end;

procedure TQuotingTests.TwoQuotedTokensWithSpaceBetween;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TQuoteOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('"-Alpha:foo bar" "-Beta:baz qux"', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('foo bar', LOpts.Alpha);
    Assert.AreEqual('baz qux', LOpts.Beta);
  finally
    LOpts.Free;
  end;
end;

procedure TQuotingTests.UnterminatedQuoteAbsorbsRest;
begin
  // Unterminated quote should not crash; the rest of the line becomes the value.
  var LParser := CreateCommandLineParser;
  var LOpts := TQuoteOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Alpha:"value never ends', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('value never ends', LOpts.Alpha);
  finally
    LOpts.Free;
  end;
end;

procedure TQuotingTests.EmptyQuotedValue;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TQuoteOptions.Create;
  try
    LOpts.Alpha := 'preset';
    // -Alpha:"" yields empty parameter value; switch is not marked provided
    // because the parser skips the SetValue call for empty param values on
    // non-Boolean switches. Initial value should be preserved.
    Assert.IsTrue(LParser.Parse('-Alpha:""', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('preset', LOpts.Alpha);
  finally
    LOpts.Free;
  end;
end;

procedure TQuotingTests.ArrayWithSpacesInsideQuotes;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TQuoteOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Items:"item one";"item two";"item three"', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(3, Length(LOpts.Items));
    Assert.AreEqual('item one', LOpts.Items[0]);
    Assert.AreEqual('item two', LOpts.Items[1]);
    Assert.AreEqual('item three', LOpts.Items[2]);
  finally
    LOpts.Free;
  end;
end;

procedure TQuotingTests.ArrayWithMultipleSeparateQuotedItems;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TQuoteOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Items:"i 1;i 2;i 3"', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(3, Length(LOpts.Items));
    Assert.AreEqual('i 1', LOpts.Items[0]);
    Assert.AreEqual('i 2', LOpts.Items[1]);
    Assert.AreEqual('i 3', LOpts.Items[2]);
  finally
    LOpts.Free;
  end;
end;

end.

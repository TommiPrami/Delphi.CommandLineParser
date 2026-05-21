unit Delphi.CommandLineParser.SkipTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TWithSkippedProperty = class
  strict private
    FName: string;
    FHelper: string;
  public
    [CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPDefault('')]
    property Name: string read FName write FName;
    // This property should be invisible to the parser.
    [CLPSkip]
    property Helper: string read FHelper write FHelper;
  end;

  [TestFixture]
  TSkipTests = class(TObject)
  public
    [Test] procedure SkippedPropertyIsNotMatchedByName;
    [Test] procedure NonSkippedPropertyStillWorks;
    [Test] procedure SkippedPropertyDoesNotAppearInUsage;
  end;

implementation

uses
  System.SysUtils, System.StrUtils;

procedure TSkipTests.SkippedPropertyIsNotMatchedByName;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithSkippedProperty.Create;
  try
    // -Helper:x must fail because Helper is skipped, hence unknown.
    Assert.IsFalse(LParser.Parse('-Helper:x', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TSkipTests.NonSkippedPropertyStillWorks;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithSkippedProperty.Create;
  try
    Assert.IsTrue(LParser.Parse('-Name:foo', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('foo', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TSkipTests.SkippedPropertyDoesNotAppearInUsage;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TWithSkippedProperty.Create;
  try
    LParser.Parse('', LOpts);
    var LUsage := LParser.Usage;
    var LJoined := string.Join(sLineBreak, LUsage);
    Assert.IsFalse(ContainsText(LJoined, 'Helper'),
      'Skipped property should not appear in Usage output');
  finally
    LOpts.Free;
  end;
end;

end.

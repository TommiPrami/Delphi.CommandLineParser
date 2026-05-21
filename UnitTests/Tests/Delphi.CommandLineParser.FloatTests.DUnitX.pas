unit Delphi.CommandLineParser.FloatTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TFloatOptions = class
  strict private
    FRatio: Double;
    FSingle: Single;
  public
    [CLPLongName('Ratio'), CLPDescription('Ratio', '<float>'), CLPDefault('1.0')]
    property Ratio: Double read FRatio write FRatio;
    [CLPLongName('S'), CLPDescription('Single', '<float>'), CLPDefault('0.5')]
    property Single: Single read FSingle write FSingle;
  end;

  [TestFixture]
  TFloatTests = class(TObject)
  public
    [Test] procedure ParsesPositiveFloat;
    [Test] procedure ParsesNegativeFloat;
    [Test] procedure ParsesIntegerAsFloat;
    [Test] procedure UsesInvariantDecimalSeparator;
    [Test] procedure InvalidFloatRejected;
    [Test] procedure SinglePropertyAccepted;
    [Test] procedure DefaultAppliedWhenAbsent;
  end;

implementation

uses
  System.SysUtils;

procedure TFloatTests.ParsesPositiveFloat;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFloatOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Ratio:3.14', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(3.14, LOpts.Ratio, 0.0001);
  finally
    LOpts.Free;
  end;
end;

procedure TFloatTests.ParsesNegativeFloat;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFloatOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Ratio:-2.5', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(-2.5, LOpts.Ratio, 0.0001);
  finally
    LOpts.Free;
  end;
end;

procedure TFloatTests.ParsesIntegerAsFloat;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFloatOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Ratio:7', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(7.0, LOpts.Ratio, 0.0001);
  finally
    LOpts.Free;
  end;
end;

procedure TFloatTests.UsesInvariantDecimalSeparator;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFloatOptions.Create;
  try
    // Should always accept '.' regardless of the OS locale.
    Assert.IsTrue(LParser.Parse('-Ratio:1.25', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(1.25, LOpts.Ratio, 0.0001);
  finally
    LOpts.Free;
  end;
end;

procedure TFloatTests.InvalidFloatRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFloatOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Ratio:notafloat', LOpts));
    Assert.IsTrue(LParser.ErrorInfo.IsError);
  finally
    LOpts.Free;
  end;
end;

procedure TFloatTests.SinglePropertyAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFloatOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-S:0.125', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Single(0.125), LOpts.Single, Single(0.0001));
  finally
    LOpts.Free;
  end;
end;

procedure TFloatTests.DefaultAppliedWhenAbsent;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFloatOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(1.0, LOpts.Ratio, 0.0001);
  finally
    LOpts.Free;
  end;
end;

end.

unit Delphi.CommandLineParser.RangeTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TRangeOptions = class
  strict private
    FCount: Integer;
    FRatio: Double;
  public
    [CLPLongName('Count'), CLPRange(1, 100), CLPDescription('Count', '<int>'), CLPDefault('1')]
    property Count: Integer read FCount write FCount;
    [CLPLongName('Ratio'), CLPRange(0.0, 1.0), CLPDescription('Ratio', '<float>'), CLPDefault('0.5')]
    property Ratio: Double read FRatio write FRatio;
  end;

  [TestFixture]
  TRangeTests = class(TObject)
  public
    [Test] procedure IntegerInsideRangeAccepted;
    [Test] procedure IntegerAtLowerBoundAccepted;
    [Test] procedure IntegerAtUpperBoundAccepted;
    [Test] procedure IntegerBelowLowerBoundRejected;
    [Test] procedure IntegerAboveUpperBoundRejected;
    [Test] procedure FloatInsideRangeAccepted;
    [Test] procedure FloatOutsideRangeRejected;
  end;

implementation

uses
  System.SysUtils;

procedure TRangeTests.IntegerInsideRangeAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRangeOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Count:50', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(50, LOpts.Count);
  finally
    LOpts.Free;
  end;
end;

procedure TRangeTests.IntegerAtLowerBoundAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRangeOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Count:1', LOpts), LParser.ErrorInfo.Text);
  finally
    LOpts.Free;
  end;
end;

procedure TRangeTests.IntegerAtUpperBoundAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRangeOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Count:100', LOpts), LParser.ErrorInfo.Text);
  finally
    LOpts.Free;
  end;
end;

procedure TRangeTests.IntegerBelowLowerBoundRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRangeOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Count:0', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TRangeTests.IntegerAboveUpperBoundRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRangeOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Count:101', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TRangeTests.FloatInsideRangeAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRangeOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Ratio:0.7', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(0.7, LOpts.Ratio, 0.0001);
  finally
    LOpts.Free;
  end;
end;

procedure TRangeTests.FloatOutsideRangeRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRangeOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Ratio:2.5', LOpts));
  finally
    LOpts.Free;
  end;
end;

end.

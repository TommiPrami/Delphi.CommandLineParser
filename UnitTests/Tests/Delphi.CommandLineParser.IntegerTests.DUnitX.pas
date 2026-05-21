unit Delphi.CommandLineParser.IntegerTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  [TestFixture]
  TIntegerTests = class(TObject)
  public
    [Test] procedure ParsesPositiveInteger;
    [Test] procedure ParsesNegativeInteger;
    [Test] procedure ParsesZero;
    [Test] procedure ParsesHighInt;
    [Test] procedure ParsesLowInt;
    [Test] procedure InvalidIntegerReturnsFalse;
    [Test] procedure NonNumericReturnsFalse;
    [Test] procedure FloatStringRejected;
    [Test] procedure DefaultAppliedWhenAbsent;
  end;

implementation

uses
  System.SysUtils;

type
  TIntOptions = class
  strict private
    FN: Integer;
  public
    [CLPLongName('N'), CLPDescription('Number', '<int>'), CLPDefault('123')]
    property N: Integer read FN write FN;
  end;

procedure TIntegerTests.ParsesPositiveInteger;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-N:42', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(42, LOpts.N);
  finally
    LOpts.Free;
  end;
end;

procedure TIntegerTests.ParsesNegativeInteger;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-N:-99', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(-99, LOpts.N);
  finally
    LOpts.Free;
  end;
end;

procedure TIntegerTests.ParsesZero;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-N:0', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(0, LOpts.N);
  finally
    LOpts.Free;
  end;
end;

procedure TIntegerTests.ParsesHighInt;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-N:2147483647', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(High(Integer), LOpts.N);
  finally
    LOpts.Free;
  end;
end;

procedure TIntegerTests.ParsesLowInt;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-N:-2147483648', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Low(Integer), LOpts.N);
  finally
    LOpts.Free;
  end;
end;

procedure TIntegerTests.InvalidIntegerReturnsFalse;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-N:12abc', LOpts));
    Assert.IsTrue(LParser.ErrorInfo.IsError);
  finally
    LOpts.Free;
  end;
end;

procedure TIntegerTests.NonNumericReturnsFalse;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-N:hello', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TIntegerTests.FloatStringRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-N:3.14', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TIntegerTests.DefaultAppliedWhenAbsent;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIntOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(123, LOpts.N);
  finally
    LOpts.Free;
  end;
end;

end.

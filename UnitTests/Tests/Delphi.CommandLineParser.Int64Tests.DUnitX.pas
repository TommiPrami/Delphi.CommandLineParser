unit Delphi.CommandLineParser.Int64Tests.DUnitX;

{
  WHY THIS UNIT EXISTS
  --------------------
  MapPropertyType maps BOTH tkInteger and tkInt64 to the stInteger switch type.
  The original SetValue parsed the value with Val() into a 32-bit Integer local,
  which meant an Int64 property could never receive a value outside the signed
  32-bit range (-2^31 .. 2^31-1): such input was rejected as "invalid data" even
  though the property could clearly hold it.

  THE IDEA
  --------
  After the fix:
    * An Int64 property accepts the full signed 64-bit range.
    * A 32-bit Integer property still rejects values that do not fit in 32 bits
      (reported as a normal data error, NOT silently truncated/wrapped).
  These tests pin down both halves so a future refactor can't quietly
  reintroduce truncation or, conversely, start wrapping 32-bit overflow.
}

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TInt64Options = class
  strict private
    FBig: Int64;
  public
    [CLPLongName('Big'), CLPDescription('Big number', '<int64>'), CLPDefault('0')]
    property Big: Int64 read FBig write FBig;
  end;

  TInt32Options = class
  strict private
    FSmall: Integer;
  public
    [CLPLongName('Small'), CLPDescription('Small number', '<int32>'), CLPDefault('0')]
    property Small: Integer read FSmall write FSmall;
  end;

  [TestFixture]
  TInt64Tests = class(TObject)
  public
    [Test] procedure ParsesValueBeyond32BitPositive;
    [Test] procedure ParsesValueBeyond32BitNegative;
    [Test] procedure ParsesMaxInt64;
    [Test] procedure ParsesMinInt64;
    [Test] procedure SmallValueStillWorksOnInt64;
    [Test] procedure Int32PropertyRejectsValueAbove32Bit;
    [Test] procedure Int32PropertyStillAcceptsBoundaryValues;
  end;

implementation

uses
  System.SysUtils;

procedure TInt64Tests.ParsesValueBeyond32BitPositive;
begin
  // 5_000_000_000 > High(Integer) (2_147_483_647). Must succeed on an Int64.
  var LParser := CreateCommandLineParser;
  var LOpts := TInt64Options.Create;
  try
    Assert.IsTrue(LParser.Parse('-Big:5000000000', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Int64(5000000000), LOpts.Big);
  finally
    LOpts.Free;
  end;
end;

procedure TInt64Tests.ParsesValueBeyond32BitNegative;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TInt64Options.Create;
  try
    Assert.IsTrue(LParser.Parse('-Big:-5000000000', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Int64(-5000000000), LOpts.Big);
  finally
    LOpts.Free;
  end;
end;

procedure TInt64Tests.ParsesMaxInt64;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TInt64Options.Create;
  try
    Assert.IsTrue(LParser.Parse('-Big:9223372036854775807', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(High(Int64), LOpts.Big);
  finally
    LOpts.Free;
  end;
end;

procedure TInt64Tests.ParsesMinInt64;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TInt64Options.Create;
  try
    Assert.IsTrue(LParser.Parse('-Big:-9223372036854775808', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Low(Int64), LOpts.Big);
  finally
    LOpts.Free;
  end;
end;

procedure TInt64Tests.SmallValueStillWorksOnInt64;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TInt64Options.Create;
  try
    Assert.IsTrue(LParser.Parse('-Big:42', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Int64(42), LOpts.Big);
  finally
    LOpts.Free;
  end;
end;

procedure TInt64Tests.Int32PropertyRejectsValueAbove32Bit;
begin
  // A 32-bit Integer property must NOT silently accept/wrap a value that does
  // not fit. It should be reported as a data error.
  var LParser := CreateCommandLineParser;
  var LOpts := TInt32Options.Create;
  try
    Assert.IsFalse(LParser.Parse('-Small:5000000000', LOpts));
    Assert.AreEqual(Integer(ekInvalidData), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TInt64Tests.Int32PropertyStillAcceptsBoundaryValues;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TInt32Options.Create;
  try
    Assert.IsTrue(LParser.Parse('-Small:2147483647', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(High(Integer), LOpts.Small);

    Assert.IsTrue(LParser.Parse('-Small:-2147483648', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Low(Integer), LOpts.Small);
  finally
    LOpts.Free;
  end;
end;

end.

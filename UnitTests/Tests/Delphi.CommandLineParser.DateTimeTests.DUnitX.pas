unit Delphi.CommandLineParser.DateTimeTests.DUnitX;

interface

uses
  System.SysUtils, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TDateTimeOptions = class
  strict private
    FDate: TDate;
    FTime: TTime;
    FDateTime: TDateTime;
  public
    [CLPLongName('Date'), CLPDescription('Date', '<YYYY-MM-DD>'), CLPDefault('2020-01-01')]
    property Date: TDate read FDate write FDate;
    [CLPLongName('Time'), CLPDescription('Time', '<HH:MM:SS>'), CLPDefault('00:00:00')]
    property Time: TTime read FTime write FTime;
    [CLPLongName('Stamp'), CLPDescription('Timestamp', '<ISO8601>'), CLPDefault('2020-01-01T00:00:00')]
    property Stamp: TDateTime read FDateTime write FDateTime;
  end;

  [TestFixture]
  TDateTimeTests = class(TObject)
  public
    [Test] procedure ParsesISO8601Date;
    [Test] procedure ParsesTime;
    [Test] procedure ParsesISO8601DateTime;
    [Test] procedure InvalidDateRejected;
    [Test] procedure InvalidTimeRejected;
    [Test] procedure DefaultsApplied;
  end;

implementation

uses
  System.DateUtils;

procedure TDateTimeTests.ParsesISO8601Date;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDateTimeOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Date:2024-03-15', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(EncodeDate(2024, 3, 15), DateOf(LOpts.Date));
  finally
    LOpts.Free;
  end;
end;

procedure TDateTimeTests.ParsesTime;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDateTimeOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Time:14:30:45', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(EncodeTime(14, 30, 45, 0), TimeOf(LOpts.Time), 0.00001);
  finally
    LOpts.Free;
  end;
end;

procedure TDateTimeTests.ParsesISO8601DateTime;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDateTimeOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Stamp:2024-03-15T14:30:45', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(EncodeDate(2024, 3, 15), DateOf(LOpts.Stamp));
  finally
    LOpts.Free;
  end;
end;

procedure TDateTimeTests.InvalidDateRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDateTimeOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Date:not-a-date', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TDateTimeTests.InvalidTimeRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDateTimeOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Time:not-a-time', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TDateTimeTests.DefaultsApplied;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDateTimeOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(EncodeDate(2020, 1, 1), DateOf(LOpts.Date));
  finally
    LOpts.Free;
  end;
end;

end.

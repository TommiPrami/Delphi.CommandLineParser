unit Delphi.CommandLineParser.IllegalEnumValueTests.DUnitX;

interface

uses
  System.SysUtils, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TMode = (mUninitialized, mDefault, mFast, mInternal);

  TIllegalEnumOptions = class
  strict private
    FMode: TMode;
  public
    [CLPLongName('Mode'), CLPDescription('Mode', '<mode>'), CLPDefault('mDefault'),
     CLPIllegalValue('mUninitialized'), CLPIllegalValue('mInternal')]
    property Mode: TMode read FMode write FMode;
  end;

  [TestFixture]
  TIllegalEnumValueTests = class(TObject)
  public
    [Test] procedure LegalValueAccepted;
    [Test] procedure FirstIllegalValueRejected;
    [Test] procedure SecondIllegalValueRejected;
    [Test] procedure IllegalValueProducesInvalidDataError;
    [Test] procedure DefaultValueStillAppliesEvenIfIllegalElsewhere;
  end;

implementation

procedure TIllegalEnumValueTests.LegalValueAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIllegalEnumOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Mode:mFast', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Integer(mFast), Integer(LOpts.Mode));
  finally
    LOpts.Free;
  end;
end;

procedure TIllegalEnumValueTests.FirstIllegalValueRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIllegalEnumOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Mode:mUninitialized', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TIllegalEnumValueTests.SecondIllegalValueRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIllegalEnumOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Mode:mInternal', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TIllegalEnumValueTests.IllegalValueProducesInvalidDataError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TIllegalEnumOptions.Create;
  try
    LParser.Parse('-Mode:mUninitialized', LOpts);
    Assert.AreEqual(Integer(ekInvalidData), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TIllegalEnumValueTests.DefaultValueStillAppliesEvenIfIllegalElsewhere;
begin
  // The CLPDefault is 'mDefault', which is allowed. Default application
  // should succeed even though some identifiers are blacklisted.
  var LParser := CreateCommandLineParser;
  var LOpts := TIllegalEnumOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Integer(mDefault), Integer(LOpts.Mode));
  finally
    LOpts.Free;
  end;
end;

end.

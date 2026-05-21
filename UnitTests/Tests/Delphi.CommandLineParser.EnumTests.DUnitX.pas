unit Delphi.CommandLineParser.EnumTests.DUnitX;

interface

uses
  System.SysUtils, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TColor = (Red, Green, Blue);

  TEnumOptions = class
  strict private
    FColor: TColor;
  public
    [CLPLongName('Color'), CLPDescription('Color', '<Red|Green|Blue>'), CLPDefault('Red')]
    property Color: TColor read FColor write FColor;
  end;

  [TestFixture]
  TEnumTests = class(TObject)
  public
    [Test] procedure ParsesFirstEnumValue;
    [Test] procedure ParsesMiddleEnumValue;
    [Test] procedure ParsesLastEnumValue;
    [Test] procedure DefaultEnumApplied;
    [Test] procedure UnknownEnumValueFailsGracefully;
    [Test] procedure UnknownEnumValueSetsInvalidDataError;
  end;

implementation

procedure TEnumTests.ParsesFirstEnumValue;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TEnumOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Color:Red', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Integer(Red), Integer(LOpts.Color));
  finally
    LOpts.Free;
  end;
end;

procedure TEnumTests.ParsesMiddleEnumValue;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TEnumOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Color:Green', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Integer(Green), Integer(LOpts.Color));
  finally
    LOpts.Free;
  end;
end;

procedure TEnumTests.ParsesLastEnumValue;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TEnumOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Color:Blue', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Integer(Blue), Integer(LOpts.Color));
  finally
    LOpts.Free;
  end;
end;

procedure TEnumTests.DefaultEnumApplied;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TEnumOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(Integer(Red), Integer(LOpts.Color));
  finally
    LOpts.Free;
  end;
end;

procedure TEnumTests.UnknownEnumValueFailsGracefully;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TEnumOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Color:Purple', LOpts));
  finally
    LOpts.Free;
  end;
end;

procedure TEnumTests.UnknownEnumValueSetsInvalidDataError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TEnumOptions.Create;
  try
    LParser.Parse('-Color:Purple', LOpts);
    Assert.AreEqual(Integer(ekInvalidData), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

end.

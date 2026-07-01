unit Delphi.CommandLineParser.InvalidDefaultTests.DUnitX;

{
  WHY THIS UNIT EXISTS
  --------------------
  A CLPDefault value is supplied by the *developer*, not the end user. If that
  default cannot be applied to the property (wrong type, out of a declared
  CLPRange, not a valid enum identifier, ...) it is a programming/configuration
  mistake, exactly like a malformed attribute. Previously the parser called
  SetValue(default) and ignored the Boolean result, so a broken default was
  silently dropped: the property kept its zero value and Parse reported success.
  That hides real bugs.

  THE IDEA
  --------
  An invalid default must surface loudly, through the same channel as other
  configuration errors: raise ECLPConfigurationError (with Kind = ekInvalidDefault).
  A *valid* default must, of course, still apply without complaint.

  These tests lock that contract: bad defaults raise, good defaults don't.
}

interface

uses
  System.SysUtils, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  // Default '999' is outside the declared range 1..100.
  TDefaultOutOfRange = class
  strict private
    FCount: Integer;
  public
    [CLPLongName('Count'), CLPRange(1, 100), CLPDescription('Count', '<int>'), CLPDefault('999')]
    property Count: Integer read FCount write FCount;
  end;

  // Default 'notanumber' is not a valid integer.
  TDefaultWrongType = class
  strict private
    FN: Integer;
  public
    [CLPLongName('N'), CLPDescription('N', '<int>'), CLPDefault('notanumber')]
    property N: Integer read FN write FN;
  end;

  TColor = (Red, Green, Blue);

  // Default 'Purple' is not a member of TColor.
  TDefaultBadEnum = class
  strict private
    FColor: TColor;
  public
    [CLPLongName('Color'), CLPDescription('Color', '<color>'), CLPDefault('Purple')]
    property Color: TColor read FColor write FColor;
  end;

  // A perfectly valid default — must NOT raise.
  TDefaultValid = class
  strict private
    FCount: Integer;
  public
    [CLPLongName('Count'), CLPRange(1, 100), CLPDescription('Count', '<int>'), CLPDefault('50')]
    property Count: Integer read FCount write FCount;
  end;

  [TestFixture]
  TInvalidDefaultTests = class(TObject)
  public
    [Test] procedure OutOfRangeDefaultRaisesConfigError;
    [Test] procedure WrongTypeDefaultRaisesConfigError;
    [Test] procedure BadEnumDefaultRaisesConfigError;
    [Test] procedure InvalidDefaultErrorKindIsInvalidDefault;
    [Test] procedure ValidDefaultDoesNotRaise;
  end;

implementation

procedure TInvalidDefaultTests.OutOfRangeDefaultRaisesConfigError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDefaultOutOfRange.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        LParser.Parse('', LOpts);
      end,
      ECLPConfigurationError);
  finally
    LOpts.Free;
  end;
end;

procedure TInvalidDefaultTests.WrongTypeDefaultRaisesConfigError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDefaultWrongType.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        LParser.Parse('', LOpts);
      end,
      ECLPConfigurationError);
  finally
    LOpts.Free;
  end;
end;

procedure TInvalidDefaultTests.BadEnumDefaultRaisesConfigError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDefaultBadEnum.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        LParser.Parse('', LOpts);
      end,
      ECLPConfigurationError);
  finally
    LOpts.Free;
  end;
end;

procedure TInvalidDefaultTests.InvalidDefaultErrorKindIsInvalidDefault;
begin
  // The ECLPConfigurationError carries structured info; verify the Kind.
  var LParser := CreateCommandLineParser;
  var LOpts := TDefaultWrongType.Create;
  try
    try
      LParser.Parse('', LOpts);
      Assert.Fail('Expected ECLPConfigurationError for an invalid default');
    except
      on E: ECLPConfigurationError do
        Assert.AreEqual(Integer(ekInvalidDefault), Integer(E.ErrorInfo.Kind));
    end;
  finally
    LOpts.Free;
  end;
end;

procedure TInvalidDefaultTests.ValidDefaultDoesNotRaise;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDefaultValid.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(50, LOpts.Count);
  finally
    LOpts.Free;
  end;
end;

end.

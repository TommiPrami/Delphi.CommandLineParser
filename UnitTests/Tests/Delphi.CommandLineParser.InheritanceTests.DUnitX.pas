unit Delphi.CommandLineParser.InheritanceTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TBaseOptions = class
  strict private
    FBase: string;
  public
    [CLPLongName('Base'), CLPDescription('Base option', '<string>'), CLPDefault('baseDefault')]
    property Base: string read FBase write FBase;
  end;

  TDerivedOptions = class(TBaseOptions)
  strict private
    FDerived: Integer;
  public
    [CLPLongName('Derived'), CLPDescription('Derived option', '<int>'), CLPDefault('0')]
    property Derived: Integer read FDerived write FDerived;
  end;

  TThreeLevelOptions = class(TDerivedOptions)
  strict private
    FThird: Boolean;
  public
    [CLPLongName('Third'), CLPDescription('Third', '<bool>')]
    property Third: Boolean read FThird write FThird;
  end;

  [TestFixture]
  TInheritanceTests = class(TObject)
  public
    [Test] procedure BasePropertyVisibleOnDerived;
    [Test] procedure DerivedPropertyAvailable;
    [Test] procedure BothLevelsParsedTogether;
    [Test] procedure ThreeLevelsOfInheritance;
    [Test] procedure BaseDefaultStillAppliedThroughDerived;
  end;

implementation

uses
  System.SysUtils;

procedure TInheritanceTests.BasePropertyVisibleOnDerived;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDerivedOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Base:fromBase', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('fromBase', LOpts.Base);
  finally
    LOpts.Free;
  end;
end;

procedure TInheritanceTests.DerivedPropertyAvailable;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDerivedOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Derived:42', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(42, LOpts.Derived);
  finally
    LOpts.Free;
  end;
end;

procedure TInheritanceTests.BothLevelsParsedTogether;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDerivedOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Base:b -Derived:9', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('b', LOpts.Base);
    Assert.AreEqual(9, LOpts.Derived);
  finally
    LOpts.Free;
  end;
end;

procedure TInheritanceTests.ThreeLevelsOfInheritance;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TThreeLevelOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Base:b -Derived:9 -Third', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('b', LOpts.Base);
    Assert.AreEqual(9, LOpts.Derived);
    Assert.IsTrue(LOpts.Third);
  finally
    LOpts.Free;
  end;
end;

procedure TInheritanceTests.BaseDefaultStillAppliedThroughDerived;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDerivedOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('baseDefault', LOpts.Base);
    Assert.AreEqual(0, LOpts.Derived);
  finally
    LOpts.Free;
  end;
end;

end.

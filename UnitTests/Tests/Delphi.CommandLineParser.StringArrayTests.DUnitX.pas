unit Delphi.CommandLineParser.StringArrayTests.DUnitX;

interface

uses
  DUnitX.TestFramework, Delphi.CommandLineParser;

type
  [TestFixture]
  TStringArrayTests = class(TObject)
  public
    [Test] procedure SingleValueProducesOneElementArray;
    [Test] procedure SemicolonSeparatedYieldsMultipleElements;
    [Test] procedure CommaSeparatedYieldsMultipleElements;
    [Test] procedure MixedCommaSemicolonSeparators;
    [Test] procedure EmptyDefaultLeavesArrayUntouched;
    [Test] procedure AbsentSwitchKeepsInitialValue;
    [Test] procedure SecondParseOverwritesPrevious;
  end;

implementation

uses
  System.SysUtils;

type
  TArrayOptions = class
  strict private
    FItems: TArray<string>;
  public
    [CLPLongName('Items'), CLPDescription('Items', '<list>'), CLPDefault('')]
    property Items: TArray<string> read FItems write FItems;
  end;

procedure TStringArrayTests.SingleValueProducesOneElementArray;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TArrayOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Items:onlyone', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(1, Length(LOpts.Items));
    Assert.AreEqual('onlyone', LOpts.Items[0]);
  finally
    LOpts.Free;
  end;
end;

procedure TStringArrayTests.SemicolonSeparatedYieldsMultipleElements;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TArrayOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Items:a;b;c;d', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(4, Length(LOpts.Items));
    Assert.AreEqual('a', LOpts.Items[0]);
    Assert.AreEqual('b', LOpts.Items[1]);
    Assert.AreEqual('c', LOpts.Items[2]);
    Assert.AreEqual('d', LOpts.Items[3]);
  finally
    LOpts.Free;
  end;
end;

procedure TStringArrayTests.CommaSeparatedYieldsMultipleElements;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TArrayOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Items:a,b,c', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(3, Length(LOpts.Items));
    Assert.AreEqual('a', LOpts.Items[0]);
    Assert.AreEqual('b', LOpts.Items[1]);
    Assert.AreEqual('c', LOpts.Items[2]);
  finally
    LOpts.Free;
  end;
end;

procedure TStringArrayTests.MixedCommaSemicolonSeparators;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TArrayOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Items:a,b;c,d;e', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(5, Length(LOpts.Items));
  finally
    LOpts.Free;
  end;
end;

procedure TStringArrayTests.EmptyDefaultLeavesArrayUntouched;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TArrayOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(0, Length(LOpts.Items));
  finally
    LOpts.Free;
  end;
end;

procedure TStringArrayTests.AbsentSwitchKeepsInitialValue;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TArrayOptions.Create;
  try
    LOpts.Items := TArray<string>.Create('preset');
    Assert.IsTrue(LParser.Parse('', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(1, Length(LOpts.Items));
    Assert.AreEqual('preset', LOpts.Items[0]);
  finally
    LOpts.Free;
  end;
end;

procedure TStringArrayTests.SecondParseOverwritesPrevious;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TArrayOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Items:a;b', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(2, Length(LOpts.Items));

    Assert.IsTrue(LParser.Parse('-Items:x;y;z', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual(3, Length(LOpts.Items));
    Assert.AreEqual('z', LOpts.Items[2]);
  finally
    LOpts.Free;
  end;
end;

end.

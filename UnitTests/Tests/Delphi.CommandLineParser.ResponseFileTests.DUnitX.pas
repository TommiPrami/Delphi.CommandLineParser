unit Delphi.CommandLineParser.ResponseFileTests.DUnitX;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TRspOptions = class
  strict private
    FName: string;
    FCount: Integer;
    FFlag: Boolean;
  public
    [CLPLongName('Name'), CLPDescription('Name', '<string>'), CLPDefault('')]
    property Name: string read FName write FName;
    [CLPLongName('Count'), CLPDescription('Count', '<int>'), CLPDefault('0')]
    property Count: Integer read FCount write FCount;
    [CLPLongName('Flag'), CLPDescription('Flag', '<bool>')]
    property Flag: Boolean read FFlag write FFlag;
  end;

  [TestFixture]
  TResponseFileTests = class(TObject)
  private
    FTempFile: string;
    function WriteRsp(const AContents: string): string;
    procedure DeleteRsp;
  public
    [Setup] procedure SetUp;
    [TearDown] procedure TearDown;

    [Test] procedure SingleLineResponseFileExpanded;
    [Test] procedure MultiLineResponseFileExpanded;
    [Test] procedure ResponseFileWithQuotedValuesPreservesSpaces;
    [Test] procedure NonExistentResponseFileFails;
    [Test] procedure ResponseFileCombinedWithSwitchRejected;
    [Test] procedure ResponseFileCombinedWithPositionalRejected;
    [Test] procedure TwoResponseFilesRejected;
    [Test] procedure RecursiveAtTokenInsideFileRejected;
    [Test] procedure NoResponseFileLeavesCommandLineUnchanged;
    [Test] procedure ErrorKindIsResponseFile;
    // #10: response files are loaded as UTF-8 (without BOM) deterministically.
    [Test] procedure Utf8ResponseFileWithoutBomDecodedCorrectly;
  end;

implementation

function TResponseFileTests.WriteRsp(const AContents: string): string;
begin
  FTempFile := TPath.Combine(TPath.GetTempPath, 'clpparser_rsp_' + TPath.GetGUIDFileName + '.txt');
  TFile.WriteAllText(FTempFile, AContents);
  Result := FTempFile;
end;

procedure TResponseFileTests.DeleteRsp;
begin
  if (FTempFile <> '') and TFile.Exists(FTempFile) then
    TFile.Delete(FTempFile);
  FTempFile := '';
end;

procedure TResponseFileTests.SetUp;
begin
  FTempFile := '';
end;

procedure TResponseFileTests.TearDown;
begin
  DeleteRsp;
end;

procedure TResponseFileTests.SingleLineResponseFileExpanded;
begin
  var LFile := WriteRsp('-Name:hello -Count:5 -Flag');
  var LParser := CreateCommandLineParser;
  var LOpts := TRspOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('@' + LFile, LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('hello', LOpts.Name);
    Assert.AreEqual(5, LOpts.Count);
    Assert.IsTrue(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

procedure TResponseFileTests.MultiLineResponseFileExpanded;
begin
  var LFile := WriteRsp(
    '-Name:multi' + sLineBreak +
    '-Count:42' + sLineBreak +
    '-Flag');
  var LParser := CreateCommandLineParser;
  var LOpts := TRspOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('@' + LFile, LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('multi', LOpts.Name);
    Assert.AreEqual(42, LOpts.Count);
    Assert.IsTrue(LOpts.Flag);
  finally
    LOpts.Free;
  end;
end;

procedure TResponseFileTests.ResponseFileWithQuotedValuesPreservesSpaces;
begin
  var LFile := WriteRsp('-Name:"value with spaces"');
  var LParser := CreateCommandLineParser;
  var LOpts := TRspOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('@' + LFile, LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('value with spaces', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

procedure TResponseFileTests.NonExistentResponseFileFails;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRspOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('@Z:\does\not\exist_4815162342.rsp', LOpts));
    Assert.AreEqual(Integer(ekResponseFile), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TResponseFileTests.ResponseFileCombinedWithSwitchRejected;
begin
  var LFile := WriteRsp('-Name:fromfile');
  var LParser := CreateCommandLineParser;
  var LOpts := TRspOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Flag @' + LFile, LOpts));
    Assert.AreEqual(Integer(ekResponseFile), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TResponseFileTests.ResponseFileCombinedWithPositionalRejected;
begin
  var LFile := WriteRsp('-Name:fromfile');
  var LParser := CreateCommandLineParser;
  var LOpts := TRspOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('@' + LFile + ' positional', LOpts));
    Assert.AreEqual(Integer(ekResponseFile), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TResponseFileTests.TwoResponseFilesRejected;
begin
  var LFile := WriteRsp('-Name:fromfile');
  var LParser := CreateCommandLineParser;
  var LOpts := TRspOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('@' + LFile + ' @' + LFile, LOpts));
    Assert.AreEqual(Integer(ekResponseFile), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TResponseFileTests.RecursiveAtTokenInsideFileRejected;
begin
  var LFile := WriteRsp('-Name:start @other.rsp');
  var LParser := CreateCommandLineParser;
  var LOpts := TRspOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('@' + LFile, LOpts));
    Assert.AreEqual(Integer(ekResponseFile), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TResponseFileTests.NoResponseFileLeavesCommandLineUnchanged;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TRspOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('-Name:plain -Count:1', LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('plain', LOpts.Name);
    Assert.AreEqual(1, LOpts.Count);
  finally
    LOpts.Free;
  end;
end;

procedure TResponseFileTests.ErrorKindIsResponseFile;
begin
  // Both expected failures (missing file, mixed with other tokens) use ekResponseFile.
  var LParser := CreateCommandLineParser;
  var LOpts := TRspOptions.Create;
  try
    LParser.Parse('@Z:\missing.rsp', LOpts);
    Assert.AreEqual(Integer(ekResponseFile), Integer(LParser.ErrorInfo.Kind));
    Assert.AreEqual(Integer(edResponseFileNotFound), Integer(LParser.ErrorInfo.Detailed));
  finally
    LOpts.Free;
  end;
end;

procedure TResponseFileTests.Utf8ResponseFileWithoutBomDecodedCorrectly;
begin
  // Write the file as raw UTF-8 bytes WITHOUT a BOM. Before the fix, the parser
  // let TStringList.LoadFromFile fall back to the system ANSI codepage when no
  // BOM was present, so a multi-byte UTF-8 character (e.g. 'é' = $C3 $A9) was
  // mis-decoded into two garbage characters. The fix loads with TEncoding.UTF8
  // as the explicit default, so non-ASCII values round-trip correctly.
  FTempFile := TPath.Combine(TPath.GetTempPath, 'clpparser_rsp_' + TPath.GetGUIDFileName + '.txt');
  TFile.WriteAllBytes(FTempFile, TEncoding.UTF8.GetBytes('-Name:café'));

  var LParser := CreateCommandLineParser;
  var LOpts := TRspOptions.Create;
  try
    Assert.IsTrue(LParser.Parse('@' + FTempFile, LOpts), LParser.ErrorInfo.Text);
    Assert.AreEqual('café', LOpts.Name);
  finally
    LOpts.Free;
  end;
end;

end.

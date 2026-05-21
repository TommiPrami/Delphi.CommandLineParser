unit Delphi.CommandLineParser.FileSystemTests.DUnitX;

interface

uses
  System.SysUtils, DUnitX.TestFramework, Delphi.CommandLineParser;

type
  TFileOptions = class
  strict private
    FPath: string;
  public
    [CLPLongName('Path'), CLPFileMustExist, CLPDescription('Path', '<file>'), CLPRequired]
    property Path: string read FPath write FPath;
  end;

  TDirectoryOptions = class
  strict private
    FDir: string;
  public
    [CLPLongName('Dir'), CLPDirectoryMustExist, CLPDescription('Directory', '<dir>'), CLPRequired]
    property Dir: string read FDir write FDir;
  end;

  TNonStringFileOptions = class
  strict private
    FN: Integer;
  public
    [CLPLongName('N'), CLPFileMustExist, CLPDescription('N', '<int>'), CLPDefault('1')]
    property N: Integer read FN write FN;
  end;

  TFileArray = class
  strict private
    FFiles: TArray<string>;
  public
    [CLPLongName('Files'), CLPFileMustExist, CLPDescription('Files', '<list>'), CLPRequired]
    property Files: TArray<string> read FFiles write FFiles;
  end;

  [TestFixture]
  TFileSystemTests = class(TObject)
  private
    function QuoteIfNeeded(const APath: string): string;
  public
    [Test] procedure ExistingFileAccepted;
    [Test] procedure NonExistingFileRejected;
    [Test] procedure FileErrorKindIsFileDoNotExist;
    [Test] procedure ExistingDirectoryAccepted;
    [Test] procedure NonExistingDirectoryRejected;
    [Test] procedure FileMustExistOnIntegerRaisesWrongTypeError;
    [Test] procedure AllFilesInArrayMustExist;
    [Test] procedure ArrayWithOneMissingFileRejected;
  end;

implementation

function TFileSystemTests.QuoteIfNeeded(const APath: string): string;
begin
  if Pos(' ', APath) > 0 then
    Result := '"' + APath + '"'
  else
    Result := APath;
end;

procedure TFileSystemTests.ExistingFileAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFileOptions.Create;
  try
    Assert.IsTrue(
      LParser.Parse('-Path:' + QuoteIfNeeded(ParamStr(0)), LOpts),
      LParser.ErrorInfo.Text);
    Assert.AreEqual(ParamStr(0), LOpts.Path);
  finally
    LOpts.Free;
  end;
end;

procedure TFileSystemTests.NonExistingFileRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFileOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Path:Z:\definitely\does\not\exist_4815162342.bin', LOpts));
    Assert.IsTrue(LParser.ErrorInfo.IsError);
  finally
    LOpts.Free;
  end;
end;

procedure TFileSystemTests.FileErrorKindIsFileDoNotExist;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFileOptions.Create;
  try
    LParser.Parse('-Path:Z:\definitely\does\not\exist_4815162342.bin', LOpts);
    Assert.AreEqual(Integer(ekFileDoesNotExist), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TFileSystemTests.ExistingDirectoryAccepted;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDirectoryOptions.Create;
  try
    var LDir := ExtractFilePath(ParamStr(0));
    if LDir.EndsWith(PathDelim) then
      LDir := LDir.Substring(0, LDir.Length - 1);

    Assert.IsTrue(
      LParser.Parse('-Dir:' + QuoteIfNeeded(LDir), LOpts),
      LParser.ErrorInfo.Text);
  finally
    LOpts.Free;
  end;
end;

procedure TFileSystemTests.NonExistingDirectoryRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TDirectoryOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-Dir:Z:\nosuchdir_4815162342', LOpts));
    Assert.AreEqual(Integer(ekDirectoryDoesNotExist), Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TFileSystemTests.FileMustExistOnIntegerRaisesWrongTypeError;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TNonStringFileOptions.Create;
  try
    Assert.IsFalse(LParser.Parse('-N:42', LOpts));
    Assert.AreEqual(
      Integer(ekWrongDataTypeForFileOrDirectoryMustExist),
      Integer(LParser.ErrorInfo.Kind));
  finally
    LOpts.Free;
  end;
end;

procedure TFileSystemTests.AllFilesInArrayMustExist;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFileArray.Create;
  try
    var LExe := QuoteIfNeeded(ParamStr(0));
    Assert.IsTrue(
      LParser.Parse('-Files:' + LExe + ';' + LExe, LOpts),
      LParser.ErrorInfo.Text);
    Assert.AreEqual(2, Length(LOpts.Files));
  finally
    LOpts.Free;
  end;
end;

procedure TFileSystemTests.ArrayWithOneMissingFileRejected;
begin
  var LParser := CreateCommandLineParser;
  var LOpts := TFileArray.Create;
  try
    var LExe := QuoteIfNeeded(ParamStr(0));
    Assert.IsFalse(
      LParser.Parse('-Files:' + LExe + ';Z:\missing_4815162342.bin', LOpts));
  finally
    LOpts.Free;
  end;
end;

end.

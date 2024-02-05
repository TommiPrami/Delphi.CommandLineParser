# Delphi.CommandLineParser
Slightly modified version of the : https://github.com/gabr42/GpDelphiUnits *by* @gabr42

*Original BSD-3 License applies*

example parameter configuration

```Delphi
type
  TCommandLine = class
  strict private
    FAutoTest  : Boolean;
    FExtraFiles: string;
    FFromDate  : string;
    FImportDir : string;
    FInputFile : string;
    FNumDays   : Integer;
    FOutputFile: string;
    FPrecision : string;
    FToDateTime: string;
  public
    [CLPLongName('ToDate'), CLPDescription('Set ending date/time', '<dt>')]
    property ToDateTime: string read FToDateTime write FToDateTime;

    [CLPDescription('Set precision'), CLPDefault('3.14')]
    property Precision: string read FPrecision write FPrecision;

    [CLPName('i'), CLPLongName('ImportDir'), CLPDescription('Set import folder', '<path>')]
    property ImportDir: string read FImportDir write FImportDir;
    
    [CLPName('a'), CLPLongName('AutoTest', 'Auto'), CLPDescription('Enable autotest mode. And now some long text for testing word wrap in Usage.')]
    property AutoTest: Boolean read FAutoTest write FAutoTest;

    [CLPName('f'), CLPLongName('FromDate'), CLPDescription('Set starting date', '<dt>'), CLPRequired]
    property FromDate: string read FFromDate write FFromDate;

    [CLPName('n'), CLPDescription('Set number of days', '<days>'), CLPDefault('100')]
    property NumDays: Integer read FNumDays write FNumDays;

    [CLPPosition(1), CLPDescription('Input file'), CLPLongName('input_file'), CLPRequired]
    property InputFile: string read FInputFile write FInputFile;

    [CLPPosition(2), CLPDescription('Output file'), CLPRequired]
    property OutputFile: string read FOutputFile write FOutputFile;

    [CLPPositionRest, CLPDescription('Extra files'), CLPName('extra_files')]
    property ExtraFiles: string read FExtraFiles write FExtraFiles;

    [CLPLongName('BooleanParamDefaultAsTrue'), CLPDescription('Boolean Param Default As True ', '<True/False>'), CLPDefault('True')]
    property BooleanParamDefaultAsTrue: Boolean read FUsePDFium write FUsePDFium;
  end;
```

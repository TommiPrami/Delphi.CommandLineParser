# Delphi.CommandLineParser
Slightly modified version of: https://github.com/gabr42/GpDelphiUnits *by* @gabr42

*Original BSD-3 License applies*

Changes to original: https://github.com/gabr42/GpDelphiUnits - GpCommandLineParser.pas
  - Code changes, at least some, are shared back to the original repository as pull requests. Primoz seems to be super busy, so I need to make my own version, to subject it to our code formatting rules. I bet it is better this way.
    If someone is committed to some formatting and coding standard for decades, I bet they are not too happy to have multiple standards in their own repository.
  - Formatting changed to a more standard style
  - Added the possibility to have a boolean parameter default to true, with the ability to set it to false:
    - -BoolParam:False (1/0, true/false, t/f should be supported)
  - Default parameter switch in Windows is "-" (not the "/" character)
  - Added support for Enums
  - Added support for TArray<string>
  - Added support for object inheritance of the command line objects. So multiple applications can share common parameters with no need to duplicate code.
  - Some comments that **I** think are not needed, like version history, are removed, and some are moved into this readme.md etc...

Example parameter configuration:

```Delphi
type
  TCustomCommandLine = class(TObject)
  strict private
    FUseMultiThreading: Boolean;
  public
    [CLPLongName('UseMultiThreading'), CLPDescription('Common Parameter for all command line applications', '<True/False>'), CLPDefault('True')]
    property UseMultiThreading: Boolean read FUseMultiThreading write FUseMultiThreading;
  end;

  TCommandLine = class(TCustomCommandLine)
  strict private
    FAutoTest                 : Boolean;
    FBooleanParamDefaultAsTrue: Boolean;
    FExtraFiles               : string;
    FFromDate                 : string;
    FImportDir                : string;
    FInputFile                : string;
    FNumDays                  : Integer;
    FOutputFile               : string;
    FPrecision                : string;
    FToDateTime               : string;
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

    [CLPLongName('BooleanParamDefaultAsTrue'), CLPDescription('Boolean Param Default As True', '<True/False>'), CLPDefault('True')]
    property BooleanParamDefaultAsTrue: Boolean read FBooleanParamDefaultAsTrue write FBooleanParamDefaultAsTrue;
  end;
```

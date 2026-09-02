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
  - Short names (`CLPName`) may be longer than one character, as long as they are shorter than the long name(s) of the switch. See [Short names](#short-names).
  - The usage text prefers the long name and marks the short name inside it, e.g. `-[R]estore` or `-Use[Se]rvice`. See [Usage output](#usage-output).
  - Some comments that **I** think are not needed, like version history, are removed, and some are moved into this readme.md etc...

Example parameter configuration:

```Delphi
type
  TCustomCommandLine = class(TObject)
  strict private
    FUseMultiThreading: Boolean;
  public
    [CLPName('Multi'), CLPLongName('UseMultiThreading'), CLPDescription('Common Parameter for all command line applications', '<True/False>'), CLPDefault('True')]
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

## Short names

Every named switch has one or more long names (`CLPLongName`; the property name is used when none is given) and optionally one short name (`CLPName`). Names are matched case-insensitively, and both kinds take the value after `:` or `=` (`-n:7`, `-NumDays=7`).

Rules for the short name:

  - It may be longer than one character: `[CLPName('Se'), CLPLongName('UseService')]`.
  - It must be shorter than every long name of the switch, including the long name derived from the property name. Otherwise `Parse` raises `ECLPConfigurationError` with `ErrorInfo.Kind = ekShortNameTooLong`, and the error text names the long name in question.
  - A switch that has no long name at all (`CLPName` equal to the property name and no `CLPLongName`) may use a short name of any length.
  - Positional switches (`CLPPosition`, `CLPPositionRest`) use `CLPName` only as a display name; the length rule does not apply to them.
  - The value can also be attached directly to a short name, DOS-style: `-n7` is the same as `-n:7`, and `-Srvhost1` the same as `-Srv:host1`. If more than one short name is a prefix of the token, the longest one wins.

## Usage output

`Usage` (and `DefaultUsageConsoleOutput`) prefers the long name. When the short name occurs inside a long name, it is marked there with brackets instead of being listed separately: `-[I]mportDir` tells the reader that both `-i` and `-ImportDir` work, and `-Use[Multi]Threading` that `-Multi` is the short form of `-UseMultiThreading`. A short name that does not occur in any long name is still listed on its own, e.g. `-q, -Silent`. When the short name occurs more than once, the start of the long name wins (`-[R]estore` for `r`), then an occurrence with the same casing (`-Use[Se]rvice` for `Se`), then the first one. The prototype line at the top always uses the plain long name.

The example above prints:

```
Usage:
MyApp.exe <input_file> <OutputFile> [extra_files] [-ToDate:<dt>]
  [-Precision:value] [-ImportDir:<path>] [-AutoTest] -FromDate:<dt>
  [-NumDays:<days>] [-BooleanParamDefaultAsTrue] [-UseMultiThreading]

  <input_file>
    - Input file
  <OutputFile>
    - Output file
  [extra_files]
    - Extra files
  [-ToDate:<dt>]
    - Set ending date/time
  [-Precision:value]
    - Set precision
      default: 3.14
  [-[I]mportDir:<path>]
    - Set import folder
  [-[A]utoTest]
    - Enable autotest mode. And now some long text for testing word wrap in
      Usage.
  -[F]romDate:<dt>
    - Set starting date
  [-[N]umDays:<days>]
    - Set number of days
      default: 100
  [-BooleanParamDefaultAsTrue]
    - Boolean Param Default As True
      default: True
  [-Use[Multi]Threading]
    - Common Parameter for all command line applications
      default: True
```

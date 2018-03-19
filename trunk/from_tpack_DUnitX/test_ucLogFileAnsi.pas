unit test_ucLogFileAnsi;

interface

{$I hrefdefines.inc}

uses
{$IFDEF MSWINDOWS}
  Windows, 
{$ENDIF}
  SysUtils, Classes,
  DUnitX.TestFramework,
  ucLogFileAnsi;

type
  [TestFixture]
  TTest_ucLogFileAnsi = class(TObject)
  private
    function TestFileName: string;
    procedure DeleteTestFile;
    function TestLogFileName: string;
    procedure DeleteTestLogFile;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
  private
    [Test]
    procedure TestStringLoadFromStream_Ansi;
    [Test]
    procedure Test_Ansi_StringWriteToFile;
  public
    { The usual issue with these tests is that too many aspects of the testing
      process itself interferes with the characters. }
    [Test]
    procedure Test_Ansi_StringAppendToFile;  // fails 19.Mar.2018
  private
    [Test]
    procedure Test_Ansi_AppendLine;
  end;

implementation

uses
  DateUtils,
  IOUtils,  // D07 and FPC do not have IOUtils nor WideStrUtils
  // and IOUtils does not SHARE the file with other processes like text editors
  {$IFDEF MSWINDOWS}WideStrUtils,  {$ENDIF}
  uFileIO_Helper,
  ZM_UTF8StringUtils,
  ZM_LoggingBase,
  ZM_CodeSiteInterface,
  // avoid linking ucFile here
  ucString, ucString8, ucStringWinAnsi;

const
  cRegisteredTM = WideChar(#$00AE);  // 0x00AE
  TestString =
    'The ultimate Delphi.' + sLineBreak +
    'The complete development solution for Windows.' + cRegisteredTM +
    sLineBreak +
    'With all of the Windows languages and SDKs you need' + sLineBreak +
    'in one environment for modern Windows' + sLineBreak +
    'rapid application development (RAD),' + sLineBreak +
    'Delphi 2005 takes the power of Delphi to the next level.' + sLineBreak;

var
  TestStringA: AnsiString;

const
  LogFileExt = '.info.log';

{ Global }

function TestReadDataFromFile(const AFileName: string): AnsiString;
var
  AStream: TStream;

    function TestReadStringFromStream(AStream: TStream;
      ALength: Integer): AnsiString;
    var
      PTrg: PByte;
    begin
      SetLength(Result, ALength);
      PTrg := Addr(Result[1]);
      AStream.ReadBuffer(PTrg, ALength);
    end;

begin
  AStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    //SetLength(Result, AStream.Size);
    //AStream.Read(Result[1], AStream.Size);
    Result := TestReadStringFromStream(AStream, AStream.Size);
  finally
    AStream.Free;
  end;
end;

procedure TTest_ucLogFileAnsi.DeleteTestFile;
begin
  DeleteFile(TestFileName);
end;

procedure TTest_ucLogFileAnsi.DeleteTestLogFile;
begin
  DeleteFile(TestLogFileName);
end;

procedure TTest_ucLogFileAnsi.SetUp;
begin
  TestStringA := UnicodeToAnsiCodePage(TestString, 1252);
  TearDown;
end;

procedure TTest_ucLogFileAnsi.TearDown;
begin
  DeleteTestFile;
  DeleteTestLogFile;
end;

procedure TTest_ucLogFileAnsi.Test_Ansi_AppendLine;
var
  StrU: UnicodeString;
  idx: Integer;
  Str: string;
  y: TStringList;
  LenOriginalAnsiLine: Integer;
  Len1: Integer;
  LineU: UnicodeString;
begin

  if FileExists(TestFileName) then
    DeleteFile(TestFileName);

  StrU := AnsiCodePageToUnicode(TestStringA, 1252);
  Assert.AreEqual(StrU, TestString, 'T 01');

  LenOriginalAnsiLine := Length(TestStringA);  // includes CRLF chars
  Assert.AreEqual(LenOriginalAnsiLine, Length(StrU), 'T 02');

  y := nil;
  try
    y := TStringList.Create;
    y.Text := StrU;

    for idx := 0 to y.Count - 1 do
    begin
      LineU := y.Strings[idx];
      Assert.AreEqual(0, Pos(sLineBreak, LineU), 'trailing CRLF !');
      AppendLine(TestFileName, AnsiString(Utf8ToAnsiEx(UTF8String(LineU), 1252))); // specifically codepage 1252
    end;

    Str := AnsiCodePageToUnicode(StringAnsiLoadFromFile(TestFileName), 1252);
    Len1 := Length(Str);
    Assert.AreEqual(LenOriginalAnsiLine, Len1, 'length check');
    Assert.AreEqual(y.Text, Str, 'AppendLine T03; see file ' + TestFileName);

    // repeat
    for idx := 0 to y.Count - 1 do
    begin
      AppendLine(TestFileName, AnsiString(y.Strings[idx])); // codepage 1252
    end;

    Str := AnsiCodePageToUnicode(StringAnsiLoadFromFile(TestFileName), 1252);
    Assert.AreEqual(StrU + StrU, Str, 'AppendLine T04');
  finally
    FreeAndNil(y);
  end;
  
end;

function TTest_ucLogFileAnsi.TestFileName: string;
begin
  Result := 'Test.tst';
end;

function TTest_ucLogFileAnsi.TestLogFileName: string;
begin
  Result := DatedFilename(
    ChangeFileExt(FilePathAndNameForModule, LogFileExt));
end;

procedure TTest_ucLogFileAnsi.Test_Ansi_StringAppendToFile;
var
  SAnsi: AnsiString;
  LenOriginal: Integer;
  Len1, Len2: Integer;
begin
  DeleteFile(TestFileName);

  LenOriginal := Length(TestStringA);

  StringAnsiAppendToFile(TestFileName, TestStringA);
  SAnsi := StringAnsiLoadFromFile(TestFileName);
  Len1 := Length(SAnsi);

  Assert.AreEqual(LenOriginal, Len1, 'length compared to StringAnsiLoadFromFile');
  Assert.AreEqual(TestStringA, SAnsi, 'StringAnsiAppendToFile 1');

  StringAnsiAppendToFile(TestFileName, TestStringA);

  SAnsi := StringAnsiLoadFromFile(TestFileName);
  Len2 := Length(SAnsi);

  Assert.AreEqual(2 * LenOriginal, Len2, '2x original length');
  Assert.AreEqual(
    TestStringA + TestStringA, SAnsi, 'StringAnsiAppendToFile 2');

  DeleteFile(TestFileName);
end;

procedure TTest_ucLogFileAnsi.TestStringLoadFromStream_Ansi;
const cFn = 'TestStringLoadFromStream_Ansi';
var
  StartingDataAnsi: AnsiString;
  AStream: TStream;
begin
  CSEnterMethod(Self, cFn);
  StartingDataAnsi := UnicodeToAnsiCodePage(TestString, 1252);
  AStream := nil;

  try
    AStream := TStringStream.Create('');
    AStream.Write(StartingDataAnsi[1], Length(StartingDataAnsi));
    AStream.Seek(0, TSeekOrigin.soBeginning);

    Assert.AreEqual(
      StartingDataAnsi,  // shows registered trademark symbol
      StringLoadFromStream(AStream),
      'StringLoadFromStream');
  finally
    FreeAndNil(AStream);
  end;
  CSExitMethod(Self, cFn);
end;

procedure TTest_ucLogFileAnsi.Test_Ansi_StringWriteToFile;
var
  StartingDataAnsi: AnsiString;
  SAnsi: AnsiString;
  AStream: TFileStream;
begin
  StartingDataAnsi := UnicodeToAnsiCodePage(TestString, 1252);
  StringAnsiWriteToFile(TestFileName, StartingDataAnsi);

  AStream := nil;
  try
    AStream := TFileStream.Create(TestFileName, fmOpenRead or fmShareDenyWrite);
    SAnsi := StringLoadFromStream(AStream);
  finally
    FreeAndNil(AStream);
  end;

  Assert.AreEqual(
    StartingDataAnsi,
    SAnsi,
    'StringAnsiWriteToFile');
    
end;


initialization
  TDUnitX.RegisterTestFixture(TTest_ucLogFileAnsi);

end.

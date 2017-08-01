unit test_ucPipe;

(*
  Copyright (c) 2000-2017 HREF Tools Corp.

  Permission is hereby granted, on 18-Jun-2017, free of charge, to any person
  obtaining a copy of this file (the "Software"), to deal in the Software
  without restriction, including without limitation the rights to use, copy,
  modify, merge, publish, distribute, sublicense, and/or sell copies of the
  Software, and to permit persons to whom the Software is furnished to do so,
  subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
*)

interface

{$I hrefdefines.inc}
{
  Master copy of hrefdefines.inc is versioned on Source Forge in the ZaphodsMap project:
  https://sourceforge.net/p/zaphodsmap/code/HEAD/tree/trunk/ZaphodsMap/Source/hrefdefines.inc
}

{$Define HAVE_UNICONSOLE}  // little utility that writes Ansi, UTF8 and UTF16

uses
  SysUtils,
  DUnitX.TestFrameWork,
  {$IFDEF CodeSite}ZM_CodeSiteInterface,{$ENDIF}
  ucPipe;

type
  [TestFixture]
  TTest_ucPipe = class(TObject)
  private
    Houses8: UTF8String;
    Houses16: UnicodeString;
    FTemp16: TByteArray;
    FTemp16ByteCount: Integer;
    procedure AssembleUTF16(var Data: TLittleBuffer;
      const ByteCount: Integer);
  public
    [Setup]
    procedure SetUp;
  public
    [Test]
    procedure Test_ucPipe_DefaultCodePage;
    [Test]
    procedure Test_ucPipe_Pos_Unicode;
    [Test]
    procedure Test_ucPipe_Delphi_Raw_CastAs_UTF8;
    [Test]
    procedure Test_ucPipe_GetDosOutput_UTF16Directory;
  public
    FMiddleDot8: UTf8String;
    {$IFDEF HAVE_UNICONSOLE}
    // Requires UniConsole.exe
    [Test]
    procedure Test_ucPipe_GetDosOutput_UTF8_UniConsole;
    [Test]
    procedure Test_ucPipe_GetDosOutput_UTF16_UniConsole;
    [Test]
    procedure Test_ucPipe_GetDosOutput_Bat_UniConsole;
    {$ENDIF}

  {$IFDEF MSWINDOWS}
  public
    // Ansi Windows Only
    [Test]
    procedure Test_ucPipe_Pos_Ansi;

    {$IFDEF HAVE_UNICONSOLE}
    // Requires UniConsole.exe
    [Test]
    procedure Test_ucPipe_GetDosOutput_Ansi_UniConsole;
    {$ENDIF}

    [Test]
    procedure Test_ucPipe_GetDosOutput_AnsiDirectory;
  {$ENDIF}
  end;

implementation

uses
  {$IFDEF CodeSite}CodeSiteLogging,{$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF} // to avoid DCC Hint about inline function
  Math, WideStrUtils, Character,
  {$IFDEF MSWINDOWS}ucStringWinAnsi,{$ENDIF}
  ucLogFil, uFileIO_Helper, uDUnitX_Logging_Helper, uZM_Lingvo_Helper;

{$IFDEF MSWINDOWS}
type
  LatinStr = type AnsiString(1252);
const
  cHauserDeu: LatinStr = 'Häuser';
{$ENDIF}

const
  cMiddleDot = #$2E31;

{ TTest_ucPipe }


procedure TTest_ucPipe.AssembleUTF16( var Data: TLittleBuffer;
  const ByteCount: Integer);
begin
  // This could be done for old-Delphi but not important enough.
  Move(Data, FTemp16[FTemp16ByteCount], ByteCount);
  FTemp16ByteCount := FTemp16ByteCount + ByteCount;
end;

procedure TTest_ucPipe.SetUp;
begin
  FMiddleDot8 := UTF8String(cMiddleDot);
  Houses8 := AnsiToUTF8Ex(cHauserDeu, 1252);
  Houses16 := AnsiCodePageToUnicode(cHauserDeu, 1252);
end;

procedure TTest_ucPipe.Test_ucPipe_DefaultCodePage;
begin
  Assert.AreEqual(1252, DefaultSystemCodePage, 'Assert.isTrue -codepage on dcc cmd line');
end;

procedure TTest_ucPipe.Test_ucPipe_GetDosOutput_AnsiDirectory;
var
  SAnsi: AnsiString;
  CommandStrAnsi: AnsiString;
  ErrorCode: Integer;
const
  cDirectoryTest = 'directory_ansi_test.utx';
begin
{$WARNINGS OFF}
  CommandStrAnsi := AnsiString(GetEnvironmentVariable('windir') +
    AnsiString('\system32\cmd.exe /A /c dir /S'));  // NB: /A means Ansi
{$WARNINGS OFF}
  SAnsi := GetDosOutputA(CommandStrAnsi, nil, ErrorCode);
  Assert.AreEqual(ErrorCode, 0, 'GetDosOutputA non-zero error code');

  Assert.isTrue(SAnsi <> '');
  StringWriteToFile(cDirectoryTest, SAnsi);

  Assert.isTrue(Pos('Directory of', SAnsi) > 0,
    'The following does not appear to be a Directory listing: ' +
      SAnsi);
  DeleteFile(cDirectoryTest);
end;

procedure TTest_ucPipe.Test_ucPipe_GetDosOutput_UTF16Directory;
const cFn = 'Test_ucPipe_GetDosOutput_UTF16Directory';
var
  APath: string;
  S16: UnicodeString;
  ErrorCode: Integer;
  SRaw: RawByteString;
  RussianWord: string;
  n: Integer;
  InfoMsg: string;
const
  cDirectoryTest = 'directory_test.utx';
begin
  {$IFDEF CodeSite}CSEnterMethod(Self, cFn);{$ENDIF}

  FTemp16ByteCount := 0;
  FTemp16[0] := 0;

  APath := Unicode_Test_Data_Path; //'..\..\..\..\..\Externals\ZM_TestDataFiles\UniData\';
  {$IFDEF CodeSite}
  CSSend(APath + 'chinese.utx', S(FileExists(APath + 'chinese.utx')));
  {$ENDIF}
  Assert.IsTrue(FileExists(APath + 'chinese.utx'),
    'APath required; adjust the TempBuild output to match up with other tests');

  // This does not display any Russian filenames on USA Windows 7.
  SRaw := GetDosOutput(GetEnvironmentVariable('windir') + '\system32\cmd.exe',
    '/U /c dir ' + //NB: /U Unicode
    APath,
    '', AssembleUTF16, ErrorCode);
  DUnit_Log('SRaw', SRaw);
  Assert.AreEqual(0, ErrorCode, 't01: ' + SysErrorMessage(ErrorCode));

  // NB: SRaw contains garbage because it is really an AnsiString
  n := FTemp16ByteCount div SizeOf(WideChar);
  SetLength(S16, n);

  // This works to turn an array of bytes into a UnicodeString  28-July-2011
  Move(FTemp16[0], S16[1], FTemp16ByteCount);

  Assert.AreNotEqual('', S16, 't02');
  DUnit_Log('S16', S16);

  Assert.isTrue(Pos('Directory of', S16) > 0,
    't03: The following does not appear to be a Directory listing: ' + S16);
  Assert.isTrue(Pos('houses', S16) > 0,
    't04: Did not list the file named [houses.utx]: ' + S16);
  Assert.isTrue(Pos(AnsiCodePageToUnicode(cHauserDeu, 1252), S16) > 0,
    't05: Did not list the file named [Häuser.utx]: ' + S16);

  RussianWord := Welcome_in_Russian;  // see DUnitX files in ZaphodsMap project
  RussianWord := StringReplace(RussianWord, ' ', '_', [rfReplaceAll]);

  { June 2017.  Windows 10 Pro insists on capitalizing the first letter of
    the Russian filename. }
  Assert.AreNotEqual(0, Length(RussianWord));
  RussianWord[Low(RussianWord)] := RussianWord[Low(RussianWord)].ToUpper;

  InfoMsg := 'Directory output did not list the file named [' + RussianWord +
     '] in ' +  S16;
  if Pos(RussianWord, S16) = 0 then
    DUnit_Log('InfoMsg', InfoMsg);
  Assert.AreNotEqual(0, Pos(RussianWord, S16), 't06: ' + InfoMsg);

  DeleteFile(cDirectoryTest);

  {$IFDEF CodeSite}CSExitMethod(Self, cFn);{$ENDIF}
end;

{$IFDEF HAVE_UNICONSOLE}
procedure TTest_ucPipe.Test_ucPipe_GetDosOutput_Ansi_UniConsole;
var
  SAnsi: Latinstr;
  ErrorCode: Integer;
  x: Integer;
const
  cTestFile = 'uniconsole_ansi.txt';
begin
  SAnsi := GetDosOutputA('UniConsole.exe /ansi', nil, ErrorCode);
  StringWriteToFile(cTestFile, SAnsi);
  Assert.AreEqual(0, ErrorCode, 'ansi: ' + SysErrorMessage(ErrorCode));
  Assert.isTrue(SAnsi <> '');
  X := Pos(LatinStr('text:') + cHauserDeu, SAnsi);
  Assert.isTrue(X > 0,
    'text:' + AnsiCodePageToUnicode(cHauserDeu, 1252) + 'NOT found in: ' +
      AnsiCodePageToUnicode(SAnsi, 1252));
  DeleteFile(cTestFile);
end;

procedure TTest_ucPipe.Test_ucPipe_GetDosOutput_Bat_UniConsole;
const cFn = 'Test_ucPipe_GetDosOutput_Bat_UniConsole';
var
  SRaw: RawByteString;
  S8: UTF8String;
  ErrorCode: Integer;
begin
  {$IFDEF CodeSite}CSEnterMethod(Self, cFn);{$ENDIF}

  {$IFDEF MSWINDOWS}
  SRaw := GetDosOutput('run-uniconsole-utf8.bat', '', GetCurrentDir +
    '\..\..\..\..\..\UniConsole\', nil, ErrorCode);
  S8 := SRaw;
  {$IFDEF CodeSite}CSSend('S8', string(S8));{$ENDIF}
  Assert.AreNotEqual(0, Pos('utf8', S8), string(S8));
  {$ENDIF}

  {$IFDEF CodeSite}CSExitMethod(Self, cFn);{$ENDIF}
end;

{$ENDIF}

{$IFDEF HAVE_UNICONSOLE}
procedure TTest_ucPipe.Test_ucPipe_GetDosOutput_UTF8_UniConsole;
const cFn = 'Test_ucPipe_GetDosOutput_UTF8_UniConsole';
var
  S8: UTF8String;
  SRaw: RawByteString;
  SUni: UnicodeString;
  ErrorCode: Integer;
  x: Integer;
const
  cTestFile = 'uniconsole_utf8.utx';
begin
  {$IFDEF CodeSite}CSEnterMethod(Self, cFn);{$ENDIF}

  Assert.IsTrue(FileExists('UniConsole.exe'),
    'File does NOT exist in ' + GetCurrentDir + ' : uniconsole.exe');

  // NB: much depends on how UniConsole.exe was compiled.
  SRaw := GetDosOutput('UniConsole.exe', '-utf8', '', nil, ErrorCode);

  DUnit_Log('SRaw', string(SRaw));

  S8 := SRaw;  // do not convert. just assign to a UTF8String.

  DUnit_Log('S8', string(S8));
  UTF8StringWriteToFile(cTestFile, S8);
  Assert.AreEqual(0, ErrorCode, 'utf8: ' + SysErrorMessage(ErrorCode));
  Assert.AreNotEqual(UTF8String(''), S8);
  SUni := string(S8);
  x := Pos(UnicodeString('text:' + Houses16), SUni);
  DUnit_Log('x', x.ToString);

  Assert.AreNotEqual(0, x,
    'text:' + Houses16 + ' NOT found in ' + SUni);
  DeleteFile(cTestFile);

  {$IFDEF CodeSite}CSExitMethod(Self, cFn);{$ENDIF}
end;
{$ENDIF}


{$IFDEF MSWINDOWS}
procedure TTest_ucPipe.Test_ucPipe_Pos_Ansi;
var
  x: Integer;
begin
  x := Pos(cHauserDeu, UnicodeToAnsiCodePage('text:Häuser' + cMiddleDot, 1252));
  Assert.isTrue(x > 0, Format('x is %d', [x]));
end;
{$ENDIF}

procedure TTest_ucPipe.Test_ucPipe_Pos_Unicode;
var
  x: Integer;
begin
  x := Pos(Houses16, 'text:' + Houses16);
  Assert.isTrue(x > 0, 'Pos Unicode');
end;

procedure TTest_ucPipe.Test_ucPipe_Delphi_Raw_CastAs_UTF8;
var
  S8, S8FromAnsi, S8FromRaw: UTF8String;
  SRaw: RawByteString;
  SAnsi: AnsiString;
begin
  SAnsi := cHauserDeu;
  SRaw := cHauserDeu;
  Assert.AreEqual(RawByteString(SAnsi), SRaw);

  S8 := AnsiToUTF8Ex(cHauserDeu, 1252);
  S8FromAnsi := AnsiToUTF8Ex(SAnsi, 1252);
  Assert.AreEqual(S8, S8FromAnsi, 'Delphi, convert from ansi');

  S8FromRaw := AnsiToUTF8Ex(SAnsi, 1252);
  Assert.AreEqual(S8, S8FromRaw, 'Delphi, convert from raw');

  S8FromRaw := UTF8String(SRaw);  // should not alter data
  Assert.isTrue(S8 <> S8FromRaw, 'See S8FromRaw; Delphi cast via UTF8String');
end;

{$IFDEF HAVE_UNICONSOLE}
procedure TTest_ucPipe.Test_ucPipe_GetDosOutput_UTF16_UniConsole;
var
  S16: UnicodeString;
  SRaw: RawByteString;
  i: Integer;
  ErrorCode: Integer;
  x: Integer;
const
  cTestFile = 'uniconsole_utf16.utx';

begin
  SRaw := GetDosOutput('UniConsole.exe', '/utf16', '', nil,
    ErrorCode);
  Assert.AreEqual(0, ErrorCode, 'utf16: ' + SysErrorMessage(ErrorCode));
  Assert.isTrue(SRaw <> '');
  for i := 1 to Length(SRaw) do
    if SRaw[i] <> #0 then
      S16 := S16 + Char(SRaw[i]);
  x := Pos('text:' + Houses16, S16);
  Assert.isTrue(X > 0,
    'Not found in ' + S16);
  DeleteFile(cTestFile);
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TTest_ucPipe);

end.

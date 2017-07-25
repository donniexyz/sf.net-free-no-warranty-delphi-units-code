program UniConsole;  // console app that outputs UTF8 and UTF16 sample data

(*
  Copyright (c) 2002-2017 HREF Tools Corp.

  Permission is hereby granted, on 26-Jul-2017, free of charge, to any person
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

{$I hrefdefines.inc}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  ZM_UTF8StringUtils,
  {$IFDEF MSWINDOWS}
  ucVers,  // in TPack
  {$ENDIF }
  uCode;   // in TPack

{$R *.res}

type
  LatinStr8 = type UTF8String;

const
  cMiddleDot = #$2E31;
  cHousesDeu8: LatinStr8 = 'Häuser';
var
  aMiddleDot8: UTF8String;
  i: Integer;
  data: RawByteString;

{ On USA Windows 7, CHCP reports a default code page of 437.
  Change that to 65001 to see the /utf8 variation correctly.
  i.e. call CHCP 65000 before calling UniConsole.exe /utf8

  15-May-2011
}

{$IFDEF MSWINDOWS}
function AnsiCodePageToUTF8(const SA: AnsiString; const CodePage: Cardinal)
  : UTF8String;
var
  SU: UnicodeString;
begin
  SetLength(SU, Length(SA));
  UnicodeFromLocaleChars(CodePage, 0, PAnsiChar(SA), Length(SA), PWideChar(SU),
    Length(SA) * 2);
  Result := Utf8Encode(SU);
end;
{$ENDIF}


begin
  aMiddleDot8 := UTF8String(cMiddleDot);

  WriteLn('');
  WriteLn(ExtractFileName(ParamStr(0))
  {$IFDEF MSWINDOWS}+ ' v' + GetVersionDigits(False){$ENDIF}
  );
  WriteLn('Compiled with ' + PascalCompilerCode);
  WriteLn(FormatDateTime('dd-MMM hh:nn:ss', Now));

  {$IFDEF MSWINDOWS}
  if HaveParam('/ansi') then
  begin
    Write('text:');
    Write(cHousesDeu8);
    WriteLn(cMiddleDot);
    WriteLn(Format('bytes:%d' + cMiddleDot,
      [Length(cHousesDeu8)]));
    WriteLn('encoding:ansi' + cMiddleDot);
  end
  else
  {$ENDIF}
  if HaveParam('/utf8') then
  begin
    // The cast of 'text:' to RawByteString is required for D16. 4-Jul-2011.
    // Otherwise, Data contains Ansi data not UTF-8.
    Data := UTF8String('text:') + cHousesDeu8 +
      aMiddleDot8 + UTF8String(sLineBreak);
    Write(Data);
    WriteLn(Format('bytes:%d' + cMiddleDot,
      [Length(cHousesDeu8)]));
    WriteLn(Format('count UTF8 symbols:%d' + cMiddleDot,
      [UTF8Length(cHousesDeu8)]));
    WriteLn('encoding:utf8' + cMiddleDot);
  end
  else
  if HaveParam('/utf16') then
  begin
    Write('text:');
    Write(string(cHousesDeu8));
    WriteLn(cMiddleDot);
    WriteLn(Format('bytes:%d' + cMiddleDot,
      [ByteLength(string(cHousesDeu8)) ]));
    WriteLn('encoding:utf16' + cMiddleDot);
  end
  else
  begin
    WriteLn('Use one switch: /ansi  /utf8  /utf16');
    {$IFDEF DUNIT}WriteLn('Compiling with DUNIT flag interferes!!!');{$ENDIF}
    WriteLn('ParamCount: ' + IntToStr(ParamCount));
    for I := 1 to ParamCount do
    begin
      WriteLn(Format('  %d %s', [i, ParamStr(i)]));
    end;

  end;
  WriteLn('###');

end.

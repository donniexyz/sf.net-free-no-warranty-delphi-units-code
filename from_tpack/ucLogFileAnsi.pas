unit ucLogFileAnsi;

(*
Permission is hereby granted, on 18-Jul-2017, free of charge, to any person
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

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE} 
  !! requires Windows API !!
{$ENDIF}
  SysUtils, Classes, ucString;

// ---- replace/output to a file

{$IFDEF MSWINDOWS}
// AnsiString variations
procedure StringAnsiWriteToFile(const AFileName: string;
  const AText: AnsiString); 
procedure StringAnsiAppendToFile(const AFileName: string;
  const AText: AnsiString); 
procedure AppendLine(const AFileName: string; const AText: AnsiString);
  overload;
procedure StringWriteToStream(AStream: TStream;
  const AText: AnsiString);
function StringLoadFromStream(AStream: TStream): AnsiString;
{$ENDIF}

implementation

uses
{$IFDEF MSWINDOWS}WideStrUtils,{$ENDIF}
// NB: IOUtils with TFile.OpenRead is NOT used because it does not read files
// when they are open in any other process, even readonly in another text editor
  ZM_CodeSiteInterface,
  {*htns*}ZM_LoggingBase,
  {$IFDEF MSWINDOWS}ucStringWinAnsi,{$ENDIF}
  {*htns*}ZM_UTF8StringUtils;
  
{$IFDEF MSWINDOWS}
procedure AppendLine(const AFileName: string; const AText: AnsiString);
//const cFn = 'AppendLine_w_AnsiString';
begin
  //CSEnterMethod(nil, cFn);
  StringAnsiAppendToFile(AFileName, AText + sLineBreak);
  //CSExitMethod(nil, cFn);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure StringAnsiAppendToFile(const AFileName: string;
  const AText: AnsiString); overload;
{$IFDEF LogUcLog}const cFn = 'StringAnsiAppendToFile';{$ENDIF}
var
  AStream: TStream;
begin
  {$IFDEF LogUcLog}CSEnterMethod(nil, cFn);
  CSSend('AText', ConvAto16(AText));{$ENDIF}
  AStream := nil;

  try
    if FileExists(AFileName) then
      AStream := TFileStream.Create(AFileName, fmOpenWrite or
        fmShareDenyWrite)
    else
    begin
      AStream := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
    end;
    AStream.Seek(0, soEnd);
    AStream.Write(AText[1], Length(AText));
  finally
    FreeAndNil(AStream);
  end;
  {$IFDEF LogUcLog}CSExitMethod(nil, cFn);{$ENDIF}
end;
{$ENDIF}

procedure StringAppendToFile(const AFileName: string;
  const AText: UnicodeString); overload;
{$IFDEF LogUcLog}const cFn = 'StringAppendToFile_UnicodeString';{$ENDIF}
var
  AStream: TStream;
begin
  {$IFDEF LogUcLog}CSEnterMethod(nil, cFn);
  CSSend('AText', AText);
  {$ENDIF}
  AStream := nil;
  try
    if FileExists(AFileName) then
      AStream := TFileStream.Create(AFileName, fmOpenWrite or
        fmShareDenyWrite)
    else
    begin
      AStream := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
      AStream.WriteBuffer(UTF16BOM, Length(UTF16BOM));
    end;
    AStream.Seek(0, soEnd);
    AStream.Write(AText[1], Length(AText) * SizeOf(WideChar));
  finally
    FreeAndNil(AStream);
  end;
  {$IFDEF LogUcLog}CSExitMethod(nil, cFn);{$ENDIF}
end;

procedure UTF8StringAppendToFile(const AFileName: string;
  const AText: UTF8String; const FlagBOM: Boolean = True);
var
  AStream: TStream;
  n: Int64;
begin
  AStream := nil;

  try
    if FileExists(AFileName) then
      AStream := TFileStream.Create(AFileName, fmOpenWrite or
        fmShareDenyWrite)
    else
    begin
      AStream := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
      if FlagBOM then
        AStream.WriteBuffer(UTF8BOM, Length(UTF8BOM));
    end;
    AStream.Seek(0, soEnd);
    n := Length(AText); // Length in Bytes which is >= UTF8Length
    AStream.Write(AText[1], Int32(n));
  finally
    FreeAndNil(AStream);
  end;
end;

{$IFDEF MSWINDOWS}
procedure StringAnsiWriteToFile(const AFileName: string; const AText: AnsiString);
begin
  DeleteFile(AFileName);
  StringAnsiAppendToFile(AFileName, AText);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function StringLoadFromStream(AStream: TStream): AnsiString;
// const cFn = StringLoadFromStream;
var
  ASize: Integer;
begin
  ASize := AStream.Size - AStream.Position;
  SetLength(Result, ASize);
  AStream.Read(Result[1], Integer(ASize));
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure StringWriteToStream(AStream: TStream; const AText: AnsiString);
begin
  AStream.WriteBuffer(AText[1], Length(AText));
end;
{$ENDIF}

// initialization
// finalization {none}

end.

unit ucScnDir;  {Scanning Files and Directories}
(*
Copyright (c) 2000-2005 HREF Tools Corp.

Permission is hereby granted, on 1-Jun-2005, free of charge, to any person
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

Author of original version of this file: Michael Ax
Copyright transferred to HREF Tools Corp. on 2-May-2000.
Ported to Kylix 3 and Delphi for .Net on 31-May-2005.
*)

interface

{$I hrefdefines.inc}

uses
{$IFDEF CLR}
  Borland.Vcl.Windows, Borland.Vcl.SysUtils, Borland.Vcl.Classes,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, SysUtils, Classes,
{$ENDIF}
{$IFDEF LINUX}
  SysUtils, Classes,
{$ENDIF}
  ucString, ucFile;

//----------------------------------------------------------------------
// v2 implementation

type
  TtpFindFileEvent = function(const AFileName: string;
    const ASearchRec: TSearchRec): Boolean of object;

// wrapper functions
function tpScanDirectory(const APath, ASpec: string;
  ACallback: TtpFindFileEvent; ASubDirs: Boolean): Boolean;

function tpScanAllDirs(const APath, ASpec: string;
  ACallback: TtpFindFileEvent): Boolean;

// find file logic container class
type
  TtpFindFile = class(TObject)
  private
    FAttributes: Integer;
    FPath, FMask: string;
    FSubDirs: Boolean;
    FOnFindFile: TtpFindFileEvent;
    procedure SetMask(const AValue: string);
    procedure SetPath(const AValue: string);
    function IsAnyFile(ASearchRec: TSearchRec): Boolean;
    function IsDirectory(ASearchRec: TSearchRec): Boolean;
  protected
    procedure DoFindFile(const AFileName: string; const ASearchRec: TSearchRec;
      var AContinue: Boolean); virtual;
    function EnumerateFiles(const APath: string): Boolean;
  public
    constructor Create; virtual;
    function Execute: Boolean;
    property Attributes: Integer read FAttributes write FAttributes;
    property Path: string read FPath write SetPath;
    property Mask: string read FMask write SetMask;
    property SubDirs: Boolean read FSubDirs write FSubDirs;
    property OnFindFile: TtpFindFileEvent read FOnFindFile write FOnFindFile;
  end;

//----------------------------------------------------------------------
// v1 implementation retained for compatibility:

type
  TScanExecProc = function(const FileName: string): Boolean;

function ScanDirectory(const APath, ASpec: string;
  Proc: TScanExecProc; SubDirs: Boolean): Integer;
function ScanAllFiles(const APath, ASpec: string; Proc: TScanExecProc): Integer;
function ScanAllDirs(const APath, ASpec: string; Proc: TScanExecProc): Integer;

implementation

//----------------------------------------------------------------------
// v1 functions retained, bugs and all, for compatibility with existing apps.

function ScanAllDirs(const APath, ASpec: string; Proc: TScanExecProc): Integer;
var
  SearchRec: TSearchRec;
begin
  try
    Result := FindFirst(
      IncludeTrailingPathDelimiter(APath) + ASpec, faDirectory, SearchRec);
    if Result = 0 then
      repeat
        if (faDirectory and SearchRec.Attr = faDirectory) and
          not Proc(IncludeTrailingPathDelimiter(APath) + SearchRec.Name) then
          Break;
      until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

function ScanAllFiles(const APath, ASpec: string; Proc: TScanExecProc): Integer;
var
  SearchRec: TSearchRec;
begin
  Result := FindFirst(IncludeTrailingPathDelimiter(APath) + ASpec,
    faReadOnly or faArchive, SearchRec);
  if Result = 0 then
  try
    repeat
    until not Proc(IncludeTrailingPathDelimiter(APath) + SearchRec.Name) or
      (FindNext(searchRec) <> 0);
  finally
    FindClose(SearchRec);
  end;
end;

function ScanDirectory(const APath, ASpec: string;
  Proc: TScanExecProc; SubDirs: Boolean): Integer;
var
  SearchRec: TSearchRec;
begin
  Result := ScanAllFiles(APath, ASpec, Proc);
  {}
  if SubDirs then
  try {and recurse}
    if FindFirst(
      IncludeTrailingPathDelimiter(APath) + '*', faDirectory, SearchRec) = 0 then
      repeat
        if (Copy(SearchRec.Name, 1, 1) <> '.') and
          ((SearchRec.Attr and faDirectory) = faDirectory) then //recurse
          ScanDirectory(
            IncludeTrailingPathDelimiter(APath) + SearchRec.Name,
            ASpec, Proc, SubDirs);
      until FindNext(searchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

//----------------------------------------------------------------------
// v2 implementation..

// wrapper functions

function tpScanDirectory(const APath, ASpec: string;
  ACallback: TtpFindFileEvent; ASubDirs: Boolean): Boolean;
begin
  with TtpFindFile.Create do
  try
    Path := APath;
    Mask := ASpec;
    SubDirs := ASubDirs;
    OnFindFile := ACallback;
    Result := Execute;
  finally
    Free;
  end;
end;

function tpScanAllDirs(const APath, ASpec: string;
  ACallback: TtpFindFileEvent): Boolean;
begin
  Result := tpScanDirectory(APath, ASpec, ACallback, True);
end;

// find file logic container..

constructor TtpFindFile.Create;
begin
  inherited Create;
  FAttributes := faAnyFile;
  FPath := IncludeTrailingPathDelimiter('');
  FMask := '*';
  FSubDirs := True;
end;

procedure TtpFindFile.SetPath(const AValue:String);
begin
  FPath := IncludeTrailingPathDelimiter(AValue);
end;

function TtpFindFile.Execute: Boolean;
begin
  Result := EnumerateFiles(Path);
end;

procedure TtpFindFile.SetMask(const AValue: string);
begin
  FMask := Trim(AValue);
  if FMask = '' then FMask := '*';
end;

function TtpFindFile.EnumerateFiles(const APath: string): Boolean;
var
  ASearchRec: TSearchRec;
begin
  Result := Assigned(FOnFindFile);
  if Result then
  begin
    // Search files
    if FindFirst(APath + Mask, Attributes, ASearchRec) = 0 then
    try
      repeat
        if IsAnyFile(ASearchRec) then
          DoFindFile(APath + ASearchRec.Name, ASearchRec, Result);
      until Result and (FindNext(ASearchRec) <> 0);
    finally
      FindClose(ASearchRec);
    end;

    if Result and SubDirs then
      // Search subdirectories
      if FindFirst(APath + '*', faAnyFile, ASearchRec) = 0 then
      try
        repeat
          if IsDirectory(ASearchRec) then
            Result := EnumerateFiles(
              IncludeTrailingPathDelimiter(APath + ASearchRec.Name));
        until Result and (FindNext(ASearchRec) <> 0);
      finally
        FindClose(ASearchRec);
      end;
  end;
end;

procedure TtpFindFile.DoFindFile(const AFileName: string;
  const ASearchRec: TSearchRec; var AContinue: Boolean);
begin
  if Assigned(FOnFindFile) then
    AContinue := FOnFindFile(AFileName, ASearchRec)
  else
    AContinue := True;
end;

function TtpFindFile.IsAnyFile(ASearchRec: TSearchRec): Boolean;
begin
  Result := (ASearchRec.Name <> '.') and (ASearchRec.Name <> '..');
end;

function TtpFindFile.IsDirectory(ASearchRec: TSearchRec): Boolean;
begin
  Result := IsAnyFile(ASearchRec) and (ASearchRec.Attr and faDirectory <> 0);
end;

end.

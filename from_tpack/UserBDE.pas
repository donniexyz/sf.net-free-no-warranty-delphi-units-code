unit UserBDE;

(*
Permission is hereby granted, on 11-Apr-2003, free of charge, to any person 
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

// Original author of this file: Michael Ax

{A UserInfo component for BDE users. Need I say more? sure, but I rather you read it.}
{CAVEAT: made to work for production, not peer-reviewed. that's what you are doing now!}

{-----------------------------------------------------------------------------------------}
{ USERBDE                                                                                 }
{-----------------------------------------------------------------------------------------}

interface

uses
  Classes, SysUtils
{$IFDEF WIN32}
, BDE
{$ELSE}
, dbiTypes, dbiProcs
{$ENDIF}
, UpdateOk;

Type
  TtpBDEUserInfo = class(TTpUpdate)
  {this wrapper for a couple of bde status calls demonstrates initializing vis DoUpdate}
  private
    fLocalShare     : Boolean;       { If Local files will be shared }
    fNetProtocol    : Word;          { Net Protocol (35, 40 etc.) }
    fNetShare       : Boolean;       { True if connected to network }
    fzNetType       : DBINAME;       { Network type }
    fzUserName      : DBIUSERNAME;   { Network user name }
    fzIniFile       : DBIPATH;       { Configuration file }
    fzLangDriver    : DBINAME;       { System language driver }
  protected
    function GetPath:String;
    function GetDriver:String;
    function GetNetType:String;
    function GetUserName:String;
    procedure SetNoWord(Value:Word);
    procedure SetNoBool(Value:Boolean);
    procedure SetNoString(const Value:String);
  public
    Constructor Create(aOwner:TComponent); Override;
    function DoUpdate:boolean; Override;
  published
    property NetProtocol   : Word read fNetProtocol write SetNoWord;   { Net Protocol (35, 40 etc.) }
    property LocalShare    : Boolean read fLocalShare write SetNoBool; { If Local files will be shared }
    property NetShare      : Boolean read fNetShare write SetNoBool;   { True if connected to network }
    property NetType       : String read GetNetType write SetNoString; { Network type }
    property UserName      : String read GetUserName write SetNoString;{ Network user name }
    property IniFile       : String read GetPath write SetNoString;    { Configuration file }
    property LangDriver    : String read GetDriver write SetNoString;  { System language driver }
    end;

{-----------------------------------------------------------------------------------------}

//procedure Register;

implementation

{-----------------------------------------------------------------------------------------}

Constructor TtpBDEUserInfo.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  fzNetType[0]    := #0;
  fzUserName[0]   := #0;
  fzIniFile[0]    := #0;
  fzLangDriver[0] := #0;
end;

function TtpBDEUserInfo.DoUpdate:boolean;
var
  BDEcfg: SYSConfig;      { BDE Configuration information record }
begin
  Result:=inherited DoUpdate;
  if not Result then
    Exit;
  DbiGetSysConfig(BDEcfg);
  with BDEcfg do begin
    fNetProtocol    := iNetProtocol;
    fLocalShare     := bLocalShare;
    fNetShare       := bNetShare;
    fzNetType       := fzNetType;
    fzUserName      := szUserName;
    fzIniFile       := fzIniFile;
    fzLangDriver    := fzLangDriver;
    end;
end;

{-----------------------------------------------------------------------------------------}
{ OBJECT PLUMBING  (convert PChar to Pascal String)                                       }
{-----------------------------------------------------------------------------------------}

function TtpBDEUserInfo.GetPath:String;
begin
  Result:=StrPas(fzIniFile);
end;

function TtpBDEUserInfo.GetDriver:String;
begin
  Result:=StrPas(fzLangDriver);
end;

function TtpBDEUserInfo.GetNetType:String;
begin
  Result:=StrPas(fzNetType);
end;

function TtpBDEUserInfo.GetUserName:String;
begin
  Result:=StrPas(fzUserName);
end;

{}

procedure TtpBDEUserInfo.SetNoWord(Value:Word);
begin
end;

procedure TtpBDEUserInfo.SetNoBool(Value:Boolean);
begin
end;

procedure TtpBDEUserInfo.SetNoString(const Value:String);
begin
end;

{------------------------------------------------------------------------------}

//procedure Register;
//begin
//  RegisterComponents('TPACK', [TtpBDEUserInfo]);
//end;

{----------------------------------------------------------------------------------------}
end.

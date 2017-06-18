unit UserNetw;  {access user information on Novell NetWare}

(*
Permission is hereby granted, on 21-Mar-2003, free of charge, to any person
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

{REQUIRES 'NOVELL' TO BE DEFINED FOR NOVEL FEATURES TO WORK. WILL BE FIXED.}

{COMPLETELY RIPPED OUT OF 'THE DELPHI SCREEN SAVER', REGURGITATED AND REINTEGRATED MY WAY.}
{I added a tiny bit of code to avoid triggering password lockout in the Checkpassword but
did not get rid of the ugly conditionals yet. however! once initialized this component will
give YOU the server name!.. If you're going to always run on novell, this code is fine,
if not have it check for the DLL's existance before doing the update. PRODUCTION/not library
quality code; but it now is a generic enough component to spark some ideas. share them.}

{-----------------------------------------------------------------------------------------}
{ USERNOV                                                                                 }
{-----------------------------------------------------------------------------------------}

interface

uses
  Classes
  ,UpdateOk;

Const
  NetWorkPasswordOk = 0;
  NetWorkPasswordLockOut = 197;

Type
  TtpNetWareUserInfo = class(TTpUpdate)
  {wraps some netware calls for easy access (gets the servername!) and can validate a
  password against the users password}
  private
    fConnectID,
    fConnectNumber : LongInt;                                { Default connection id }
    pServerName  :PString;
    pLoginName   :PString;
    pFullName    :PString;
    pLoginTime   :PString;
  protected
    function GetServerName:String;
    function GetLoginName:String;
    function GetFullName:String;
    function GetLoginTime:String;
    procedure SetNoString(const Value:String);
  public
    init:Boolean; {could make a property}
    HasPW:Boolean;
    Constructor Create(aOwner:TComponent); Override;
    Destructor Destroy; Override;
    function DoUpdate:boolean; Override;
    Function HasPassWord:Boolean;
    Function CheckPassWord(const Value:String):Integer;
  published
    property ConnectID:Longint read fConnectID;
    property ConnectNumber:Longint read fConnectNumber;

    property ServerName:String read GetServerName write SetNoString;
    property LoginName:String read GetLoginName write SetNoString;
    property FullName:String read GetFullName write SetNoString;
    property LoginTime: String read GetLoginTime write SetNoString;
    end;

{$IFDEF NOVELL}
Function GetConnectionNumber : Integer;                    { Get novell connection number }
Function GetConnectionInformation(ConId : LongInt;         { Get Novell connection info }
           UserName : PChar; Var ObjType : Integer; Var ObjID : LongInt; LoginTime : PChar) : Integer;
Function GetDefaultConnectionID : Integer;                 { Get Novell Default Connection ID }
Function GetFileServerName(DefConID : LongInt;             { Get Novell File server Name }
           FsName : PChar) : Integer;
Function ReadPropertyValue(szUserName : PChar;             { Read a Novell property Value }
           ObjType : Integer;szPropName : PChar;SegNum :  Integer;szLongName : PChar;
           Var iMoreSegs : Char;Var iMoreFlags : Char) : Integer;
Function VerifyBinderyObjectPassword(UserName : PChar;     { Verify a password against a bindery object }
           BinType  : Integer; PassWord : PChar) : Integer;
{$ENDIF}

{-----------------------------------------------------------------------------------------}

//procedure Register;

implementation

{-----------------------------------------------------------------------------------------}

uses
  Controls
  ,SysUtils
  ,Forms;

{-----------------------------------------------------------------------------------------}
{ NETWARE USER INFO                                                                       }
{-----------------------------------------------------------------------------------------}

{$IFDEF NOVELL}
Function GetConnectionNumber;         external 'NWNETAPI';
Function GetConnectionInformation;    external 'NWNETAPI';
Function GetDefaultConnectionID;      external 'NWNETAPI';
Function GetFileServerName;           external 'NWNETAPI';
Function ReadPropertyValue;           external 'NWNETAPI';    { Declare novell calls }
Function VerifyBinderyObjectPassword; external 'NWNETAPI';
{$ENDIF}

{-----------------------------------------------------------------------------------------}
{ OBJECT CREATION                                                                         }
{-----------------------------------------------------------------------------------------}

Constructor TtpNetWareUserInfo.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
{  Options:=[uifUpdateOnLoad,uifUpdateOnGet];}
  pServerName:=NullStr;
  pLoginName:=NullStr;
  pFullName:=NullStr;
  pLoginTime:=NullStr;
  init:=True; 
  HasPW:=False;
end;

Destructor TtpNetWareUserInfo.Destroy;
begin
  DisposeStr(pLoginTime);
  DisposeStr(pFullName);
  DisposeStr(pLoginName);
  DisposeStr(pServerName);
  inherited Destroy;
end;


function TtpNetWareUserInfo.DoUpdate:boolean;
{$IFDEF NOVELL}
var
  fzServerName  : array [0..50] of Char;                   { File server Name }
  fzLoginName   : array [0..50] of Char;                   { User Name }
  fzFullName    : array [0..127] of Char;                  { Fill Name }
  fzLoginTime   : array [0..9] of Char;                    { Login Time }
  iRc : Integer;                                           { Return Type }
  iOtype   : Integer;                                      { Object Type }
  iOID : LongInt;                                          { Object Id }
  iMoreSegs, iMoreFlags : Char;                            { Local Flags }
{$ENDIF}
begin
  Result:=inherited DoUpdate;
  if not Result then
    Exit;
{$IFDEF NOVELL}
  fzServerName[0] := #0;                                   { Null FS name }
  fzLoginName[0] := #0;                                    { Null User Name }
  fzFullName[0] := #0;                                     { Fill Name }
  fzLoginTime[0] := #0;                                    { Login Time }
  fConnectID := GetDefaultConnectionID;                    { Get default connection ID }
  fConnectNumber:= GetConnectionNumber;                    { Get connection Number }
  iRc := GetConnectionInformation(                         { Get connection info }
           fConnectNumber,@fzLoginName[0],iOtype,iOID,fzLoginTime);
  if fzLoginName[0] = #0 then Exit;                        { If No User Name, No Net thus exit }
  iRc := GetFileServerName(fConnectID,fzServerName);       { Get FS name }
  iRc := ReadPropertyValue(                                { Get the user's full Name }
           fzLoginName,iOtype,'IDENTIFICATION',1,fzFullName,iMoreSegs,iMoreFlags);
  MovePChar2PString(pServerName,fzServerName,false);
  MovePChar2PString(pLoginName,fzLoginName,false);
  MovePChar2PString(pFullName,fzFullName,false);
  MovePChar2PString(pLoginTime,fzLoginTime,false);
{$ENDIF}
end;

{-----------------------------------------------------------------------------------------}
{ OBJECT FUNCTIONS                                                                        }
{-----------------------------------------------------------------------------------------}

Function TtpNetWareUserInfo.HasPassWord:Boolean;
{since calling the VerifyBinderyObjectPassword function multiple times with the wrong pw
can trigger the network's intruder alert and leave the user locked out when the log back in
we're using two constants to get the value only once. Perhaps there is a system message that
kicks in when a user changes names while logged in, but this will kick in just once per run.}
begin
  if Init then begin
    Init:=False;
    HasPW:=CheckPassWord('')<>NetWorkPasswordOk;
    end;
  Result:=HasPW; {true if the user has a network password}
end;

Function TtpNetWareUserInfo.CheckPassWord(const Value:String):Integer;
{$IFDEF NOVELL}
var
  Cursor:TCursor;
  zPWD: PChar;
  zLoginName: PChar;
begin
  Cursor:= Screen.Cursor;
  Screen.Cursor := crHourGlass;                            { Start waiting }
  zPWD:=MakePChar(Value);
  zLoginName:=MakePChar(pLoginName^);
  Result := VerifyBinderyObjectPassword(zLoginName,1,zPWD); { Verify against USER login name }
  FreePChar(zLoginName);
  FreePChar(zPWD);
  Screen.Cursor := Cursor;
{$ELSE}
begin
  Result:= NetWorkPasswordOk;
{$ENDIF}
end;

{-----------------------------------------------------------------------------------------}
{ PROPERTY PLUMBING                                                                       }
{-----------------------------------------------------------------------------------------}

procedure TtpNetWareUserInfo.SetNoString(const Value:String);
begin
end;

function TtpNetWareUserInfo.GetServerName:String;
begin
  Result:=pServerName^;
end;

function TtpNetWareUserInfo.GetLoginName:String;
begin
  Result:=pLoginName^;
end;

function TtpNetWareUserInfo.GetFullName:String;
begin
  Result:=pFullName^;
end;

function TtpNetWareUserInfo.GetLoginTime:String;
begin
  Result:=pLoginTime^;
end;

{------------------------------------------------------------------------------}

//procedure Register;
//begin
//  RegisterComponents('TPACK', [TtpNetWareUserInfo]);
//end;

{----------------------------------------------------------------------------------------}
end.


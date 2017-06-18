unit WebInfo2;

(*
WebHub Demonstration Unit.

Extends WebInfo by adding two additional lists to help work with remote projects.
Adds 3 Verbs and one new public method to select a remote machine from a
drop-down.

Author: Michael Ax.
Copyright (c) 1998 HREF Tools Corp. 
FREE for use by any registered WebHub VCL customer.
May 1998.
*)

(*
Permission is hereby granted, on 10-June-2017, free of charge, to any person 
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

uses
  Forms, Classes, DsgnIntf, Controls, Sysutils,     
  IniLink, ucString,
  WebIList,
  WebInfou;         

type
  TWhRemoteAppsList = class(TWebIniList)
  public
    function ColumnCount:integer; override;
    function ColumnHeaders:String; override;
    end;

  TWhRemoteMachines = class(TWebIniList)
  public
    function ColumnCount:integer; override;
    function ColumnHeaders:String; override;
    end;

type
  TWebInfo2= class(TWebInfo)
  private
    fRemoteMachineID: String;
    fWhRemoteAppsList: TWhRemoteAppsList;
    fWhRemoteMachines: TWhRemoteMachines;
    procedure UpdateRemoteWebs;
    procedure SetRemoteMachineID(const Value: String);
    procedure SetWhRemoteAppsList(Value: TWhRemoteAppsList);
    procedure SetWhRemoteMachines(Value: TWhRemoteMachines);
    procedure EnumRemoteMachineIDs(Proc: TGetStrProc);
  protected
    function  GetLocalVerbCount: Integer;
    function  GetVerbCount: Integer; Override;
    function  GetVerb(Index: Integer): string; Override;
    procedure ExecuteVerb(Index: Integer); Override;
    function DoUpdate:Boolean; Override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure PickRemoteMachineID;
  published
    property RemoteMachineID: String read fRemoteMachineID write SetRemoteMachineID stored false;
    property RemoteMachines: TWhRemoteMachines read fWhRemoteMachines write SetWhRemoteMachines;
    property RemoteApps: TWhRemoteAppsList read fWhRemoteAppsList write SetWhRemoteAppsList;
    end;

type     //reghref
  TWebInfoRemoteMachineIDProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    end;

procedure Register;

implementation

uses
  WebPick;

//------------------------------------------------------------------------------

function TWhRemoteMachines.ColumnCount:integer;
//single section named [RemoteFTP]
begin
  Result:=6;
end;

function TWhRemoteMachines.ColumnHeaders:String;
begin
  Result:='MachineID=FTP-Url or Path,Username,Password,LocalDir,RemoteDir,Port (Blank if LAN)';
end;

//------------------------------------------------------------------------------

function TWhRemoteAppsList.ColumnCount:integer;
//multiple sections named [RemoteWebs.MachineID]
begin
  Result:=2;
end;

function TWhRemoteAppsList.ColumnHeaders:String;
begin
  Result:='AppID=.INI FileName (nopath)';
end;

//------------------------------------------------------------------------------

Constructor TWebInfo2.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  fWhRemoteMachines:= TWhRemoteMachines.CreateFor(self,'RemoteMachines');
  fWhRemoteAppsList:= TWhRemoteAppsList.CreateFor(self,'RemoteAppsList');
end;

destructor TWebInfo2.Destroy;
begin
  fWhRemoteAppsList.Free;
  fWhRemoteMachines.Free;
  inherited Destroy;
end;

//

procedure TWebInfo2.SetWhRemoteAppsList(Value: TWhRemoteAppsList);
begin
  fWhRemoteAppsList.Assign(Value);
end;

procedure TWebInfo2.SetWhRemoteMachines(Value: TWhRemoteMachines);
begin
  fWhRemoteMachines.Assign(Value);
end;

//------------------------------------------------------------------------------

function TWebInfo2.DoUpdate:Boolean;
begin
  Result:=inherited DoUpdate;
  if not Result then
    Exit;
  with fWhRemoteMachines do try
    Clear;
    with WebIni do begin
      IniFileName:=Self.IniFileName;
      Open;
      fWhRemoteMachines.ReadSection(Self,'RemoteMachines');
      Close;
      end;
    RemoteMachineID:='';
    fWhRemoteAppsList.Clear;
  finally
    UpdateEditor;
    end;
end;

//------------------------------------------------------------------------------

function TWebInfo2.GetLocalVerbCount: Integer;
begin
  Result := 3
end;

function TWebInfo2.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + GetLocalVerbCount;
end;

function TWebInfo2.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit Remote Machines';
    1: Result := 'Select Remote Machine';
    2: Result := 'Edit Remote Apps';
  else
    Result:=inherited GetVerb(Index - GetLocalVerbCount);
  end;
end;

procedure TWebInfo2.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: with fWhRemoteMachines do Edit;
    1: PickRemoteMachineID;
    2: with fWhRemoteAppsList do Edit;
  else
    Inherited ExecuteVerb(Index - GetLocalVerbCount);
    end;
end;

//------------------------------------------------------------------------------

procedure TWebInfo2.PickRemoteMachineID;
begin
  with TfmWebPick.create(application) do try
    LoadItems(EnumRemoteMachineIDs);
    Chosen:=RemoteMachineID;
    Caption:='Pick Remote MachineID:';
    if ShowModal=mrOk then
      RemoteMachineID:=Chosen;
  finally
    free;
    end;
end;

procedure TWebInfo2.EnumRemoteMachineIDs(Proc: TGetStrProc);
var
  i:Integer;
begin
  with fWhRemoteMachines do
    for i:= 0 to pred(count) do
      Proc(uppercase(LeftOfEqual(Strings[i])));
end;

//------------------------------------------------------------------------------

function TWebInfoRemoteMachineIDProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList, paSortList];
end;

procedure TWebInfoRemoteMachineIDProperty.GetValues(Proc: TGetStrProc);
begin
  TWebInfo2(GetComponent(0)).EnumRemoteMachineIDs(proc);
end;

//------------------------------------------------------------------------------

procedure TWebInfo2.SetRemoteMachineID(const Value: String);
begin
  if (fRemoteMachineID<>Value) then begin
    with fWhRemoteMachines do
      if (RemoteMachineID='')
      and (Value<>'')
      and (Count>0)
      and (IndexOfName(Value)=0) then
        //default to first if setting bad string.
        fRemoteMachineID:=LeftOfEqual(Strings[0])
      else
        fRemoteMachineID:=Value;
    UpdateRemoteWebs;
    end;
end;

procedure TWebInfo2.UpdateRemoteWebs;
begin
  with fWhRemoteAppsList do try
    Clear;
    if RemoteMachineID<>'' then
      with WebIni do begin
        IniFileName:=Self.IniFileName;
        Open;
        fWhRemoteAppsList.ReadSection(Self,'RemoteWebs.'+RemoteMachineID);
        Close;
        end
  finally
    UpdateEditor;
    end;
end;

//------------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('WebHub', [TWebInfo2]);
  RegisterPropertyEditor(TypeInfo(String), TWebInfo2,'RemoteMachineID', TWebInfoRemoteMachineIDProperty);
end;

//------------------------------------------------------------------------------

end.

unit tpDbgCtl;
(*
Permission is hereby granted, on 31-Oct-2003, free of charge, to any person
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
*)

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Windows, Forms, Dialogs, 
  StdCtrls, Buttons, Toolbar, ExtCtrls, 
  UpdateOk, tpAction, tpDbg;

type
  TDebugControl= class(TtpAction)
  private
    {uses vars global to this unit}
  protected
    function GetState:TDebugExtendedComponentState;
    procedure SetState(Value:TDebugExtendedComponentState);
    function GetFlags:TDebugExtendedComponentFlags;
    procedure SetFlags(Value:TDebugExtendedComponentFlags);
    function GetTest:Boolean; override;
    procedure SetTest(Value:Boolean); override;
    function GetEnabled:Boolean;
    procedure SetEnabled(Value:Boolean);
  public
    constructor Create(aOwner:TComponent); Override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    destructor Destroy; Override;
    procedure Loaded; Override;
  published
    property Options: TDebugExtendedComponentFlags read GetFlags write SetFlags;
    property Enabled: Boolean read GetEnabled write SetEnabled stored false;
    property State: TDebugExtendedComponentState read GetState write SetState stored false;
    end;

{----------------------------------------------------------------------------------------}

//procedure Register;

implementation

{----------------------------------------------------------------------------------------}

constructor TDebugControl.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  DebugState:=DebugState-[decDestroying];
  Options:= DebugFlags;
  if (decCreate in DebugFlags) then
    DebugLog(nil,'Create '+ClassName);
end;

destructor TDebugControl.Destroy;
begin
  if (decDestroy in DebugFlags) then
    DebugLog(nil,'Destroy '+ClassName);
  DebugState:=DebugState+[decDestroying]-[decActive];
  Options:=[];

  if csDesigning in ComponentState then begin
    DebugDlg.Free;
    DebugDlg:=nil;
    end;

  inherited Destroy;
end;

procedure TDebugControl.Loaded;
begin
  if (decLoaded in DebugFlags) then
    DebugLog(nil,'Loaded '+ClassName);
  inherited Loaded;
  ComponentIndex:=0;        {makes itself first component.}
end;

procedure TDebugControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  //if (csUpdating in ComponentState) then
//    exit;
  if aComponent<>nil then begin
    if (aComponent is TDebugControl) and (Operation = opRemove) then
      DebugState:=DebugState-[decDestroying]+[decActive];
    if (([decInsert, decRemove]*DebugFlags)<>[]) then
      if (Operation = opRemove) and (decRemove in DebugFlags) then
        DebugLog(nil,'Remove '+AComponent.Name+'('+AComponent.ClassName+')')
      else
        if (Operation = opInsert) and (decInsert in DebugFlags) then
          DebugLog(nil,'Insert '+AComponent.Name+'('+AComponent.ClassName+')');
    end;
end;

{}

function TDebugControl.GetState:TDebugExtendedComponentState;
begin
  Result:=DebugState;
end;

procedure TDebugControl.SetState(Value:TDebugExtendedComponentState);
begin {DebugState:=Value;} {read only}
end;

{}

function TDebugControl.GetEnabled:Boolean;
begin
  Result:=decEnabled in DebugFlags;
end;

procedure TDebugControl.SetEnabled(Value:Boolean);
begin
  if Value and (not GetEnabled) then
    AdjustDebugFlags(DebugFlags+[decEnabled,decYield])
  else
    if (not Value) and GetEnabled then
      AdjustDebugFlags(DebugFlags-[decEnabled,decYield]);
end;

{}

function TDebugControl.GetFlags:TDebugExtendedComponentFlags;
begin
  Result:=DebugFlags;
end;

procedure TDebugControl.SetFlags(Value:TDebugExtendedComponentFlags);
begin
  if (csLoading in ComponentState) then
    DebugFlags:=Value
  else
    tpDbg.AdjustDebugFlags(Value); {adjusts state and changes flags}
end;

{}

{-----------------------------------------------------------------------------------------}
{ DESIGN TIME TEST PROCS                                                                  }
{-----------------------------------------------------------------------------------------}

procedure TDebugControl.SetTest(Value:Boolean);
begin
  Enabled:=True;
  DebugLog(Owner,'Test '+ClassName);
end;

function TDebugControl.GetTest:Boolean;
begin
  Result:= decActive in DebugState;
end;

{----------------------------------------------------------------------------------------}

//procedure Register;
//begin
//  RegisterComponents('TPACK', [TDebugControl]);
//end;

{----------------------------------------------------------------------------------------}
end.



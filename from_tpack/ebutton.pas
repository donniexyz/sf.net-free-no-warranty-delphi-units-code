unit ebutton;

(*
Permission is hereby granted, on 10-Feb-2004, free of charge, to any person
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

uses Classes, TypInfo, SysUtils, ToolBar, ucString;

type
  TtpComponentButton = class(TtpToolButton)
  private
    fTpComponent: TComponent;
  protected
    procedure SetTpComponent(Value:TComponent); virtual; abstract;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property TpComponent: TComponent read fTpComponent write SetTpComponent;
    end;

type
  {button to point to action verbs.
  internally points BY NUMBER! so be aware of that when
  reassigning verbs or derive a new type to use the string!}

  TtpVerbButton = class(TtpComponentButton)
  private
    fTpVerb: String;
  protected
    procedure SetTpComponent(Value:TComponent); override;
  public
    procedure Click; override;
  published
    property TpVerb: String read fTpVerb write fTpVerb;
    end;

type
  TtpRTTIButton = class(TtpComponentButton)
  protected
  public
    Filter: TTypeKinds;
    function PropertyValue(Instance: TPersistent; PropInfo: Pointer):String;
    end;

type
  TtpEnumButton = class(TtpRTTIButton)
  private
    fProperty: Integer;
    fPropChop: Byte;
  protected
    function GetProperty: String;
    procedure SetProperty(const Value:String);
    function  GetPropertyValue:String;
    procedure SetNoString(const Value:String);
    procedure SetTpComponent(Value:TComponent); override;
  public
    constructor Create(aOwner:TComponent); override;
    procedure Click; override;
  published
    property PropName: String read GetProperty write SetProperty;
    property PropValue:String read GetPropertyValue write SetNoString;
    property PropChop: byte read fPropChop write fPropChop;
    end;

{------------------------------------------------------------------------------}

//procedure Register;

implementation

uses
  UpdateOk;

type
  TtpComponentAccess= class(TtpComponent);

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

procedure TtpComponentButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csUpdating in ComponentState) then
    exit;
  if Operation = opRemove then
    if fTpComponent=aComponent then
      fTpComponent:=nil;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

procedure TtpVerbButton.Click;
begin
  inherited Click;
  if assigned(fTpComponent)
  and (fTpVerb<>'') then
    TTpComponentAccess(fTpComponent).ExecuteVerbByName(fTpVerb);
end;

procedure TtpVerbButton.SetTpComponent(Value:TComponent);
//var
//  a1,a2:string;
begin
  if (Value<>fTpComponent) then begin
    if not (Value is TtpComponent) then
      exit;
//    if (Value<>nil) and (fTpComponent<>nil)
{    and (TTpComponentAccess(Value).Verb[fTpVerb]<>
         TTpComponentAccess(fTpComponent).Verb[fTpVerb])} //then
//      fTpVerb:=-1;
//      fTpVerb:='';
    fTpComponent:=Value;
//    SplitString(Hint,'|',a1,a2);
//    TpVerb:=a1;
//    Hint:=a2;
    end;
end;

{---------------------------------------------------------------}

function TtpRTTIbutton.PropertyValue(Instance: TPersistent; PropInfo: Pointer):String;
var
  PropType: PTypeInfo;
type
  TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  function SetPropString:string;
  var
    I: Integer;
    BaseType: PTypeInfo;
    Value: Cardinal;
  begin
    Value:=GetOrdProp(Instance, PropInfo);
    Result:='';
{$IFNDEF VER90}
    BaseType := GetTypeData(PropType)^.CompType^;
{$ELSE}
    BaseType := GetTypeData(PropType)^.CompType;
{$ENDIF}
    for I := 0 to 15 do
      if I in TCardinalSet(Value) then begin
        if Result<>'' then
          Result:=Result+',';
          Result:=Result+GetEnumName(BaseType, I);
        end;
  end;

  function IntPropString(IntType: PTypeInfo; Value: Longint):string;
  begin
    Result:=IntToStr(Value);
  end;

  function OrdPropString:string;
  begin
    Result:=inttostr(
    GetOrdProp(Instance, PropInfo));
  end;

  function EnumPropString:string;
  var
    i: integer;
    BaseType: PTypeInfo;
  begin
    i:=GetOrdProp(Instance, PropInfo);
{$IFNDEF VER90}
    BaseType := GetTypeData(PropType)^.BaseType^;
{$ELSE}
    BaseType := GetTypeData(PropType)^.BaseType;
{$ENDIF}
    Result:=GetEnumName(BaseType,i);
  end;

  function FloatPropString:string;
  begin
    Result:=FloatToStr(GetFloatProp(Instance, PropInfo));
  end;

  function StringPropString:string;
  begin
    Result:=GetStrProp(Instance, PropInfo);
  end;

  function ObjectPropString:string;
  var
    o:TObject;
  begin
    o:=TObject(GetOrdProp(Instance, PropInfo));
    if o=nil then
      Result:=''
    else
      Result:=o.ClassName;
  end;

  function MethodPropString:string;
  var
    Value: TMethod;
  begin
    Value := GetMethodProp(Instance, PropInfo);
    Result:='';
    if Value.Code<>nil then
      Result:=Result+'Code:'+{Instance}owner.MethodName(Value.Code);
    if Value.Data<>nil then
      Result:=Result+'Data,';
    Result:=Copy(Result,1,length(Result)-1);
  end;

begin
  Result:='Unknown';
  if PPropInfo(PropInfo)^.SetProc <> nil then begin
{$IFNDEF VER90}
    PropType := PPropInfo(PropInfo)^.PropType^;
{$ELSE}
    PropType := PPropInfo(PropInfo)^.PropType;
{$ENDIF}
    case PropType^.Kind of
      tkInteger,
      tkChar:   Result:=OrdPropString;
      tkEnumeration: Result:=EnumPropString;
      tkSet:    Result:=SetPropString;
      tkFloat:  Result:=FloatPropString;
      tkLString,
      tkString: Result:=StringPropString;
      tkClass:  Result:=ObjectPropString;
      tkMethod: Result:=MethodPropString;
      end;
    end;
end;

{------------------------------------------------------------------------------}

constructor TtpEnumButton.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  Filter:= [tkEnumeration];
  fProperty:= -1;
end;

procedure TtpEnumButton.SetTpComponent(Value:TComponent);
var
  a1,a2:string;
begin
  if (Value<>fTpComponent) then begin
{    if not (Value is TtpComponent) then
      exit; }
    if (Value<>nil) and (fTpComponent<>nil) then
      fProperty:=-1;
    fTpComponent:=Value;
    SplitString(Hint,'|',a1,a2);
    PropName:=a1;
    Hint:=a2;
    end;
end;

function TtpEnumButton.GetProperty: String;
var
  Count: Integer;
  PropList: PPropList;
begin
  Result:='';
  if (TpComponent<>nil) and (fProperty>-1) then begin
    Count:=GetTypeData(TpComponent.ClassInfo)^.PropCount;;
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      GetPropInfos(TpComponent.ClassInfo, PropList);
      Result:=PropList^[fProperty].Name;
    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
      end;
    end;
end;



procedure TtpEnumButton.SetProperty(const Value:String);
var
  I,Count: Integer;
  PropList: PPropList;
begin
  if (TpComponent=nil) then begin
    Hint:=Value+'|'+Hint;
    exit;
    end;
  fProperty:=-1;
  Count:=GetTypeData(TpComponent.ClassInfo)^.PropCount;;
  GetMem(PropList, Count * SizeOf(Pointer));
  try
    GetPropInfos(TpComponent.ClassInfo, PropList);
    for i := 0 to pred(Count) do
      if PropList^[i]^.PropType^.Kind in Filter then
        if CompareText(PropList^[i]^.Name,Value)=0 then begin
          fProperty:=i;
          break;
          end
  finally
    FreeMem(PropList, Count * SizeOf(Pointer));
    end;

  if (fProperty>-1) {and (Caption='')}
  and (csDesigning in ComponentState) then
    Caption:=PropValue;
end;

procedure TtpEnumButton.SetNoString(const Value:String);
begin
end;

function TtpEnumButton.GetPropertyValue:String;
var
  Count: Integer;
  PropList: PPropList;
begin
  Result:='';
  if (TpComponent=nil) or (fProperty=-1) then
    exit;

  Count:=GetTypeData(TpComponent.ClassInfo)^.PropCount;
  GetMem(PropList, Count * SizeOf(Pointer));
  try
    GetPropInfos(TpComponent.ClassInfo, PropList);
    Result:=PropertyValue(TpComponent,PropList^[fProperty]);
  finally
    FreeMem(PropList, Count * SizeOf(Pointer));
    end;

  if length(Result)>=fPropChop then
    delete(Result,1,fPropChop);
end;


procedure TtpEnumButton.Click;
var
  I,Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  BaseType: PTypeInfo;
  TypeData: PTypeData;
begin
  inherited Click;
  if (TpComponent=nil) or (fProperty=-1) then
    exit;
  Count:=GetTypeData(TpComponent.ClassInfo)^.PropCount;
  GetMem(PropList, Count * SizeOf(Pointer));
  try
    GetPropInfos(TpComponent.ClassInfo, PropList);
    PropInfo := PropList^[fProperty];

    i:=GetOrdProp(TpComponent, PropInfo); {get}
{$IFNDEF VER90}
    TypeData:= GetTypeData(PropInfo^.PropType^);  {type/range}
{$ELSE}
    TypeData:= GetTypeData(PropInfo^.PropType);  {type/range}
{$ENDIF}
    if i=TypeData^.MaxValue then       {adjust. mod?}
      i:=TypeData^.MinValue
    else
      i:=succ(i);
    SetOrdProp(TpComponent, PropInfo, i); {set}

    i:=GetOrdProp(TpComponent, PropInfo); {and re-read}
{$IFNDEF VER90}
    BaseType := TypeData^.BaseType^;    {namebase}
{$ELSE}
    BaseType := TypeData^.BaseType;    {namebase}
{$ENDIF}
    Caption:=GetEnumName(BaseType,i);

  finally
    FreeMem(PropList, Count * SizeOf(Pointer));
    end;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

//procedure Register;
//begin
//  RegisterComponents('TPACK',[TtpVerbButton,TtpEnumButton]);
//
//  RegisterPropertyEditor(TypeInfo(String), TtpVerbButton
//  ,'TpVerb', TtpVerbButtonVerbProperty);
//
//  RegisterPropertyEditor(TypeInfo(TComponent), TtpComponentButton
//  ,'TpComponent', TtpComponentButtonComponentProperty);
//
//end;

{------------------------------------------------------------------------------}
end.


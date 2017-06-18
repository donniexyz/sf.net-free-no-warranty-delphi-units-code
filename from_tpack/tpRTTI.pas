unit tpRTTI;  // Delphi run time type information
(*
  Copyright (c) 2000-2017 HREF Tools Corp.

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
  Copyright transferred to HREF Tools Corp. on 2-May-2000.
  Ported to Kylix3 on Linux by D. Gabdullin, 28-Apr-2005.
*)

(*
  Warning to programmers: Please do not try to optimize GetPropertyAsString.
  String in .NET is a class (tkClass), however
  PropIsType(Component, PropertyName, tkWString) returns True.
  So is Integer - even though it is class
  PropIsType(Component, PropertyName, tkInteger) returns True.

  i.e. for best results, leave the code as you see it written here!
*)

(* This unit should compile in Free Pascal BUT does not work yet.
  Result nil below.
  When it does work, change tpShareB.pas to use this unit again.
*)

interface

{$I hrefdefines.inc}

uses
  Classes, TypInfo;

type
  TPropArray = array of PPropInfo;
  { Cross-platform helper functions }

function GetPropType(APropInfo: PPropInfo): PTypeInfo;
function GetPropTypeKind(APropInfo: PPropInfo): TTypeKind;
function GetCompType(ATypeInfo: PTypeInfo): PTypeInfo;
function GetBaseType(ATypeInfo: PTypeInfo): PTypeInfo;
function GetPropArray(ATypeInfo: PTypeInfo; Sorted: Boolean = False)
  : TPropArray; overload;
function GetPropArray(ATypeInfo: PTypeInfo; TypeKinds: TTypeKinds;
  Sorted: Boolean = False): TPropArray; overload;
function IsCanRead(PropInfo: PPropInfo): Boolean;
function IsCanWrite(PropInfo: PPropInfo): Boolean;
{$IFNDEF Delphi12UP}
function GetAnsiStrProp(AInstance: TObject; APropInfo: PPropInfo)
  : AnsiString; overload;
procedure SetAnsiStrProp(AInstance: TObject; APropInfo: PPropInfo;
  const Value: AnsiString); overload;
{$ENDIF}
{$IF (NOT Defined(Delphi25UP)) and (NOT Defined(NEXTGEN))}
function GetShortStrProp(AInstance: TObject; APropInfo: PPropInfo)
  : ShortString; overload; // D24 VCL
procedure SetShortStrProp(AInstance: TObject; APropInfo: PPropInfo;
  const Value: ShortString); overload; // D24 VCL
{$IFEND}
{ Find a named component in an application. }

function FindComponentByName(const ComponentName: string): TComponent;

{ Find a named component in an application and return its property value.
  If property does not exist or type is not integer, return zero. }

function GetPropertyAsInteger(const ComponentName, PropertyName
  : string): Integer;

{ Find a named component in an application and return its property value.
  If property does not exist or type is not string, return empty string. }

function GetPropertyAsString(const ComponentName, PropertyName: string): string;

{ Return if a component has a property. }

function HasProperty(const ComponentName, PropertyName: string): Boolean;

// for testing purpose
{$IFDEF DUNIT}
procedure Cleanup_tpRTTI_Cache;
{$ENDIF}

implementation

uses
  SysUtils,
  {avoid Forms unit here}
  MultiTypeApp;

// used to cache component in GetProperty and HasProperty

var
  Component: TComponent;

{$IFDEF DUNIT}

procedure Cleanup_tpRTTI_Cache;
begin
  Component := nil;
end;
{$ENDIF}

function FindComponentByNameAt(Component: TComponent;
  const ComponentName: string): TComponent; overload;
var
  idx: Integer;
begin
  if SameText(Component.Name, ComponentName) then
    Result := Component
  else
  begin
    // loop through all components
    for idx := 0 to Component.ComponentCount - 1 do
    begin
      Result := FindComponentByNameAt(Component.Components[idx], ComponentName);
      if Result <> nil then
        Exit;
    end;
    Result := nil;
  end;
end;

function FindComponentByName(const ComponentName: string): TComponent;
begin
  Result := FindComponentByNameAt(Application.RootOwner, ComponentName);
end;

function GetPropertyAsInteger(const ComponentName, PropertyName
  : string): Integer;
begin
  if HasProperty(ComponentName, PropertyName) and
    PropIsType(Component, PropertyName, tkInteger) then
    Result := GetOrdProp(Component, PropertyName)
  else
    Result := 0;
end;

function GetPropertyAsString(const ComponentName, PropertyName: string): string;
begin
  if HasProperty(ComponentName, PropertyName) and
    (PropIsType(Component, PropertyName, tkString) or PropIsType(Component,
    PropertyName, tkLString) or
{$IFDEF UNICODE}PropIsType(Component, PropertyName, tkUString) or {$ENDIF}
    PropIsType(Component, PropertyName, tkWString)) then
    Result := GetStrProp(Component, PropertyName)
  else
    Result := '';
end;

function HasProperty(const ComponentName, PropertyName: string): Boolean;
begin
  // check if the component asked for is another than last time
  if (Component = nil) or not SameText(Component.Name, ComponentName) then
    Component := FindComponentByName(ComponentName);
  // it is not the same component
  Result := (Component <> nil) and (IsPublishedProp(Component, PropertyName));
end;

function GetPropType(APropInfo: PPropInfo): PTypeInfo;
begin
{$IFDEF FPC}
  // Error: Unknown record field identifier "TYPEINFO"
  Result := nil;
{$ENDIF}
  Result := APropInfo.PropType^;
end;

function GetPropTypeKind(APropInfo: PPropInfo): TTypeKind;
begin
  Result := GetPropType(APropInfo).Kind;
end;

function GetCompType(ATypeInfo: PTypeInfo): PTypeInfo;
begin
{$IFDEF FPC} Result := nil; {$ENDIF}
  Result := GetTypeData(ATypeInfo).CompType^;
end;

function GetBaseType(ATypeInfo: PTypeInfo): PTypeInfo;
begin
{$IFDEF FPC}
  // Error: Unknown record field identifier "BASETYPE"
  Result := nil;
{$ENDIF}
  Result := GetTypeData(ATypeInfo).BaseType^;
end;

function GetPropArray(ATypeInfo: PTypeInfo; Sorted: Boolean = False)
  : TPropArray;
begin
  SetLength(Result, GetTypeData(ATypeInfo).PropCount);
  GetPropInfos(ATypeInfo, PPropList(Result));
{$IFDEF FPC}
  { nothing - Error: Identifier not found "SortPropList" }
{$ELSE}
  if Sorted then
    SortPropList(PPropList(Result), Length(Result));
{$ENDIF}
end;

function GetPropArray(ATypeInfo: PTypeInfo; TypeKinds: TTypeKinds;
  Sorted: Boolean = False): TPropArray;
begin
  SetLength(Result, GetPropList(ATypeInfo, TypeKinds, nil));
  GetPropList(ATypeInfo, TypeKinds, PPropList(Result));
{$IFDEF FPC}
  { nothing - Error: Identifier not found "SortPropList" }
{$ELSE}
  if Sorted then
    SortPropList(PPropList(Result), Length(Result));
{$ENDIF}
end;

function IsCanRead(PropInfo: PPropInfo): Boolean;
begin
  Result := PropInfo.GetProc <> nil;
end;

function IsCanWrite(PropInfo: PPropInfo): Boolean;
begin
  Result := PropInfo.SetProc <> nil;
end;

{$IFNDEF Delphi12UP}

function GetAnsiStrProp(AInstance: TObject; APropInfo: PPropInfo): AnsiString;
begin
  Result := GetStrProp(AInstance, APropInfo);
end;

procedure SetAnsiStrProp(AInstance: TObject; APropInfo: PPropInfo;
  const Value: AnsiString);
begin
  SetStrProp(AInstance, APropInfo, Value);
end;

{$ENDIF}

{$IF (NOT Defined(Delphi25UP)) and (NOT Defined(NEXTGEN))}
function GetShortStrProp(AInstance: TObject; APropInfo: PPropInfo): ShortString; // D24 VCL
begin
  Result := GetAnsiStrProp(AInstance, APropInfo);
end;

procedure SetShortStrProp(AInstance: TObject; APropInfo: PPropInfo;
  const Value: ShortString); // D24 VCL
begin
  SetAnsiStrProp(AInstance, APropInfo, Value);
end;
{$IFEND}

end.

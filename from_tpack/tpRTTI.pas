unit tpRTTI;  // Delphi run time type information

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
Copyright transferred to HREF Tools Corp. on 2-May-2000.
*)

interface

uses Classes, Controls, Forms, SysUtils, TypInfo;

function FindComponentByName(const ComponentName: string): TComponent; overload; //find a named component in an application
function GetPropertyAsInteger(const ComponentName, PropertyName: string): Integer;
function GetPropertyAsString(const ComponentName, PropertyName: string): string;
function HasProperty(const ComponentName, PropertyName: string): Boolean; //return if a component has a property

implementation

var
  Component: TComponent; //used to cache component returned by FindComponent in GetPropertyStr

function FindComponentByName(Component: TComponent; const ComponentName: string): TComponent; overload;
var
  I: Integer;
begin
  if AnsiCompareText(Component.Name, ComponentName) = 0 then
    Result := Component
  else begin
    //loop through all components
    for I := 0 to Pred(Component.ComponentCount) do
    begin
      Result := FindComponentByName(Component.Components[I], ComponentName);
      if Result <> nil then
        Exit;
    end;
    Result := nil;
  end;
end;

function FindComponentByName(const ComponentName: string): TComponent; overload;
begin
  Result := FindComponentByName(Application, ComponentName);
end;

function GetPropertyAsInteger(const ComponentName, PropertyName: string): Integer;
begin
  if HasProperty(ComponentName, PropertyName) then
  begin
    if GetPropInfo(Component, PropertyName, [tkInteger]) <> nil then //check if the property is an integer
      Result := GetOrdProp(Component, PropertyName)
    else
      Result := 0;
  end else
    Result := 0;
end;

function GetPropertyAsString(const ComponentName, PropertyName: string): string;
begin
  if HasProperty(ComponentName, PropertyName) then
  begin
    if GetPropInfo(Component, PropertyName, [tkString, tkLString, tkWString]) <> nil then //check if the property is a string
      Result := GetStrProp(Component, PropertyName)
    else
      Result := '';
  end else
    Result := '';
end;

function HasProperty(const ComponentName, PropertyName: string): Boolean;
begin
  //check if the component asked for is another than last time
  if (Component = nil) or
     (AnsiCompareStr(Component.Name, ComponentName) <> 0) then
    Component := FindComponentByName(ComponentName); //it is not the same component

  Result := (Component <> nil) and
            (IsPublishedProp(Component, PropertyName));
end;

end.

unit ucGoURL; {access a file through a web browser }
(*
Permission is hereby granted, on 1-Nov-2003, free of charge, to any person
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

type
  TtpBrowserMode=(bmRegistryDefault,bmShellExecute,bmNetScapeDDE);

procedure BrowseURL(const URL:string);
procedure BrowserGotoURL(const URL:string;Mode:TtpBrowserMode);
procedure BrowserNetscapeGotoURL(Value:string);

implementation

uses
  ucShell, ucstring, 
  DDEMan, Windows, Registry;

procedure BrowseURL(const URL:string);
begin
  BrowserGotoURL(URL,bmRegistryDefault);
end;

procedure BrowserGotoURL(const URL:string;Mode:TtpBrowserMode);
//use ShellExecute or DDE to display the URL
//accepts 'default' mode where it guesses the mode from the registry
const
  cHTMLViewer='SOFTWARE\Classes\htmlfile\shell\open\ddeexec\Application';
  cDefaultKey='';
  cIEKey='IExplore';
begin
  if Mode=bmRegistryDefault then
    with TRegistry.Create do try
      Mode:=bmShellExecute; //pick a default
      RootKey:= HKEY_LOCAL_MACHINE;
      if OpenKey(cHTMLViewer, False) then
        if ReadString(cDefaultKey)<>cIEKey then
          Mode:=bmNetScapeDDE;
    finally
      free;
      end;
  case Mode of
  bmShellExecute: WinShellOpen(URL);
   bmNetScapeDDE: BrowserNetscapeGotoURL(URL);
    end;
end;


procedure BrowserNetscapeGotoURL(Value:string);
//use DDE and Netscape to display the URL
const
  cNetScapePath='SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\NetScape.exe';
    cDefaultKey='';
      cNetScape='NetScape';
      cActivate='WWW_Activate';
       cOpenURL='WWW_OpenURL';
        cWakeUp='0xFFFFFFFF';
       cRequest=',,'+cWakeUp+',0x3,,,';
begin
  with TDDEClientConv.Create(nil) do try
    // specify the location of netscape.exe
    with TRegistry.Create do try
      RootKey:= HKEY_LOCAL_MACHINE;
      if OpenKey(cNetScapePath, False) then begin
        ServiceApplication:= '"'+ReadString(cDefaultKey)+'"';
        SetLink(cNetScape,cActivate); //start netscape
        RequestData(cWakeUp);         //wake it up
        SetLink(cNetScape,cOpenURL);  //tell it what we'll tell it
        RequestData(Value+cRequest);  //get the value
        CloseLink;                    //break the connection
        end;
    finally
      free;
      end;
  finally
    free;
    end;
end;

end.

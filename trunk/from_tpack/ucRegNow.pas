unit ucRegNow; // utility code - interface to RegNow affiliate system

{ ---------------------------------------------------------------------------- }
{ * The function regGetBuyURL was provided in swp_lite.zip from RegNow.com   * }
{ * There was no copyright claimed on that example.  HREF Tools Corp. is NOT * }
{ * claiming copyright on this slightly modified code.                       * }
{ * According to its web site, RegNow is the software industry’s premier     * }
{ * registration commerce provider.  If you sell shareware licenses, you     * }
{ * should consider signing up with them.                                    * }
{ ---------------------------------------------------------------------------- }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, ShellApi, Registry;

function regGetBuyURL(const publisher, appName: string;
  const appVer: string = '0'): string;

(* example usage:
  function TMainForm.SalesURL: string;
  begin
  {$IFDEF REGNOW}
  Result := regGetBuyURL('MyCompanyName', 'MyProductName', GetVersionDigits(True));  // uses ucVers.pas
  if Result = '' then
  Result := 'https://www.regnow.com/softsell/nph-softsell.cgi?item=xxxxx-x'; // RegNow product id
  {$ELSE}
  Result := 'https://store.www.href.com/hrefshop';  // your online store url
  {$ENDIF}
  end;

*)

implementation

function regGetBuyURL(const publisher, appName: string;
  const appVer: string = '0'): string;
var
  KeyPath: string;
begin
  { About the version number:
    "The file version needs to remain 0 in your affiliate tracking code for
    the mechanism to work." -- RegNow Support, 22-Apr-2011
  }

  { form the registry key path }
  KeyPath := 'SOFTWARE\Digital River\SoftwarePassport\' + publisher + '\' +
    appName + '\' + appVer;

  Result := '';

  { Read the "BuyURL" value from the registry key }
  with TRegistry.Create do
    try
      { read the "BuyURL" value from HKEY_LOCAL_MACHINE branch first }
      RootKey := HKEY_LOCAL_MACHINE;
      Access := KEY_READ;
      if OpenKeyReadOnly(KeyPath) then
      begin
        Result := ReadString('BuyURL');
        CloseKey;
      end
      else
      begin
        { fail to read from HKEY_LOCAL_MACHINE branch, try HKEY_CURRENT_USER }
        RootKey := HKEY_CURRENT_USER;
        if OpenKeyReadOnly(KeyPath) then
        begin
          Result := ReadString('BuyURL');
          CloseKey;
        end
      end;

    finally
      Free;
    end;
end;

end.

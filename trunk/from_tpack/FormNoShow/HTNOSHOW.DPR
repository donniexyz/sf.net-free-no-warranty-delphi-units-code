program Htnoshow;

{ Author: Michael Ax, HREF Tools Corp.
  Copyright (c) 1995-1996 HREF Tools Corp.  All Rights Reserved.

  For updates & other news, please visit our web site at http://www.href.com/.

  Our License Agreement for Free and Trial-Basis items is posted on the web.
  Please <A HREF="http://www.href.com/pub/freelicn.html">Read It Now</A>.
  If you agree to be bound to all its terms you may use our software,
  otherwise you may not.  Expiration dates do apply to the usage of some
  files.
}

{ ----------------------------------------------------------------------- }

{ This project demonstrates how to avoid using application.run;
  Application.run has an unnecessary side effect of showing the form,
  even if it is set to invisible.

  This workaround provides all the functionality of application.run
  while letting you control the visible state.

  You may want to use this technique in the context of web-applications
  where you don't want the form to display on the server screen at all.

}


uses
  Forms,
  Windows,
  SysUtils,
  Htinvsf in 'HTINVSF.PAS' {Form1};

{$R *.RES}

procedure DoneApplication; far;
begin
  with Application do begin
    if Handle<>0 then
      ShowOwnedPopups(Handle,False);
    Destroying;
    DestroyComponents;
    end;
end;

procedure Run;
begin
  AddExitProc(DoneApplication);

(*
  with Application do
    MainForm.Visible:=True;    {your choice as to whether this is turned on}
*)

  with Application do
    repeat
      HandleMessage
    until Terminated;
end;

begin
  Application.CreateForm(TForm1, Form1);
  run;
end.
